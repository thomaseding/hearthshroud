{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Hearth.Client.Console (
    main
) where


--------------------------------------------------------------------------------


import Control.Applicative
import Control.Error
import Control.Exception
import Control.Lens
import Control.Lens.Helper
import Control.Lens.Internal.Zoom (Zoomed, Focusing)
import Control.Monad.Loops
import Control.Monad.Prompt
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.State.Local
import Data.Char
import Data.Data
import Data.Generics.Uniplate.Data
import Data.List
import Data.NonEmpty
import Hearth.Action
import Hearth.Cards
import Hearth.Client.Console.HandColumn
import Hearth.DebugEvent
import Hearth.Engine
import Hearth.GameEvent
import Hearth.Model
import Hearth.Names
import Hearth.Prompt
import Language.Haskell.TH.Syntax (nameBase)
import System.Console.ANSI


--------------------------------------------------------------------------------


data LogState = LogState {
    _callDepth :: Int,
    _useShortTag :: Bool,
    _quiet :: Bool
} deriving (Show, Eq, Ord)
makeLenses ''LogState


data ConsoleState = ConsoleState {
    _logState :: LogState
} deriving (Show, Eq, Ord)
makeLenses ''ConsoleState


newtype Console' st a = Console {
    unConsole :: StateT st IO a
} deriving (Functor, Applicative, Monad, MonadIO, MonadState st)


instance MonadReader st (Console' st) where
    ask = get
    local = stateLocal


type Console = Console' ConsoleState


type instance Zoomed (Console' st) = Focusing IO


instance Zoom (Console' st) (Console' st') st st' where
    zoom l = Console . zoom l . unConsole


unlessM :: (Monad m) => m Bool -> m () -> m ()
unlessM c m = do
    c >>= \case
        False -> m
        True -> return ()


enableQuiet :: ConsoleState -> ConsoleState
enableQuiet = set (logState.quiet) True


isQuiet :: Console Bool
isQuiet = view $ logState.quiet


logIndentation :: Console String
logIndentation = do
    n <- view $ logState.callDepth
    return $ concat $ replicate n "    "


debugEvent :: DebugEvent -> Console ()
debugEvent e = unlessM isQuiet $ case e of
    FunctionEntered name -> do
        logState.useShortTag >>=. \case
            True -> liftIO $ putStrLn ">"
            False -> return ()
        lead <- logIndentation
        logState.callDepth += 1
        logState.useShortTag .= True
        liftIO $ putStr $ lead ++ "<:" ++ (showName name)
    FunctionExited name -> do
        logState.callDepth -= 1
        logState.useShortTag >>=. \case
            True -> liftIO $ putStrLn "/>"
            False -> do
                lead <- logIndentation
                liftIO $ putStrLn $ lead ++ "</:" ++ (showName name) ++ ">"
        logState.useShortTag .= False
    where
        showName = nameBase


gameEvent :: GameEvent -> Console ()
gameEvent e = unlessM isQuiet $ do
    logState.useShortTag >>=. \case
        True -> liftIO $ putStrLn "/>"
        False -> return ()
    logState.useShortTag .= False
    txt <- return $ case e of
        CardDrawn (PlayerHandle who) (mCard) (Deck deck) -> let
            cardAttr = case mCard of
                Nothing -> ""
                Just card -> " card=" ++ quote (simpleName card)
            in "<cardDrawn"
                ++ " handle=" ++ quote who
                ++ cardAttr
                ++ " deck=" ++ quote (length deck)
                ++ " />"
        PlayedCard (PlayerHandle who) card result -> case result of
            Failure -> ""
            Success -> "<playedCard"
                ++ " handle=" ++ quote who
                ++ " card=" ++ quote (simpleName card)
                ++ " />"
        HeroTakesDamage (PlayerHandle who) (Health oldHealth) (Damage damage) -> let
            newHealth = oldHealth - damage
            in "<heroTakesDamage"
                ++ " handle=" ++ quote who
                ++ " old=" ++ quote oldHealth
                ++ " new=" ++ quote newHealth
                ++ " dmg=" ++ quote damage
                ++ " />"
        GainsManaCrystal (PlayerHandle who) mCrystalState -> let
            stateAttr = case mCrystalState of
                Nothing -> "none"
                Just CrystalFull -> "full"
                Just CrystalEmpty -> "empty"
            in "<gainsManaCrystal"
                ++ " handle=" ++ quote who
                ++ " crystal=" ++ show stateAttr
                ++ " />"
        ManaCrystalsRefill (PlayerHandle who) amount -> let
            in "<manaCrystalsRefill"
                ++ " handle=" ++ quote who
                ++ " amount=" ++ quote amount
                ++ "/>"
        ManaCrystalsEmpty (PlayerHandle who) amount -> let
            in "<manaCrystalsEmpty"
                ++ " handle=" ++ quote who
                ++ " amount=" ++ quote amount
                ++ "/>"
    case null txt of
        True -> return ()
        False -> do
            lead <- logIndentation
            liftIO $ putStrLn $ lead ++ txt
    where
        quote = show . show


simpleName :: (Data a) => a -> BasicCardName
simpleName card = case universeBi card of
    [name] -> name
    _ -> $logicError 'simpleName


instance MonadPrompt HearthPrompt Console where
    prompt = \case
        PromptDebugEvent e -> debugEvent e
        PromptGameEvent e -> gameEvent e
        PromptAction snapshot -> getAction snapshot
        PromptShuffle xs -> return xs
        PromptPickRandom (NonEmpty x _) -> return x
        PromptMulligan _ xs -> return xs


getAction :: GameSnapshot -> Console Action
getAction snapshot = local enableQuiet $ runQuery snapshot $ do
    showPlayers
    handle <- getActivePlayerHandle
    cards <- view $ getPlayer handle.playerHand.handCards
    mCard <- flip firstM cards $ \card -> playCard handle card >>= \case
        Failure -> return False
        Success -> return True
    case mCard of
        Nothing -> return ActionEndTurn
        Just card -> return $ ActionPlayCard card


main :: IO ()
main = do
    result <- finally runTestGame $ do
        clearScreen
        setCursorPosition 0 0
    print result


runTestGame :: IO GameResult
runTestGame = flip evalStateT st $ unConsole $ runHearth (player1, player2)
    where
        st = ConsoleState {
            _logState = LogState {
                _callDepth = 0,
                _useShortTag = False,
                _quiet = True } }
        power = HeroPower {
            _heroPowerCost = ManaCost 0,
            _heroPowerEffects = [] }
        hero name = Hero {
            _heroAttack = 0,
            _heroHealth = 30,
            _heroPower = power,
            _heroName = BasicHeroName name }
        deck1 = Deck $ take 30 $ cycle cardUniverse
        deck2 = Deck $ take 30 $ cycle $ reverse cardUniverse
        player1 = PlayerData (hero Thrall) deck1
        player2 = PlayerData (hero Rexxar) deck2


data Who = Alice | Bob
    deriving (Show, Eq, Ord)


showPlayers :: Hearth Console ()
showPlayers = do
    liftIO clearScreen
    ps <- mapM (view . getPlayer) =<< getPlayerHandles
    liftIO $ do
        forM_ (zip ps [Alice, Bob]) $ \(p, who) -> do
            printPlayer who p
        setCursorPosition 0 0
        getLine >> return ()


printPlayer :: Who -> Player -> IO ()
printPlayer who p = do
    let deck = showDeck $ p^.playerDeck
        hand = handColumn $ p^.playerHand
        minions = map showMinion $ p^.playerMinions
        (deckLoc, handLoc, minionsLoc) = let
            (x, y, z) = (0, 25, 50)
            width = 140
            in case who of
                Alice -> (x, y, z)
                Bob -> (width - x, width - y, width - z)
    printColumn (map toUpper $ show who) deckLoc [deck]
    printColumn "HAND" handLoc $ hand
    printColumn "MINIONS" minionsLoc minions



showDeck :: Deck -> String
showDeck (Deck cs) = "Deck:" ++ show (length cs)

printColumn :: String -> Int -> [String] -> IO ()
printColumn label column = zipWithM_ f [0..] . ([label, replicate (length label) '-', ""] ++ )
    where
        f row str = do
            setCursorPosition row column
            putStr str


showMinion :: BoardMinion -> String
showMinion m = show $ simpleName $ m^.boardMinion.minionName




