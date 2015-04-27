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
import Hearth.Client.Console.BoardMinionsColumn
import Hearth.Client.Console.HandColumn
import Hearth.Client.Console.PlayerColumn
import Hearth.DebugEvent
import Hearth.Engine
import Hearth.GameEvent
import Hearth.Model
import Hearth.Names
import Hearth.Prompt
import Language.Haskell.TH.Syntax (nameBase)
import System.Console.ANSI


--------------------------------------------------------------------------------


screenWidth :: Int
screenWidth = 150


data LogState = LogState {
    _loggedLines :: [String],
    _totalLines :: !Int,
    _undisplayedLines :: !Int,
    _callDepth :: !Int,
    _useShortTag :: !Bool,
    _quiet :: !Bool
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


isQuiet :: Console Bool
isQuiet = view $ logState.quiet


localQuiet :: Console a -> Console a
localQuiet m = do
    q <- view $ logState.quiet
    logState.quiet .= True
    x <- m
    logState.quiet .= q
    return x


newLogLine :: Console ()
newLogLine = zoom logState $ do
    totalLines += 1
    undisplayedLines += 1
    loggedLines %= ("" :)


appendLogLine :: String -> Console ()
appendLogLine str = logState.loggedLines %= \case
    s : ss -> (s ++ str) : ss
    [] -> $logicError 'appendLogLine


logIndentation :: Console String
logIndentation = do
    n <- view $ logState.callDepth
    return $ concat $ replicate n "    "


debugEvent :: DebugEvent -> Console ()
debugEvent e = unlessM isQuiet $ case e of
    FunctionEntered name -> do
        logState.useShortTag >>=. \case
            True -> do
                appendLogLine ">"
                newLogLine
            False -> return ()
        lead <- logIndentation
        logState.callDepth += 1
        logState.useShortTag .= True
        appendLogLine $ lead ++ "<:" ++ (showName name)
    FunctionExited name -> do
        logState.callDepth -= 1
        logState.useShortTag >>=. \case
            True -> do
                appendLogLine "/>"
                newLogLine
            False -> do
                lead <- logIndentation
                appendLogLine $ lead ++ "</:" ++ (showName name) ++ ">"
                newLogLine
        logState.useShortTag .= False
    where
        showName = nameBase


gameEvent :: GameEvent -> Console ()
gameEvent e = unlessM isQuiet $ do
    logState.useShortTag >>=. \case
        True -> do
            appendLogLine "/>"
            newLogLine
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
            appendLogLine $ lead ++ txt
            newLogLine
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
getAction snapshot = localQuiet $ runQuery snapshot $ do
    refreshDisplay
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
        setSGR [SetColor Foreground Dull White]
        clearScreen
        setCursorPosition 0 0
    print result


runTestGame :: IO GameResult
runTestGame = flip evalStateT st $ unConsole $ runHearth (player1, player2)
    where
        st = ConsoleState {
            _logState = LogState {
                _loggedLines = [""],
                _totalLines = 1,
                _undisplayedLines = 1,
                _callDepth = 0,
                _useShortTag = False,
                _quiet = False } }
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


refreshDisplay :: Hearth Console ()
refreshDisplay = do
    ps <- mapM (view . getPlayer) =<< getPlayerHandles
    deepestPlayer <- liftIO $ do
        clearScreen
        setSGR [SetColor Foreground Dull White]
        foldM (\n -> liftM (max n) . uncurry printPlayer) 0 (zip ps [Alice, Bob])
    lift $ do
        refreshLogWindow $ deepestPlayer + 1

        presentPrompt


presentPrompt :: Console ()
presentPrompt = liftIO $ do
    setSGR [SetColor Foreground Dull White]
    putStrLn ""
    putStr "> "
    _ <- getLine
    return ()


refreshLogWindow :: Int -> Console ()
refreshLogWindow row = do
    let displayCount = 106 - row
    totalCount <- view $ logState.totalLines
    let lineNoLen = length $ show totalCount
        pad str = replicate (lineNoLen - length str) '0' ++ str
        putWithLineNo (lineNo, str) = do
            when (totalCount < displayCount && null str) $ do
                setSGR [SetColor Foreground Vivid Blue]
                return ()
            putStrLn $ pad (show (lineNo::Int)) ++ " " ++ str
    freshCount <- view $ logState.undisplayedLines
    logState.undisplayedLines .= 0
    (freshLines, oldLines) <- view $ logState.loggedLines.to (id
        . splitAt (if totalCount < displayCount then min displayCount freshCount + displayCount - totalCount else freshCount)
        . zip (iterate pred $ max displayCount totalCount)
        . (replicate (displayCount - totalCount) "" ++)
        . take displayCount)
    liftIO $ do
        setSGR [SetColor Foreground Dull Cyan]
        setCursorPosition row 0
        putStrLn $ replicate screenWidth '-'
        putStrLn ""
        mapM_ putWithLineNo $ reverse oldLines
        setSGR [SetColor Foreground Dull White]
        mapM_ putWithLineNo $ reverse freshLines


printPlayer :: Player -> Who -> IO Int
printPlayer p who = do
    setSGR [SetColor Foreground Vivid Green]
    let playerName = map toUpper $ show who
        player = playerColumn p
        hand = handColumn $ p^.playerHand
        boardMinions = boardMinionsColumn $ p^.playerMinions
        (deckLoc, handLoc, minionsLoc) = let
            (x, y, z) = (0, 25, 50)
            width = screenWidth - 10
            in case who of
                Alice -> (x, y, z)
                Bob -> (width - x, width - y, width - z)
    n0 <- printColumn playerName deckLoc player
    n1 <- printColumn "HAND" handLoc hand
    n2 <- printColumn "MINIONS" minionsLoc boardMinions
    return $ maximum [n0, n1, n2]


printColumn :: String -> Int -> [String] -> IO Int
printColumn label column strs = do
    let strs' = [label, replicate (length label) '-', ""] ++ strs
    zipWithM_ f [0..] strs'
    return $ length strs'
    where
        f row str = do
            setCursorPosition row column
            putStr str




















