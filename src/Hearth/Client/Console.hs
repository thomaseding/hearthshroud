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


module Hearth.Client.Console where


--------------------------------------------------------------------------------


import Control.Applicative
import Control.Error
import Control.Lens
import Control.Lens.Helper
import Control.Lens.Internal.Zoom (Zoomed, Focusing)
import Control.Monad.Loops
import Control.Monad.Prompt
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.State.Local
import Data.Data
import Data.Generics.Uniplate.Data
import Data.List
import Data.NonEmpty
import Hearth.Action
import Hearth.DebugEvent
import Hearth.Engine
import Hearth.GameEvent
import Hearth.Model
import Hearth.Names
import Hearth.Prompt
import Language.Haskell.TH.Syntax (nameBase)


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


runTestGame :: IO GameResult
runTestGame = flip evalStateT st $ unConsole $ runHearth (player1, player2)
    where
        st = ConsoleState {
            _logState = LogState {
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


showPlayers :: Hearth Console ()
showPlayers = do
    ps <- mapM (view . getPlayer) =<< getPlayerHandles
    liftIO $ do
        forM_ ps $ \p -> do
            putStrLn ""
            putStrLn $ showPlayer p
        getLine >> return ()


showPlayer :: Player -> String
showPlayer p = let
    deck = showDeck $ p^.playerDeck
    hand = showHand $ p^.playerHand
    minions = "Board:" ++ joinList (map showMinion $ p^.playerMinions)
    in unlines [deck, hand, minions]


joinList :: [String] -> String
joinList ss = "[" ++ intercalate "," ss ++ "]"


showDeck :: Deck -> String
showDeck (Deck cs) = "Deck:" ++ show (length cs)


showHand :: Hand -> String
showHand (Hand cs) = "Hand:" ++ joinList (reverse $ map (show . simpleName) cs)


showMinion :: BoardMinion -> String
showMinion m = show $ simpleName $ m^.boardMinion.minionName


cardUniverse :: [DeckCard]
cardUniverse = [
    bloodfenRaptor,
    boulderfistOgre,
    chillwindYeti,
    coreHound,
    magmaRager,
    murlocRaider,
    oasisSnapjaw,
    riverCrocolisk,
    warGolem ]


mkVanilla :: BasicCardName -> Mana -> Attack -> Health -> DeckCard
mkVanilla name mana attack health = DeckCardMinion $ DeckMinion minion
    where
        minion = Minion {
            _minionCost = ManaCost mana,
            _minionAttack = attack,
            _minionHealth = health,
            _minionName = BasicCardName name }


bloodfenRaptor :: DeckCard
bloodfenRaptor = mkVanilla BloodfenRaptor 2 3 2


boulderfistOgre :: DeckCard
boulderfistOgre = mkVanilla BoulderfistOgre 6 6 7


chillwindYeti :: DeckCard
chillwindYeti = mkVanilla MurlocRaider 1 2 1


coreHound :: DeckCard
coreHound = mkVanilla CoreHound 7 9 5


magmaRager :: DeckCard
magmaRager = mkVanilla MagmaRager 3 5 1


murlocRaider :: DeckCard
murlocRaider = mkVanilla MurlocRaider 1 2 1


oasisSnapjaw :: DeckCard
oasisSnapjaw = mkVanilla OasisSnapjaw 4 2 7


riverCrocolisk :: DeckCard
riverCrocolisk = mkVanilla RiverCrocolisk 2 2 3


warGolem :: DeckCard
warGolem = mkVanilla WarGolem 7 7 7






