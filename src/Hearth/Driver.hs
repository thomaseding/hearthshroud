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


module Hearth.Driver where


--------------------------------------------------------------------------------


import Control.Applicative
import Control.Error
import Control.Lens
import Control.Lens.Helper
import Control.Lens.Internal.Zoom (Zoomed, Focusing)
import Control.Monad.LessIO
import Control.Monad.Prompt
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.State.Local
import Data.Generics.Uniplate.Data
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
    _useShortTag :: Bool
} deriving (Show, Eq, Ord)
makeLenses ''LogState


data DriverState = DriverState {
    _logState :: LogState
} deriving (Show, Eq, Ord)
makeLenses ''DriverState


newtype Driver' st a = Driver {
    unDriver :: StateT st IO a
} deriving (Functor, Applicative, Monad, MonadIO, MonadState st)


instance MonadReader st (Driver' st) where
    ask = get
    local = stateLocal


type Driver = Driver' DriverState


type instance Zoomed (Driver' st) = Focusing IO


instance Zoom (Driver' st) (Driver' st') st st' where
    zoom l = Driver . zoom l . unDriver


logIndentation :: Driver String
logIndentation = do
    n <- view $ logState.callDepth
    return $ concat $ replicate n "    "


debugEvent :: DebugEvent -> Driver ()
debugEvent = \case
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


gameEvent :: GameEvent -> Driver ()
gameEvent e = do
    logState.useShortTag >>=. \case
        True -> liftIO $ putStrLn "/>"
        False -> return ()
    logState.useShortTag .= False
    txt <- return $ case e of
        CardDrawn (PlayerHandle who) (mCard) (Deck deck) -> let
            cardAttr = case mCard of
                Nothing -> ""
                Just card -> " card=" ++ quote (cardName card)
            in "<cardDrawn"
                ++ " handle=" ++ quote who
                ++ cardAttr
                ++ " deck=" ++ quote (length deck)
                ++ " />"
        HeroTakesDamage (PlayerHandle who) (Health oldHealth) (Damage damage) -> let
            newHealth = oldHealth - damage
            in "<heroTakesDamage"
                ++ " handle=" ++ quote who
                ++ " old=" ++ quote oldHealth
                ++ " new=" ++ quote newHealth
                ++ " dmg=" ++ quote damage
                ++ " />"
    lead <- logIndentation
    liftIO $ putStrLn $ lead ++ txt
    where
        quote = show . show


cardName :: HandCard -> BasicCardName
cardName card = case universeBi card of
    [name] -> name
    _ -> $logicError 'cardName


instance MonadPrompt HearthPrompt Driver where
    prompt = \case
        PromptDebugEvent e -> debugEvent e
        PromptGameEvent e -> gameEvent e
        PromptAction -> return ActionEndTurn
        PromptShuffle xs -> return xs
        PromptPickRandom (NonEmpty x _) -> return x
        PromptMulligan _ xs -> return xs


runDriver :: IO GameResult
runDriver = less $ flip evalStateT st $ unDriver $ runHearth (player1, player2)
    where
        st = DriverState {
            _logState = LogState {
                _callDepth = 0,
                _useShortTag = False } }
        power = HeroPower {
            _heroPowerCost = 0,
            _heroPowerEffects = [] }
        hero = Hero {
            _heroAttack = 0,
            _heroHealth = 30,
            _heroPower = power,
            _heroName = BasicHeroName Thrall }
        deck1 = Deck $ take 30 $ cycle cards
        deck2 = Deck $ take 30 $ cycle $ reverse cards
        player1 = PlayerData hero deck1
        player2 = changeTo Rexxar $ changeTo deck2 player1
        changeTo = transformBi . const


cards :: [DeckCard]
cards = [
    bloodfenRaptor,
    boulderfistOgre,
    chillwindYeti,
    coreHound,
    magmaRager,
    murlocRaider,
    oasisSnapjaw,
    riverCrocolisk,
    warGolem ]


mkVanilla :: BasicCardName -> Cost -> Attack -> Health -> DeckCard
mkVanilla name _ attack health = DeckCardMinion $ DeckMinion minion
    where
        minion = Minion {
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






