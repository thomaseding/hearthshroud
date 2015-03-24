{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Hearth.Driver where


--------------------------------------------------------------------------------


import Control.Applicative
import Control.Lens
import Control.Lens.Helper
import Control.Lens.Internal.Zoom (Zoomed, Focusing)
import Control.Monad.Prompt
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.State.Local
import Control.Monad.Trans
import Data.NonEmpty
import Hearth.Engine
import Hearth.LogEvent
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
    zoom lens = Driver . zoom lens . unDriver


logEvent :: LogEvent -> Driver ()
logEvent = zoom logState . \case
    LogFunctionEntered name -> do
        useShortTag >>=. \case
            True -> liftIO $ putStrLn ">"
            False -> return ()
        tabby
        callDepth += 1
        useShortTag .= True
        liftIO $ putStr $ "<" ++ (showName name)
    LogFunctionExited name -> do
        callDepth -= 1
        useShortTag >>=. \case
            True -> do
                liftIO $ putStrLn "/>"
            False -> do
                tabby
                liftIO $ putStrLn $ "</" ++ (showName name) ++ ">"
        useShortTag .= False
    where
        showName = nameBase
        tabby = do
            n <- view callDepth
            liftIO $ putStr $ concat $ replicate n "    "


instance MonadPrompt HearthPrompt Driver where
    prompt = \case
        PromptLogEvent e -> logEvent e
        PromptShuffle x -> return x
        PromptPickRandom (NonEmpty x _) -> return x
        PromptMulligan _ -> return []


runDriver :: IO GameResult
runDriver = flip evalStateT st $ unDriver $ runHearth (NonEmpty player [player])
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
        deck = Deck []
        player = PlayerData hero deck






