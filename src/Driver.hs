{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}


module Driver where


--------------------------------------------------------------------------------


import Control.Applicative
import Control.Lens
import Control.Monad.Prompt
import Control.Monad.State
import Control.Monad.Trans
import Engine
import Names


--------------------------------------------------------------------------------


data DriverState = DriverState {
    _callDepth :: Int
} deriving (Show, Eq, Ord)
makeLenses ''DriverState


newtype Driver a = Driver {
    unDriver :: StateT DriverState IO a
} deriving (Functor, Applicative, Monad, MonadIO, MonadState DriverState)


logEvent :: LogEvent -> Driver ()
logEvent = \case
    LogFunctionEntered name -> do
        tabby
        callDepth += 1
        liftIO $ putStrLn $ "<" ++ name ++ ">"
    LogFunctionExited name -> do
        callDepth -= 1
        tabby
        liftIO $ putStrLn $ "</" ++ name ++ ">"
    where
        tabby = do
            n <- gets $ view callDepth
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
        st = DriverState { _callDepth = 0 }
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






