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


data LogState = LogState {
    _callDepth :: Int,
    _useShortTag :: Bool
} deriving (Show, Eq, Ord)
makeLenses ''LogState


data DriverState = DriverState {
    _logState :: LogState
} deriving (Show, Eq, Ord)
makeLenses ''DriverState


newtype Driver a = Driver {
    unDriver :: StateT DriverState IO a
} deriving (Functor, Applicative, Monad, MonadIO, MonadState DriverState)


logEvent :: LogEvent -> Driver ()
logEvent = \case
    LogFunctionEntered name -> do
        gets (view $ logState.useShortTag) >>= \case
            True -> liftIO $ putStrLn ">"
            False -> return ()
        tabby
        logState.callDepth += 1
        logState.useShortTag .= True
        liftIO $ putStr $ "<" ++ name
    LogFunctionExited name -> do
        logState.callDepth -= 1
        gets (view $ logState.useShortTag) >>= \case
            True -> do
                liftIO $ putStrLn "/>"
            False -> do
                tabby
                liftIO $ putStrLn $ "</" ++ name ++ ">"
        logState.useShortTag .= False
    where
        tabby = do
            n <- gets $ view $ logState.callDepth
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






