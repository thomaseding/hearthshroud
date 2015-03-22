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


module Driver where


--------------------------------------------------------------------------------


import Control.Applicative
import Control.Lens
import Control.Lens.Internal.Zoom (Zoomed, Focusing)
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


newtype Driver' st a = Driver {
    unDriver :: StateT st IO a
} deriving (Functor, Applicative, Monad, MonadIO, MonadState st)


type Driver = Driver' DriverState


type instance Zoomed (Driver' st) = Focusing IO


instance Zoom (Driver' st) (Driver' st') st st' where
    zoom lens = Driver . zoom lens . unDriver


logEvent :: LogEvent -> Driver ()
logEvent e = zoom logState $ case e of
    LogFunctionEntered name -> do
        useShortTag >>=. \case
            True -> liftIO $ putStrLn ">"
            False -> return ()
        tabby
        callDepth += 1
        useShortTag .= True
        liftIO $ putStr $ "<" ++ name
    LogFunctionExited name -> do
        callDepth -= 1
        useShortTag >>=. \case
            True -> do
                liftIO $ putStrLn "/>"
            False -> do
                tabby
                liftIO $ putStrLn $ "</" ++ name ++ ">"
        useShortTag .= False
    where
        tabby = do
            n <- use callDepth
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






