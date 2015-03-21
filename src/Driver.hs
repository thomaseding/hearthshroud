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
import Control.Monad.Prompt
import Control.Monad.Trans
import Engine
import Names


--------------------------------------------------------------------------------


newtype Driver a = Driver {
    unDriver :: IO a
} deriving (Functor, Applicative, Monad, MonadIO)


instance MonadPrompt HearthPrompt Driver where
    prompt = return . \case
        PromptShuffle x -> x
        PromptPickRandom (NonEmpty x _) -> x
        PromptMulligan _ -> []


runDriver :: IO GameResult
runDriver = unDriver $ runHearth (NonEmpty player [player])
    where
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


