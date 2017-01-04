{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}


module Hearth.Prompt where


--------------------------------------------------------------------------------


import Data.NonEmpty
import GHC.Exts (Constraint)
import Hearth.Action
import Hearth.DebugEvent
import Hearth.GameEvent
import Hearth.Model


--------------------------------------------------------------------------------


data HearthError
    = InvalidMulligan
    | InvalidShuffle
    | InvalidHandCard
    | InvalidWeapon
    | InvalidMinion
    | InvalidPlayer
    | InvalidCharacter
    | InvalidElect
    deriving (Show, Eq, Ord)


data family PickResult :: Selection -> * -> *


data instance PickResult AtRandom a
    = AtRandomPick a
    deriving (Show, Eq, Ord)


data instance PickResult Targeted a
    = TargetedPick a
    | AbortTargetedPick
    deriving (Show, Eq, Ord)


data HearthPrompt :: (* -> Constraint) -> * -> * where
    PromptDebugEvent :: DebugEvent -> HearthPrompt k ()
    PromptError :: HearthError -> HearthPrompt k ()
    PromptGameEvent :: GameSnapshot k -> GameEvent k -> HearthPrompt k ()
    PromptAction :: GameSnapshot k -> HearthPrompt k (Action k)
    PromptShuffle :: [a] -> HearthPrompt k [a]
    PromptMulligan :: Handle Player -> [HandCard k] -> HearthPrompt k [HandCard k]
    PromptPickAtRandom :: GameSnapshot k -> PromptPick AtRandom k a -> HearthPrompt k (PickResult AtRandom a)
    PromptPickTargeted :: GameSnapshot k -> PromptPick Targeted k a -> HearthPrompt k (PickResult Targeted a)


data PromptPick :: Selection -> (* -> Constraint) -> * -> * where
    PickHandCard :: NonEmpty (HandCard k) -> PromptPick s k (HandCard k)
    PickWeapon :: NonEmpty (Handle Weapon) -> PromptPick s k (Handle Weapon)
    PickMinion :: NonEmpty (Handle Minion) -> PromptPick s k (Handle Minion)
    PickPlayer :: NonEmpty (Handle Player) -> PromptPick s k (Handle Player)
    PickCharacter :: NonEmpty (Handle Character) -> PromptPick s k (Handle Character)
    PickElect :: NonEmpty (Elect k s) -> PromptPick s k (Elect k s)




