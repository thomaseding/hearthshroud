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
import GHC.Prim (Constraint)
import Hearth.Action
import Hearth.DebugEvent
import Hearth.GameEvent
import Hearth.Model


--------------------------------------------------------------------------------


data HearthError
    = InvalidMulligan
    | InvalidShuffle
    | InvalidHandCard
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
    PromptDebugEvent :: DebugEvent -> HearthPrompt c ()
    PromptError :: HearthError -> HearthPrompt c ()
    PromptGameEvent :: GameSnapshot c -> GameEvent c -> HearthPrompt c ()
    PromptAction :: GameSnapshot c -> HearthPrompt c (Action c)
    PromptShuffle :: [a] -> HearthPrompt c [a]
    PromptMulligan :: Handle Player -> [HandCard c] -> HearthPrompt c [HandCard c]
    PromptPickAtRandom :: GameSnapshot c -> PromptPick AtRandom c a -> HearthPrompt c (PickResult AtRandom a)
    PromptPickTargeted :: GameSnapshot c -> PromptPick Targeted c a -> HearthPrompt c (PickResult Targeted a)


data PromptPick :: Selection -> (* -> Constraint) -> * -> * where
    PickHandCard :: NonEmpty (HandCard c) -> PromptPick s c (HandCard c)
    PickMinion :: NonEmpty (Handle Minion) -> PromptPick s c (Handle Minion)
    PickPlayer :: NonEmpty (Handle Player) -> PromptPick s c (Handle Player)
    PickCharacter :: NonEmpty (Handle Character) -> PromptPick s c (Handle Character)
    PickElect :: NonEmpty (Elect c s) -> PromptPick s c (Elect c s)




