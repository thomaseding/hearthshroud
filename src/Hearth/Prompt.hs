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
import Hearth.Action
import Hearth.DebugEvent
import Hearth.GameEvent
import Hearth.Model


--------------------------------------------------------------------------------


data HearthError
    = InvalidMulligan
    | InvalidShuffle
    | InvalidMinion
    | InvalidPlayer
    | InvalidCharacter
    deriving (Show, Eq, Ord)


data family PickResult :: Selection -> * -> *


data instance PickResult AtRandom a
    = AtRandomPick a
    deriving (Show, Eq, Ord)


data instance PickResult Targeted a
    = TargetedPick a
    | AbortTargetedPick
    deriving (Show, Eq, Ord)


data HearthPrompt :: * -> * where
    PromptDebugEvent :: DebugEvent -> HearthPrompt ()
    PromptError :: HearthError -> HearthPrompt ()
    PromptGameEvent :: GameSnapshot -> GameEvent -> HearthPrompt ()
    PromptAction :: GameSnapshot -> HearthPrompt Action
    PromptShuffle :: [a] -> HearthPrompt [a]
    PromptMulligan :: Handle Player -> [HandCard] -> HearthPrompt [HandCard]
    PromptPickAtRandom :: PromptPick AtRandom a -> HearthPrompt (PickResult AtRandom a)
    PromptPickTargeted :: PromptPick Targeted a -> HearthPrompt (PickResult Targeted a)


data PromptPick :: Selection -> * -> * where
    PickMinion :: GameSnapshot -> NonEmpty (Handle Minion) -> PromptPick s (Handle Minion)
    PickPlayer :: GameSnapshot -> NonEmpty (Handle Player) -> PromptPick s (Handle Player)
    PickCharacter :: GameSnapshot -> NonEmpty (Handle Character) -> PromptPick s (Handle Character)




