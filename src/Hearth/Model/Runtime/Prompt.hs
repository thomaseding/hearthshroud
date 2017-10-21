{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}


module Hearth.Model.Runtime.Prompt where


--------------------------------------------------------------------------------


import Data.NonEmpty
import Hearth.Model.Authoring
import Hearth.Model.Runtime
import Hearth.Model.Runtime.Action
import Hearth.Model.Runtime.DebugEvent
import Hearth.Model.Runtime.GameEvent


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


data instance PickResult 'AtRandom' a
    = AtRandomPick a
    deriving (Show, Eq, Ord)


data instance PickResult 'Targeted' a
    = TargetedPick a
    | AbortTargetedPick
    deriving (Show, Eq, Ord)


data HearthPrompt :: * -> * where
    PromptDebugEvent :: DebugEvent -> HearthPrompt ()
    PromptError :: HearthError -> HearthPrompt ()
    PromptGameEvent :: GameSnapshot -> GameEvent -> HearthPrompt ()
    PromptAction :: GameSnapshot -> HearthPrompt Action
    PromptShuffle :: [a] -> HearthPrompt [a]
    PromptMulligan :: Handle 'Player' -> [HandCard] -> HearthPrompt [HandCard]
    PromptPickAtRandom :: GameSnapshot -> PromptPick 'AtRandom' a -> HearthPrompt (PickResult 'AtRandom' a)
    PromptPickTargeted :: GameSnapshot -> PromptPick 'Targeted' a -> HearthPrompt (PickResult 'Targeted' a)


data PromptPick :: Selection -> * -> * where
    PickHandCard :: NonEmpty HandCard -> PromptPick s HandCard
    PickWeapon :: NonEmpty (Handle 'Weapon') -> PromptPick s (Handle 'Weapon')
    PickMinion :: NonEmpty (Handle 'Minion') -> PromptPick s (Handle 'Minion')
    PickPlayer :: NonEmpty (Handle 'Player') -> PromptPick s (Handle 'Player')
    PickCharacter :: NonEmpty (Handle 'Character') -> PromptPick s (Handle 'Character')
    PickElect :: NonEmpty (Elect s) -> PromptPick s (Elect s)




