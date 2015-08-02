{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
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


data family PickResult s a


data instance PickResult AtRandom a
    = RandomPick a


data instance PickResult Targeted a
    = TargetPick a
    | AbortTargetPick


data HearthPrompt :: * -> * where
    PromptDebugEvent :: DebugEvent -> HearthPrompt ()
    PromptError :: HearthError -> HearthPrompt ()
    PromptGameEvent :: GameEvent -> HearthPrompt ()
    PromptAction :: GameSnapshot -> HearthPrompt Action
    PromptShuffle :: [a] -> HearthPrompt [a]
    PromptMulligan :: PlayerHandle -> [HandCard] -> HearthPrompt [HandCard]
    PromptPickAtRandom :: PromptPick AtRandom a -> HearthPrompt (PickResult AtRandom a)
    PromptPickTargeted :: PromptPick Targeted a -> HearthPrompt (PickResult Targeted a)
deriving instance (Show a) => Show (HearthPrompt a)


data PromptPick :: * -> * -> * where
    PickMinion :: GameSnapshot -> NonEmpty MinionHandle -> PromptPick s MinionHandle
    PickPlayer :: GameSnapshot -> NonEmpty PlayerHandle -> PromptPick s PlayerHandle
    PickCharacter :: GameSnapshot -> NonEmpty CharacterHandle -> PromptPick s CharacterHandle
deriving instance Show (PromptPick s a)




