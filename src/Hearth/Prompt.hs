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


class (Functor (PickResult s)) => IsPickResult s where
    data PickResult s a


instance IsPickResult AtRandom where
    data PickResult AtRandom a = AtRandomPick a
        deriving (Show, Eq, Ord)


instance Functor (PickResult AtRandom) where
    fmap f (AtRandomPick x) = AtRandomPick (f x)


instance IsPickResult Targeted where
    data PickResult Targeted a = TargetedPick a | AbortTargetedPick
        deriving (Show, Eq, Ord)


instance Functor (PickResult Targeted) where
    fmap f = \case
        TargetedPick x -> TargetedPick (f x)
        AbortTargetedPick -> AbortTargetedPick


data HearthPrompt :: * -> * where
    PromptDebugEvent :: DebugEvent -> HearthPrompt ()
    PromptError :: HearthError -> HearthPrompt ()
    PromptGameEvent :: GameSnapshot -> GameEvent -> HearthPrompt ()
    PromptAction :: GameSnapshot -> HearthPrompt Action
    PromptShuffle :: [a] -> HearthPrompt [a]
    PromptMulligan :: Handle Player -> [HandCard] -> HearthPrompt [HandCard]
    PromptPickAtRandom :: PromptPick AtRandom a -> HearthPrompt (PickResult AtRandom a)
    PromptPickTargeted :: PromptPick Targeted a -> HearthPrompt (PickResult Targeted a)


data PromptPick :: * -> * -> * where
    PickMinion :: GameSnapshot -> NonEmpty (Handle Minion) -> PromptPick s (Handle Minion)
    PickPlayer :: GameSnapshot -> NonEmpty (Handle Player) -> PromptPick s (Handle Player)
    PickCharacter :: GameSnapshot -> NonEmpty (Handle Character) -> PromptPick s (Handle Character)




