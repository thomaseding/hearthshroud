{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}


module Hearth.Prompt where


--------------------------------------------------------------------------------


import Data.NonEmpty
import Hearth.Action
import Hearth.DebugEvent
import Hearth.GameEvent
import Hearth.Model


--------------------------------------------------------------------------------


data Selection = Targeted' | AtRandom
    deriving (Show, Eq, Ord)


data HearthPrompt :: * -> * where
    PromptDebugEvent :: DebugEvent -> HearthPrompt ()
    PromptGameEvent :: GameEvent -> HearthPrompt ()
    PromptAction :: GameSnapshot -> HearthPrompt Action
    PromptShuffle :: [a] -> HearthPrompt [a]
    PromptMulligan :: PlayerHandle -> [HandCard] -> HearthPrompt [HandCard]
    PromptPickMinion :: GameSnapshot -> Selection -> NonEmpty MinionHandle -> HearthPrompt MinionHandle
    PromptPickPlayer :: GameSnapshot -> Selection -> NonEmpty PlayerHandle -> HearthPrompt PlayerHandle
    PromptPickCharacter :: GameSnapshot -> Selection -> NonEmpty CharacterHandle -> HearthPrompt CharacterHandle

deriving instance (Show a) => Show (HearthPrompt a)



