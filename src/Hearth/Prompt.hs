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


data HearthPrompt :: * -> * where
    PromptDebugEvent :: DebugEvent -> HearthPrompt ()
    PromptGameEvent :: GameEvent -> HearthPrompt ()
    PromptAction :: HearthPrompt Action
    --PromptQuery :: HearthPrompt Query
    PromptShuffle :: [a] -> HearthPrompt [a]
    PromptPickRandom :: NonEmpty a -> HearthPrompt a
    PromptMulligan :: PlayerHandle -> [HandCard] -> HearthPrompt [HandCard]

deriving instance (Show a) => Show (HearthPrompt a)



