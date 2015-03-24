{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}


module Hearth.Prompt where


--------------------------------------------------------------------------------


import Data.NonEmpty
import Hearth.LogEvent
import Hearth.Model


--------------------------------------------------------------------------------


data HearthPrompt :: * -> * where
    PromptLogEvent :: LogEvent -> HearthPrompt ()
    PromptShuffle :: [a] -> HearthPrompt [a]
    PromptPickRandom :: NonEmpty a -> HearthPrompt a
    PromptMulligan :: PlayerHandle -> HearthPrompt [HandCard]

deriving instance (Show a) => Show (HearthPrompt a)



