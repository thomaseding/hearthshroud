{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}


module Hearth.GameEvent where


--------------------------------------------------------------------------------


import Hearth.Model


--------------------------------------------------------------------------------


data GameEvent :: * where
    CardDrawn :: PlayerHandle -> HandCard -> GameEvent
    deriving (Show, Eq, Ord)






