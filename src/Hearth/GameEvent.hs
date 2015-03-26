{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}


module Hearth.GameEvent where


--------------------------------------------------------------------------------


import Hearth.Model


--------------------------------------------------------------------------------


data GameEvent :: * where
    CardDrawn :: PlayerHandle -> Maybe HandCard -> Deck -> GameEvent
    HeroTakesDamage :: PlayerHandle -> Health -> Damage -> GameEvent
    deriving (Show, Eq, Ord)






