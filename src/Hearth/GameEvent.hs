{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}


module Hearth.GameEvent where


--------------------------------------------------------------------------------


import Hearth.Model


--------------------------------------------------------------------------------


data GameEvent :: * where
    CardDrawn :: PlayerHandle -> Maybe HandCard -> Deck -> GameEvent
    PlayedCard :: PlayerHandle -> HandCard -> Result -> GameEvent
    HeroTakesDamage :: PlayerHandle -> Health -> Damage -> GameEvent
    GainsManaCrystal :: PlayerHandle -> Maybe CrystalState -> GameEvent
    ManaCrystalsRefill :: PlayerHandle -> Int -> GameEvent
    ManaCrystalsEmpty :: PlayerHandle -> Int -> GameEvent
    deriving (Show, Eq, Ord)






