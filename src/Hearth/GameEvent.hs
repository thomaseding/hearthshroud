{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}


module Hearth.GameEvent where


--------------------------------------------------------------------------------


import Data.Data
import Hearth.Model


--------------------------------------------------------------------------------


data GameEvent :: * where
    GameBegins :: GameEvent
    GameEnds :: GameResult -> GameEvent
    DeckShuffled :: PlayerHandle -> Deck -> GameEvent
    CardDrawn :: PlayerHandle -> Either DeckCard HandCard -> Deck -> GameEvent
    PlayedCard :: PlayerHandle -> HandCard -> Result -> GameEvent
    HeroTakesDamage :: PlayerHandle -> Health -> Armor -> Damage -> GameEvent
    MinionTakesDamage :: BoardMinion -> Damage -> GameEvent
    MinionDied :: BoardMinion -> GameEvent
    AttackMinion :: BoardMinion -> BoardMinion -> GameEvent
    GainsManaCrystal :: PlayerHandle -> Maybe CrystalState -> GameEvent
    ManaCrystalsRefill :: PlayerHandle -> Int -> GameEvent
    ManaCrystalsEmpty :: PlayerHandle -> Int -> GameEvent
    LostDivineShield :: BoardMinion -> GameEvent
    EnrageActivated :: BoardMinion -> GameEvent
    Silenced :: BoardMinion -> GameEvent
    deriving (Show, Typeable)






