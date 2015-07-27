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
    PlayedMinion :: PlayerHandle -> MinionHandle -> GameEvent
    PlayedSpell :: PlayerHandle -> Spell -> GameEvent   -- TODO: Should take a SpellHandle instead of Spell
    HeroTakesDamage :: PlayerHandle -> Health -> Armor -> Damage -> GameEvent
    MinionTakesDamage :: BoardMinion -> Damage -> GameEvent
    MinionDied :: BoardMinion -> GameEvent
    EnactAttack :: CharacterHandle -> CharacterHandle -> GameEvent
    GainsManaCrystal :: PlayerHandle -> Maybe CrystalState -> GameEvent
    ManaCrystalsRefill :: PlayerHandle -> Int -> GameEvent
    ManaCrystalsEmpty :: PlayerHandle -> Int -> GameEvent
    LostDivineShield :: BoardMinion -> GameEvent
    Silenced :: MinionHandle -> GameEvent
    deriving (Show, Typeable)






