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
    HeroTakesDamage :: PlayerHandle -> Damage -> GameEvent
    MinionTakesDamage :: MinionHandle -> Damage -> GameEvent
    MinionDied :: MinionHandle -> GameEvent
    EnactAttack :: CharacterHandle -> CharacterHandle -> GameEvent
    GainsManaCrystal :: PlayerHandle -> Maybe CrystalState -> GameEvent
    ManaCrystalsRefill :: PlayerHandle -> Int -> GameEvent
    ManaCrystalsEmpty :: PlayerHandle -> Int -> GameEvent
    LostDivineShield :: MinionHandle -> GameEvent
    Silenced :: MinionHandle -> GameEvent
    AttackFailed :: AttackFailedReason -> GameEvent
    deriving (Typeable)


data AttackFailedReason :: * where
    AttackWithEnemy :: AttackFailedReason
    DefendWithFriendly :: AttackFailedReason
    ZeroAttack :: AttackFailedReason
    DoesNotHaveCharge :: AttackFailedReason
    OutOfAttacks :: AttackFailedReason
    TauntsExist :: AttackFailedReason
    deriving (Show, Typeable)




