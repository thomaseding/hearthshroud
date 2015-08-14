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
    DeckShuffled :: Handle Player -> Deck -> GameEvent
    CardDrawn :: Handle Player -> Either DeckCard HandCard -> Deck -> GameEvent
    UsedHeroPower :: Handle Player -> HeroPower -> GameEvent
    PlayedMinion :: Handle Player -> Handle Minion -> GameEvent
    PlayedSpell :: Handle Player -> Handle Spell -> GameEvent
    TookDamage :: Handle Character -> Damage -> GameEvent
    HealthRestored :: Handle Character -> Health -> GameEvent
    GainedArmor :: Handle Player -> Armor -> GameEvent
    MinionDied :: Handle Minion -> GameEvent
    EnactAttack :: Handle Character -> Handle Character -> GameEvent
    GainsManaCrystal :: Handle Player -> Maybe CrystalState -> GameEvent
    ManaCrystalsRefill :: Handle Player -> Int -> GameEvent
    ManaCrystalsEmpty :: Handle Player -> Int -> GameEvent
    LostDivineShield :: Handle Minion -> GameEvent
    Silenced :: Handle Minion -> GameEvent
    AttackFailed :: AttackFailedReason -> GameEvent
    Transformed :: Handle Minion -> Minion -> GameEvent
    deriving (Typeable)


data AttackFailedReason :: * where
    AttackWithEnemy :: AttackFailedReason
    DefendWithFriendly :: AttackFailedReason
    ZeroAttack :: AttackFailedReason
    DoesNotHaveCharge :: AttackFailedReason
    OutOfAttacks :: AttackFailedReason
    TauntsExist :: AttackFailedReason
    deriving (Show, Typeable)




