{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}


module Hearth.GameEvent where


--------------------------------------------------------------------------------


import Data.Data
import GHC.Prim (Constraint)
import Hearth.Model


--------------------------------------------------------------------------------


data GameEvent :: (* -> Constraint) -> * where
    GameBegins :: GameEvent k
    GameEnds :: GameResult -> GameEvent k
    PhaseEvent :: Scoped Phase -> GameEvent k
    DeckShuffled :: Handle Player -> Deck k -> GameEvent k
    CardDrawn :: Handle Player -> Either (DeckCard k) (HandCard k) -> Deck k -> GameEvent k
    UsedHeroPower :: Handle Player -> HeroPower k -> GameEvent k
    PlayedMinion :: Handle Player -> Handle Minion -> GameEvent k
    PlayedSpell :: Handle Player -> Handle Spell -> GameEvent k
    PlayedWeapon :: Handle Player -> Handle Weapon -> GameEvent k
    DealtDamage :: Handle Character -> Damage -> DamageSource -> GameEvent k
    HealthRestored :: Handle Character -> Health -> GameEvent k
    GainedArmor :: Handle Player -> Armor -> GameEvent k
    MinionDestroyed :: Handle Minion -> GameEvent k
    MinionDied :: Handle Minion -> GameEvent k
    EnactAttack :: Handle Character -> Handle Character -> GameEvent k
    GainsManaCrystal :: Handle Player -> Maybe CrystalState -> GameEvent k
    ManaCrystalsRefill :: Handle Player -> Int -> GameEvent k
    ManaCrystalsEmpty :: Handle Player -> Int -> GameEvent k
    LostDivineShield :: Handle Minion -> GameEvent k
    Silenced :: Handle Minion -> GameEvent k
    AttackFailed :: AttackFailedReason -> GameEvent k
    Transformed :: Handle Minion -> MinionCard k -> GameEvent k
    deriving (Typeable)


data AttackFailedReason :: * where
    AttackWithEnemy :: AttackFailedReason
    DefendWithFriendly :: AttackFailedReason
    ZeroAttack :: AttackFailedReason
    DoesNotHaveCharge :: AttackFailedReason
    OutOfAttacks :: AttackFailedReason
    TauntsExist :: AttackFailedReason
    AttackerIsFrozen :: AttackFailedReason
    deriving (Show, Typeable)




