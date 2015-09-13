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
    GameBegins :: GameEvent c
    GameEnds :: GameResult -> GameEvent c
    PhaseEvent :: Scoped Phase -> GameEvent c
    DeckShuffled :: Handle Player -> Deck c -> GameEvent c
    CardDrawn :: Handle Player -> Either (DeckCard c) (HandCard c) -> Deck c -> GameEvent c
    UsedHeroPower :: Handle Player -> HeroPower c -> GameEvent c
    PlayedMinion :: Handle Player -> Handle Minion -> GameEvent c
    PlayedSpell :: Handle Player -> Handle Spell -> GameEvent c
    DealtDamage :: Handle Character -> Damage -> DamageSource -> GameEvent c
    HealthRestored :: Handle Character -> Health -> GameEvent c
    GainedArmor :: Handle Player -> Armor -> GameEvent c
    MinionDestroyed :: Handle Minion -> GameEvent c
    MinionDied :: Handle Minion -> GameEvent c
    EnactAttack :: Handle Character -> Handle Character -> GameEvent c
    GainsManaCrystal :: Handle Player -> Maybe CrystalState -> GameEvent c
    ManaCrystalsRefill :: Handle Player -> Int -> GameEvent c
    ManaCrystalsEmpty :: Handle Player -> Int -> GameEvent c
    LostDivineShield :: Handle Minion -> GameEvent c
    Silenced :: Handle Minion -> GameEvent c
    AttackFailed :: AttackFailedReason -> GameEvent c
    Transformed :: Handle Minion -> MinionCard c -> GameEvent c
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




