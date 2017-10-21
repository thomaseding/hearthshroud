{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}


module Hearth.Model.Runtime.GameEvent where


--------------------------------------------------------------------------------


import Data.Data
import Hearth.Model.Authoring
import Hearth.Model.Runtime


--------------------------------------------------------------------------------


data GameEvent :: * where
    GameBegins :: GameEvent
    GameEnds :: GameResult -> GameEvent
    PhaseEvent :: Scoped Phase -> GameEvent
    DeckShuffled :: Handle 'Player' -> Deck -> GameEvent
    CardDrawn :: Handle 'Player' -> Either DeckCard HandCard -> Deck -> GameEvent
    UsedHeroPower :: Handle 'Player' -> HeroPower -> GameEvent
    PlayedMinion :: Handle 'Player' -> Handle 'Minion' -> GameEvent
    PlayedSpell :: Handle 'Player' -> Handle 'Spell' -> GameEvent
    PlayedWeapon :: Handle 'Player' -> Handle 'Weapon' -> GameEvent
    DealtDamage :: Handle 'Character' -> Damage -> DamageSource -> GameEvent
    HealthRestored :: Handle 'Character' -> Health -> GameEvent
    GainedArmor :: Handle 'Player' -> Armor -> GameEvent
    MinionDestroyed :: Handle 'Minion' -> GameEvent
    MinionDied :: Handle 'Minion' -> GameEvent
    EnactAttack :: Handle 'Character' -> Handle 'Character' -> GameEvent
    GainsManaCrystal :: Handle 'Player' -> Maybe CrystalState -> GameEvent
    ManaCrystalsRefill :: Handle 'Player' -> Int -> GameEvent
    ManaCrystalsEmpty :: Handle 'Player' -> Int -> GameEvent
    LostDivineShield :: Handle 'Minion' -> GameEvent
    Silenced :: Handle 'Minion' -> GameEvent
    AttackFailed :: AttackFailedReason -> GameEvent
    Transformed :: Handle 'Minion' -> MinionCard -> GameEvent
    deriving (Typeable)


data AttackFailedReason :: * where
    AttackWithEnemy :: AttackFailedReason
    DefendWithFriendly :: AttackFailedReason
    ZeroAttack :: AttackFailedReason
    DoesNotHaveCharge :: AttackFailedReason
    OutOfAttacks :: AttackFailedReason
    TauntsExist :: AttackFailedReason
    AttackerIsFrozen :: AttackFailedReason
    AttackerCan'tAttack :: AttackFailedReason
    deriving (Show, Typeable)




