{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hearth.Model.Authoring.Internal.Instances (
    HasUserData(..),
) where


--------------------------------------------------------------------------------


import Data.Data
import Data.Function (on)
import Hearth.Model.Authoring
import Hearth.Model.Authoring.Internal


--------------------------------------------------------------------------------


deriving instance Data Armor
deriving instance Data Attack
deriving instance Data Cost
deriving instance Data CrystalState
deriving instance Data Damage
deriving instance Data Durability
deriving instance Data Health
deriving instance Data Mana


deriving instance Show Armor
deriving instance Show Attack
deriving instance Show Class
deriving instance Show Collectibility
deriving instance Show Comparison
deriving instance Show Cost
deriving instance Show CrystalState
deriving instance Show Damage
deriving instance Show Durability
deriving instance Show Health
deriving instance Show Mana
deriving instance Show Rarity
deriving instance Show TimePoint
deriving instance Show Tribe


deriving instance Eq Class
deriving instance Eq Armor
deriving instance Eq Damage
deriving instance Eq Attack
deriving instance Eq Health
deriving instance Eq Durability
deriving instance Eq TimePoint
deriving instance Eq Cost
deriving instance Eq Collectibility
deriving instance Eq Mana
deriving instance Eq CrystalState
deriving instance Eq Comparison
deriving instance Eq Tribe
deriving instance Eq Rarity


deriving instance Ord Collectibility
deriving instance Ord CrystalState
deriving instance Ord Class
deriving instance Ord Mana
deriving instance Ord Rarity
deriving instance Ord Armor
deriving instance Ord Attack
deriving instance Ord Durability
deriving instance Ord Damage
deriving instance Ord Health
deriving instance Ord Comparison
deriving instance Ord Tribe
deriving instance Ord Cost
deriving instance Ord TimePoint


deriving instance Typeable Elect
deriving instance Typeable CrystalState
deriving instance Typeable Cost
deriving instance Typeable Effect
deriving instance Typeable Ability
deriving instance Typeable TimePoint
deriving instance Typeable Enchantment
deriving instance Typeable AnyEnchantment
deriving instance Typeable CardMeta
deriving instance Typeable SpellCard
deriving instance Typeable Damage
deriving instance Typeable WeaponCard
deriving instance Typeable MinionCard
deriving instance Typeable Health
deriving instance Typeable HeroPower
deriving instance Typeable Mana
deriving instance Typeable Hero
deriving instance Typeable Attack
deriving instance Typeable Card
deriving instance Typeable Armor
deriving instance Typeable Durability
deriving instance Typeable Handle


deriving instance Enum Armor
deriving instance Enum Durability
deriving instance Enum Damage
deriving instance Enum Mana
deriving instance Enum Health
deriving instance Enum Attack


deriving instance Num Armor
deriving instance Num Attack
deriving instance Num Mana
deriving instance Num Health
deriving instance Num Damage
deriving instance Num Durability


deriving instance Real Armor
deriving instance Real Damage
deriving instance Real Durability
deriving instance Real Attack
deriving instance Real Health
deriving instance Real Mana


deriving instance Integral Armor
deriving instance Integral Mana
deriving instance Integral Attack
deriving instance Integral Health
deriving instance Integral Damage
deriving instance Integral Durability


instance Show (Handle a) where
    show = show . getRawHandle


instance Eq (Handle a) where
    (==) = on (==) getRawHandle


instance Ord (Handle a) where
    (<=) = on (<=) getRawHandle


class HasUserData a where
    getUserData :: (Typeable u) => a -> Maybe u
    setUserData :: (Typeable u) => a -> u -> a


instance HasUserData RawHandle where
    getUserData (RawHandle u _) = cast u
    setUserData (RawHandle _ n) u = RawHandle u n


instance HasUserData (Handle a) where
    getUserData = getUserData . getRawHandle
    setUserData = \case
        SpellHandle h -> SpellHandle . setUserData h
        WeaponHandle h -> WeaponHandle . setUserData h
        MinionHandle h -> MinionHandle . setUserData h
        PlayerHandle h -> PlayerHandle . setUserData h
        MinionCharacter h -> MinionCharacter . setUserData h
        PlayerCharacter h -> PlayerCharacter . setUserData h


instance HasUserData (HandleList a) where
    getUserData (HandleList u _) = cast u
    setUserData (HandleList _ hs) u = HandleList u hs


