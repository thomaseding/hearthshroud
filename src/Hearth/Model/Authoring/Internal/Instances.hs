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
    ViewIdInt(..),
    viewInt,
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
deriving instance Data IdInt
deriving instance Data Mana


class ViewIdInt (a :: *) where
    viewIdInt :: a -> IdInt

instance ViewIdInt IdInt where
    viewIdInt = id

instance ViewIdInt Armor where
    viewIdInt (Armor x) = x

instance ViewIdInt Attack where
    viewIdInt (Attack x) = x

instance ViewIdInt Health where
    viewIdInt (Health x) = x

instance ViewIdInt Durability where
    viewIdInt (Durability x) = x

instance ViewIdInt Damage where
    viewIdInt (Damage x) = x

instance ViewIdInt Mana where
    viewIdInt (Mana x) = x


viewInt :: (ViewIdInt a) => a -> Int
viewInt = f . viewIdInt
    where
        f (IdInt _ n) = n


instance Show Armor where
    show = show . viewInt

instance Show Attack where
    show = show . viewInt

instance Show Damage where
    show = show . viewInt

instance Show Durability where
    show = show . viewInt

instance Show Health where
    show = show . viewInt

instance Show Mana where
    show = show . viewInt

instance Show (Handle a) where
    show = show . getRawHandle


deriving instance Show Class
deriving instance Show Collectibility
deriving instance Show Comparison
deriving instance Show Cost
deriving instance Show CrystalState
deriving instance Show IdInt
deriving instance Show Rarity
deriving instance Show TimePoint
deriving instance Show Tribe


instance Eq IdInt where
    (==) = (==) `on` viewInt

instance Eq Armor where
    (==) = (==) `on` viewInt

instance Eq Attack where
    (==) = (==) `on` viewInt

instance Eq Damage where
    (==) = (==) `on` viewInt

instance Eq Durability where
    (==) = (==) `on` viewInt

instance Eq Health where
    (==) = (==) `on` viewInt

instance Eq Mana where
    (==) = (==) `on` viewInt

instance Eq (Handle a) where
    (==) = on (==) getRawHandle


deriving instance Eq Class
deriving instance Eq Collectibility
deriving instance Eq Cost
deriving instance Eq CrystalState
deriving instance Eq Comparison
deriving instance Eq Rarity
deriving instance Eq TimePoint
deriving instance Eq Tribe


instance Ord IdInt where
    compare = compare `on` viewInt

instance Ord Armor where
    compare = compare `on` viewInt

instance Ord Attack where
    compare = compare `on` viewInt

instance Ord Damage where
    compare = compare `on` viewInt

instance Ord Durability where
    compare = compare `on` viewInt

instance Ord Health where
    compare = compare `on` viewInt

instance Ord Mana where
    compare = compare `on` viewInt

instance Ord (Handle a) where
    (<=) = on (<=) getRawHandle


deriving instance Ord Class
deriving instance Ord CrystalState
deriving instance Ord Collectibility
deriving instance Ord Comparison
deriving instance Ord Cost
deriving instance Ord Rarity
deriving instance Ord Tribe
deriving instance Ord TimePoint


deriving instance Typeable Ability
deriving instance Typeable AnyEnchantment
deriving instance Typeable Armor
deriving instance Typeable Attack
deriving instance Typeable Card
deriving instance Typeable CardMeta
deriving instance Typeable Cost
deriving instance Typeable CrystalState
deriving instance Typeable Damage
deriving instance Typeable Durability
deriving instance Typeable Elect
deriving instance Typeable Effect
deriving instance Typeable Enchantment
deriving instance Typeable Handle
deriving instance Typeable Health
deriving instance Typeable Hero
deriving instance Typeable HeroPower
deriving instance Typeable Mana
deriving instance Typeable MinionCard
deriving instance Typeable SpellCard
deriving instance Typeable TimePoint
deriving instance Typeable WeaponCard


instance Enum IdInt where
    fromEnum (IdInt _ n) = n
    toEnum = IdInt Nothing


deriving instance Enum Armor
deriving instance Enum Attack
deriving instance Enum Damage
deriving instance Enum Durability
deriving instance Enum Health
deriving instance Enum Mana


instance Num IdInt where
    IdInt _ x + IdInt _ y = IdInt Nothing (x + y)
    IdInt _ x - IdInt _ y = IdInt Nothing (x - y)
    IdInt _ x * IdInt _ y = IdInt Nothing (x * y)
    negate (IdInt _ x) = IdInt Nothing (negate x)
    abs (IdInt _ x) = IdInt Nothing (abs x)
    signum (IdInt _ x) = IdInt Nothing (signum x)
    fromInteger = IdInt Nothing . fromInteger


deriving instance Num Armor
deriving instance Num Attack
deriving instance Num Mana
deriving instance Num Health
deriving instance Num Damage
deriving instance Num Durability


instance Real IdInt where
    toRational (IdInt _ x) = toRational x


deriving instance Real Armor
deriving instance Real Attack
deriving instance Real Damage
deriving instance Real Durability
deriving instance Real Health
deriving instance Real Mana


instance Integral IdInt where
    quot (IdInt _ x) (IdInt _ y) = IdInt Nothing (quot x y)
    rem (IdInt _ x) (IdInt _ y) = IdInt Nothing (rem x y)
    div (IdInt _ x) (IdInt _ y) = IdInt Nothing (div x y)
    mod (IdInt _ x) (IdInt _ y) = IdInt Nothing (mod x y)
    quotRem (IdInt _ x) (IdInt _ y) = let
        (q, r) = quotRem x y
        in (IdInt Nothing q, IdInt Nothing r)
    divMod (IdInt _ x) (IdInt _ y) = let
        (d, m) = divMod x y
        in (IdInt Nothing d, IdInt Nothing m)
    toInteger (IdInt _ x) = toInteger x


deriving instance Integral Armor
deriving instance Integral Attack
deriving instance Integral Damage
deriving instance Integral Durability
deriving instance Integral Health
deriving instance Integral Mana


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


