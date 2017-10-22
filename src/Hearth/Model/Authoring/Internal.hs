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


module Hearth.Model.Authoring.Internal where


--------------------------------------------------------------------------------


import Data.Data


--------------------------------------------------------------------------------


data ObjectType
    = Spell'
    | Weapon'
    | Minion'
    | Player'
    | Character'
    deriving (Typeable)


newtype Mana = Mana Int


newtype Attack = Attack { unAttack :: Int }


newtype Armor = Armor { unArmor :: Int }


newtype Health = Health { unHealth :: Int }


newtype Damage = Damage { unDamage :: Int }


newtype Durability = Durability { unDurability :: Int }


data RawHandle :: * where
    RawHandle :: (Typeable userData) => userData -> Int -> RawHandle
    deriving (Typeable)


instance Show RawHandle where
    show (RawHandle _ x) = show x


instance Eq RawHandle where
    (RawHandle _ x) == (RawHandle _ y) = x == y


instance Ord RawHandle where
    (RawHandle _ x) <= (RawHandle _ y) = x <= y


data Handle :: ObjectType -> * where
    SpellHandle :: RawHandle -> Handle 'Spell'
    WeaponHandle :: RawHandle -> Handle 'Weapon'
    MinionHandle :: RawHandle -> Handle 'Minion'
    PlayerHandle :: RawHandle -> Handle 'Player'
    MinionCharacter :: Handle 'Minion' -> Handle 'Character'
    PlayerCharacter :: Handle 'Player' -> Handle 'Character'


getRawHandle :: Handle a -> RawHandle
getRawHandle = \case
    SpellHandle h -> h
    WeaponHandle h -> h
    MinionHandle h -> h
    PlayerHandle h -> h
    MinionCharacter h -> getRawHandle h
    PlayerCharacter h -> getRawHandle h


data HandleList :: ObjectType -> * where
    HandleList :: (Typeable userData) => userData -> [Handle a] -> HandleList a




