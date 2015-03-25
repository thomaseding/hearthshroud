{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}


module Hearth.Names where


--------------------------------------------------------------------------------


import Data.Data
import Data.Typeable


--------------------------------------------------------------------------------


data HeroName :: * where
    BasicHeroName :: BasicHeroName -> HeroName
    deriving (Show, Eq, Ord, Data, Typeable)


data BasicHeroName
    = Malfurion
    | Rexxar
    | Jaina
    | Uther
    | Anduin
    | Valeera
    | Thrall
    | Gul'dan
    | Garrosh
    deriving (Show, Eq, Ord, Data, Typeable)


data CardName :: * where
    BasicCardName :: BasicCardName -> CardName
    deriving (Show, Eq, Ord, Data, Typeable)


data BasicCardName
    = MurlocRaider
    deriving (Show, Eq, Ord, Data, Typeable)



