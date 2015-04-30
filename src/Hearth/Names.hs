{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}


module Hearth.Names where


--------------------------------------------------------------------------------


import Data.Data


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
    ClassicCardName :: ClassicCardName -> CardName
    deriving (Show, Eq, Ord, Data, Typeable)


data BasicCardName
    = BloodfenRaptor
    | BoulderfistOgre
    | ChillwindYeti
    | CoreHound
    | FrostwolfGrunt
    | MagmaRager
    | MurlocRaider
    | OasisSnapjaw
    | RiverCrocolisk
    | WarGolem
    deriving (Show, Eq, Ord, Data, Typeable)


data ClassicCardName
    = ArgentSquire
    | Sunwalker
    deriving (Show, Eq, Ord, Data, Typeable)



