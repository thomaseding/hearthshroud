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
    = BluegillWarrior
    | BloodfenRaptor
    | BoulderfistOgre
    | ChillwindYeti
    | CoreHound
    | DreadInfernal
    | ElvenArcher
    | IronforgeRifleman
    | FireElemental
    | FrostwolfGrunt
    | MagmaRager
    | MurlocRaider
    | NoviceEngineer
    | OasisSnapjaw
    | RecklessRocketeer
    | RiverCrocolisk
    | ShatteredSunCleric
    | StonetuskBoar
    | StormpikeCommando
    | StormwindKnight
    | WarGolem
    | WolfRider
    deriving (Show, Eq, Ord, Data, Typeable)


data ClassicCardName
    = ArgentSquire
    | IronbeakOwl
    | Spellbreaker
    | Sunwalker
    deriving (Show, Eq, Ord, Data, Typeable)



