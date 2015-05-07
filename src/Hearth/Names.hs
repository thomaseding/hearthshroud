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
    | Moonfire
    | MurlocRaider
    | NoviceEngineer
    | OasisSnapjaw
    | RecklessRocketeer
    | RiverCrocolisk
    | ShatteredSunCleric
    | Starfire
    | StonetuskBoar
    | StormpikeCommando
    | StormwindKnight
    | TheCoin
    | WarGolem
    | WolfRider
    deriving (Show, Eq, Ord, Data, Typeable)


data ClassicCardName
    = AmaniBerserker
    | ArgentCommander
    | ArgentProtector
    | ArgentSquire
    | CruelTaskmaster
    | IronbeakOwl
    | ScarletCrusader
    | SilvermoonGuardian
    | Spellbreaker
    | Sunwalker
    deriving (Show, Eq, Ord, Data, Typeable)



