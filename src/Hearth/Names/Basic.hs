{-# LANGUAGE DeriveDataTypeable #-}


module Hearth.Names.Basic (
    BasicCardName(..),
) where


--------------------------------------------------------------------------------


import Data.Data


--------------------------------------------------------------------------------


data BasicCardName
    = BluegillWarrior
    | BloodfenRaptor
    | BoulderfistOgre
    | ChillwindYeti
    | CoreHound
    | DreadInfernal
    | ElvenArcher
    | Innervate
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
    | WildGrowth
    | WolfRider
    deriving (Show, Eq, Ord, Data, Typeable)





