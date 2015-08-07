{-# LANGUAGE DeriveDataTypeable #-}


module Hearth.Names.Basic (
    BasicCardName(..),
) where


--------------------------------------------------------------------------------


import Data.Data


--------------------------------------------------------------------------------


data BasicCardName
    = Assassinate
    | BluegillWarrior
    | BloodfenRaptor
    | BootyBayBodyguard
    | BoulderfistOgre
    | ChillwindYeti
    | Consecration
    | CoreHound
    | DarkscaleHealer
    | DreadInfernal
    | ElvenArcher
    | FanOfKnives
    | Fireball
    | FireElemental
    | Flamestrike
    | FrostwolfGrunt
    | GnomishInventor
    | GoldshireFootman
    | HolyNova
    | HolySmite
    | Innervate
    | IronforgeRifleman
    | LordOfTheArena
    | MagmaRager
    | Moonfire
    | MurlocRaider
    | Nightblade
    | NoviceEngineer
    | OasisSnapjaw
    | RecklessRocketeer
    | RiverCrocolisk
    | Sen'jinShieldmasta
    | ShatteredSunCleric
    | SilverbackPatriarch
    | Starfire
    | StonetuskBoar
    | StormpikeCommando
    | StormwindKnight
    | Swipe
    | TheCoin
    | VoodooDoctor
    | WarGolem
    | WildGrowth
    | WolfRider
    deriving (Show, Eq, Ord, Enum, Data, Typeable)





