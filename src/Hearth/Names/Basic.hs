{-# LANGUAGE DeriveDataTypeable #-}


module Hearth.Names.Basic (
    BasicCardName(..),
) where


--------------------------------------------------------------------------------


import Data.Data


--------------------------------------------------------------------------------


data BasicCardName
    = ArcaneExplosion
    | ArcaneIntellect
    | ArcaneShot
    | Assassinate
    | BlessingOfKings
    | BlessingOfMight
    | BluegillWarrior
    | BloodfenRaptor
    | BootyBayBodyguard
    | BoulderfistOgre
    | ChillwindYeti
    | Consecration
    | CoreHound
    | DarkscaleHealer
    | DrainLife
    | DreadInfernal
    | ElvenArcher
    | FanOfKnives
    | Fireball
    | FireElemental
    | Flamestrike
    | FrostwolfGrunt
    | GnomishInventor
    | GoldshireFootman
    | GuardianOfKings
    | HammerOfWrath
    | HandOfProtection
    | HealingTouch
    | Hellfire
    | HolyLight
    | HolyNova
    | HolySmite
    | Innervate
    | IronbarkProtector
    | IronforgeRifleman
    | LordOfTheArena
    | MagmaRager
    | MarkOfTheWild
    | MindBlast
    | Moonfire
    | MurlocRaider
    | Nightblade
    | NoviceEngineer
    | OasisSnapjaw
    | PowerWordShield
    | RecklessRocketeer
    | RiverCrocolisk
    | Sen'jinShieldmasta
    | ShadowBolt
    | ShatteredSunCleric
    | Shiv
    | SilverbackPatriarch
    | SinisterStrike
    | Sprint
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





