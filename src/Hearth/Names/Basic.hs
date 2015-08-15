{-# LANGUAGE DeriveDataTypeable #-}


module Hearth.Names.Basic (
    BasicHeroPowerName(..),
    BasicCardName(..),
) where


--------------------------------------------------------------------------------


import Data.Data


--------------------------------------------------------------------------------


data BasicHeroPowerName
    = Fireblast
    | LifeTap
    deriving (Show, Eq, Ord, Enum, Data, Typeable)


data BasicCardName
    = ArcaneExplosion
    | ArcaneIntellect
    | ArcaneShot
    | Assassinate
    | Backstab
    | BlessingOfKings
    | BlessingOfMight
    | BluegillWarrior
    | BloodfenRaptor
    | BootyBayBodyguard
    | BoulderfistOgre
    | Charge
    | ChillwindYeti
    | Cleave
    | Consecration
    | CoreHound
    | DarkscaleHealer
    | DeadlyShot
    | DivineSpirit
    | DrainLife
    | DreadInfernal
    | ElvenArcher
    | Execute
    | FanOfKnives
    | Fireball
    | FireElemental
    | Flamestrike
    | Frog
    | FrostwolfGrunt
    | GnomishInventor
    | GoldshireFootman
    | GuardianOfKings
    | HammerOfWrath
    | HandOfProtection
    | HealingTouch
    | Hellfire
    | Hex
    | HolyLight
    | HolyNova
    | HolySmite
    | Humility
    | Hunter'sMark
    | Innervate
    | IronbarkProtector
    | IronforgeRifleman
    | Kor'kronElite
    | LordOfTheArena
    | MagmaRager
    | MarkOfTheWild
    | MindBlast
    | Moonfire
    | MultiShot
    | MurlocRaider
    | Nightblade
    | NoviceEngineer
    | OasisSnapjaw
    | Polymorph
    | PowerWordShield
    | RecklessRocketeer
    | RiverCrocolisk
    | Sen'jinShieldmasta
    | ShadowBolt
    | ShadowWordDeath
    | ShadowWordPain
    | ShatteredSunCleric
    | Sheep
    | Shiv
    | SilverbackPatriarch
    | SinisterStrike
    | Sprint
    | Starfire
    | StonetuskBoar
    | StormpikeCommando
    | StormwindChampion
    | StormwindKnight
    | Swipe
    | TheCoin
    | Voidwalker
    | VoodooDoctor
    | WarGolem
    | Whirlwind
    | WildGrowth
    | WolfRider
    deriving (Show, Eq, Ord, Enum, Data, Typeable)





