{-# LANGUAGE DeriveDataTypeable #-}


module Hearth.Set.Basic.Names (
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
    | FlametongueTotem
    | Frog
    | Frostbolt
    | FrostNova
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
    | RaidLeader
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
    | WaterElemental
    | Whirlwind
    | WildGrowth
    | WolfRider
    deriving (Show, Eq, Ord, Enum, Data, Typeable)





