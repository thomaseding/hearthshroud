{-# LANGUAGE DeriveDataTypeable #-}


module Hearth.Set.Basic.Names (
    BasicCardName(..),
) where


--------------------------------------------------------------------------------


import Data.Data


--------------------------------------------------------------------------------


data BasicCardName
    = AnimalCompanion
    | ArcaneExplosion
    | ArcaneIntellect
    | ArcaneShot
    | Archmage
    | Assassinate
    | Backstab
    | BlessingOfKings
    | BlessingOfMight
    | BloodfenRaptor
    | Bloodlust
    | BluegillWarrior
    | Boar
    | BootyBayBodyguard
    | BoulderfistOgre
    | Charge
    | ChillwindYeti
    | Claw
    | Cleave
    | Consecration
    | CoreHound
    | Corruption
    | DalaranMage
    | DarkscaleHealer
    | DeadlyShot
    | DivineSpirit
    | DragonlingMechanic
    | DrainLife
    | DreadInfernal
    | ElvenArcher
    | ExcessMana
    | Execute
    | FanOfKnives
    | Fireball
    | FireElemental
    | Flamestrike
    | FlametongueTotem
    | Frog
    | Frostbolt
    | FrostNova
    | FrostShock
    | FrostwolfGrunt
    | FrostwolfWarlord
    | GurubashiBerserker
    | GnomishInventor
    | GoldshireFootman
    | GuardianOfKings
    | HammerOfWrath
    | HandOfProtection
    | HealingTotem
    | HealingTouch
    | Hellfire
    | HeroicStrike
    | Hex
    | HolyLight
    | HolyNova
    | HolySmite
    | Huffer
    | Humility
    | Hunter'sMark
    | Innervate
    | IronbarkProtector
    | IronforgeRifleman
    | KoboldGeomancer
    | Kor'kronElite
    | Leokk
    | LordOfTheArena
    | MagmaRager
    | MarkOfTheWild
    | MechanicalDragonling
    | MindBlast
    | MirrorImage
    | Misha
    | Moonfire
    | MortalCoil
    | MultiShot
    | MurlocRaider
    | MurlocScout
    | MurlocTidehunter
    | Nightblade
    | NorthshireCleric
    | NoviceEngineer
    | OasisSnapjaw
    | OgreMagi
    | Polymorph
    | PowerWordShield
    | RaidLeader
    | RazorfenHunter
    | RecklessRocketeer
    | RiverCrocolisk
    | RockbiterWeapon
    | SavageRoar
    | SearingTotem
    | Sen'jinShieldmasta
    | ShadowBolt
    | ShadowWordDeath
    | ShadowWordPain
    | ShatteredSunCleric
    | Sheep
    | Shiv
    | SilverbackPatriarch
    | SilverHandRecruit
    | SinisterStrike
    | Sprint
    | Starfire
    | StoneclawTotem
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





