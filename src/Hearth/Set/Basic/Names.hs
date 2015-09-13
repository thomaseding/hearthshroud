{-# LANGUAGE DeriveDataTypeable #-}


module Hearth.Set.Basic.Names (
    BasicCardName(..),
) where


--------------------------------------------------------------------------------


import Data.Data


--------------------------------------------------------------------------------


data BasicCardName
    = AncestralHealing
    | AnimalCompanion
    | ArcaneExplosion
    | ArcaneIntellect
    | ArcaneMissiles
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
    | GrimscaleOracle
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
    | Houndmaster
    | Huffer
    | Humility
    | Hunter'sMark
    | Innervate
    | IronbarkProtector
    | IronforgeRifleman
    | KillCommand
    | KoboldGeomancer
    | Kor'kronElite
    | Leokk
    | LordOfTheArena
    | MagmaRager
    | MarkOfTheWild
    | MechanicalDragonling
    | MindBlast
    | MindControl
    | MirrorImage_Minion
    | MirrorImage_Spell
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
    | SacrificialPact
    | SavageRoar
    | SearingTotem
    | Sen'jinShieldmasta
    | ShadowBolt
    | ShadowWordDeath
    | ShadowWordPain
    | ShatteredSunCleric
    | Sheep
    | ShieldBlock
    | Shiv
    | SilverbackPatriarch
    | SilverHandRecruit
    | SinisterStrike
    | Soulfire
    | Sprint
    | Starfire
    | StoneclawTotem
    | StonetuskBoar
    | StormpikeCommando
    | StormwindChampion
    | StormwindKnight
    | Succubus
    | Swipe
    | TheCoin
    | TimberWolf
    | TotemicMight
    | TundraRhino
    | Voidwalker
    | VoodooDoctor
    | WarGolem
    | WaterElemental
    | Whirlwind
    | WildGrowth
    | Windfury
    | Windspeaker
    | WolfRider
    | WrathOfAirTotem
    deriving (Show, Eq, Ord, Enum, Data, Typeable)





