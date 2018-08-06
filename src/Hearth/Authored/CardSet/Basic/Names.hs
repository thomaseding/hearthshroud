{-# LANGUAGE DeriveDataTypeable #-}


module Hearth.Authored.CardSet.Basic.Names (
    BasicCardName(..),
) where


--------------------------------------------------------------------------------


import Data.Data


--------------------------------------------------------------------------------


data BasicCardName
    = AcidicSwampOoze
    | AncestralHealing
    | AnimalCompanion
    | ArcaneExplosion
    | ArcaneIntellect
    | ArcaneMissiles
    | ArcaneShot
    | ArcaniteReaper
    | Archmage
    | Assassinate
    | Assassin'sBlade
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
    | DeadlyPoison
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
    | Light'sJustice
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
    | TruesilverChampion
    | TundraRhino
    | Voidwalker
    | VoodooDoctor
    | WarGolem
    | WarsongCommander
    | WaterElemental
    | Whirlwind
    | WickedKnife
    | WildGrowth
    | Windfury
    | Windspeaker
    | WolfRider
    | WrathOfAirTotem
    deriving (Show, Read, Eq, Ord, Enum, Data, Typeable)





