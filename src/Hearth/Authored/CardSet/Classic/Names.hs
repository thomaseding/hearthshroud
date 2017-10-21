{-# LANGUAGE DeriveDataTypeable #-}


module Hearth.Authored.CardSet.Classic.Names (
    ClassicCardName(..),
) where


--------------------------------------------------------------------------------


import Data.Data


--------------------------------------------------------------------------------


data ClassicCardName
    = Abomination
    | AbusiveSergeant
    | Al'AkirTheWindlord
    | AldorPeacekeeper
    | AmaniBerserker
    | AncientOfLore
    | AncientWatcher
    | ArcaneGolem
    | ArgentCommander
    | ArgentProtector
    | ArgentSquire
    | Armorsmith
    | Ashbringer
    | AzureDrake
    | BaronGeddon
    | BattleRage
    | BigGameHunter
    | Bite
    | BlessedChampion
    | Blizzard
    | BloodmageThalnos
    | Brawl
    | CircleOfHealing
    | ColdlightOracle
    | CrazedAlchemist
    | CruelTaskmaster
    | DarkIronDwarf
    | DireWolfAlpha
    | EarthenRingFarseer
    | EarthShock
    | EmeraldDrake
    | Equality
    | FenCreeper
    | FlameImp
    | FrostElemental
    | GadgetzanAuctioneer
    | Gnoll
    | GrommashHellscream
    | Gruul
    | Hogger
    | HolyFire
    | Infernal
    | InjuredBlademaster
    | InnerRage
    | IronbeakOwl
    | LayOnHands
    | LeperGnome
    | LootHoarder
    | Malygos
    | MarkOfNature
    | MassDispel
    | Mogu'shanWarden
    | Naturalize
    | Nourish
    | PitLord
    | PriestessOfElune
    | Pyroblast
    | RagnarosTheFirelord
    | Rampage
    | ScarletCrusader
    | Shieldbearer
    | Silence
    | SilvermoonGuardian
    | SiphonSoul
    | SoulOfTheForest
    | Spellbreaker
    | StampedingKodo
    | Starfall
    | Sunwalker
    | TaurenWarrior
    | TempleEnforcer
    | TirionFordring
    | Treant_SoulOfTheForest
    | TwistingNether
    | WindfuryHarpy
    | Wisp
    | Wrath
    deriving (Show, Read, Eq, Ord, Enum, Data, Typeable)





