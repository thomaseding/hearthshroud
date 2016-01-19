{-# LANGUAGE DeriveDataTypeable #-}


module Hearth.CardSet.Classic.Names (
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
    | ArcaneGolem
    | ArgentCommander
    | ArgentProtector
    | ArgentSquire
    | Armorsmith
    | Ashbringer
    | AzureDrake
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
    | MarkOfNature
    | MassDispel
    | Mogu'shanWarden
    | Naturalize
    | Nourish
    | PitLord
    | PriestessOfElune
    | Pyroblast
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
    deriving (Show, Eq, Ord, Enum, Data, Typeable)





