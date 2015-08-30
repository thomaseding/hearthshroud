{-# LANGUAGE DeriveDataTypeable #-}


module Hearth.Set.Classic.Names (
    ClassicCardName(..),
) where


--------------------------------------------------------------------------------


import Data.Data


--------------------------------------------------------------------------------


data ClassicCardName
    = Abomination
    | AldorPeacekeeper
    | AmaniBerserker
    | ArcaneGolem
    | ArgentCommander
    | ArgentProtector
    | ArgentSquire
    | Armorsmith
    | BattleRage
    | BigGameHunter
    | BlessedChampion
    | Brawl
    | CircleOfHealing
    | ColdlightOracle
    | CrazedAlchemist
    | CruelTaskmaster
    | DireWolfAlpha
    | EarthenRingFarseer
    | EarthShock
    | Equality
    | FenCreeper
    | FlameImp
    | GadgetzanAuctioneer
    | GrommashHellscream
    | HolyFire
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
    | Spellbreaker
    | StampedingKodo
    | Starfall
    | Sunwalker
    | TaurenWarrior
    | TempleEnforcer
    | TwistingNether
    | Wisp
    | Wrath
    deriving (Show, Eq, Ord, Enum, Data, Typeable)





