{-# LANGUAGE DeriveDataTypeable #-}


module Hearth.Names.Classic (
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
    | BattleRage
    | BigGameHunter
    | BlessedChampion
    | Brawl
    | CircleOfHealing
    | ColdlightOracle
    | CrazedAlchemist
    | CruelTaskmaster
    | EarthenRingFarseer
    | EarthShock
    | Equality
    | FenCreeper
    | FlameImp
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





