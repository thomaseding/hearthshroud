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
    | FenCreeper
    | InjuredBlademaster
    | IronbeakOwl
    | LeperGnome
    | LootHoarder
    | Mogu'shanWarden
    | PriestessOfElune
    | ScarletCrusader
    | SilvermoonGuardian
    | Spellbreaker
    | Sunwalker
    | Wisp
    | Wrath
    deriving (Show, Eq, Ord, Enum, Data, Typeable)





