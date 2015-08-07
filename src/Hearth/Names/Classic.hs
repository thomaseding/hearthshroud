{-# LANGUAGE DeriveDataTypeable #-}


module Hearth.Names.Classic (
    ClassicCardName(..),
) where


--------------------------------------------------------------------------------


import Data.Data


--------------------------------------------------------------------------------


data ClassicCardName
    = Abomination
    | AmaniBerserker
    | ArcaneGolem
    | ArgentCommander
    | ArgentProtector
    | ArgentSquire
    | ColdlightOracle
    | CruelTaskmaster
    | EarthenRingFarseer
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
    deriving (Show, Eq, Ord, Enum, Data, Typeable)





