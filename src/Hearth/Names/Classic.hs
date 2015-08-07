{-# LANGUAGE DeriveDataTypeable #-}


module Hearth.Names.Classic (
    ClassicCardName(..),
) where


--------------------------------------------------------------------------------


import Data.Data


--------------------------------------------------------------------------------


data ClassicCardName
    = AmaniBerserker
    | ArcaneGolem
    | ArgentCommander
    | ArgentProtector
    | ArgentSquire
    | CruelTaskmaster
    | InjuredBlademaster
    | IronbeakOwl
    | ScarletCrusader
    | SilvermoonGuardian
    | Spellbreaker
    | Sunwalker
    deriving (Show, Eq, Ord, Enum, Data, Typeable)





