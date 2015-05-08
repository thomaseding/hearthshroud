{-# LANGUAGE DeriveDataTypeable #-}


module Hearth.Names.Classic where


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
    | IronbeakOwl
    | ScarletCrusader
    | SilvermoonGuardian
    | Spellbreaker
    | Sunwalker
    deriving (Show, Eq, Ord, Data, Typeable)





