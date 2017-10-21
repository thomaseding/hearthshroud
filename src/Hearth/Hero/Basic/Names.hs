{-# LANGUAGE DeriveDataTypeable #-}


module Hearth.Hero.Basic.Names where


import Data.Data


--------------------------------------------------------------------------------


data BasicHeroName
    = Anduin
    | Garrosh
    | Gul'dan
    | Jaina
    | Malfurion
    | Rexxar
    | Thrall
    | Uther
    | Valeera
    deriving (Show, Read, Eq, Ord, Enum, Data, Typeable)



