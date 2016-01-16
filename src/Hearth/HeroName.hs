{-# LANGUAGE DeriveDataTypeable #-}


module Hearth.HeroName (
    HeroName(..),
) where


--------------------------------------------------------------------------------


import Data.Data


--------------------------------------------------------------------------------


data HeroName
    = Anduin
    | Garrosh
    | Gul'dan
    | Jaina
    | Malfurion
    | Rexxar
    | Thrall
    | Uther
    | Valeera
    deriving (Show, Eq, Ord, Enum, Data, Typeable)





