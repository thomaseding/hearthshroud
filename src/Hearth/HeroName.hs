{-# LANGUAGE DeriveDataTypeable #-}


module Hearth.HeroName (
    HeroName(..),
) where


--------------------------------------------------------------------------------


import Data.Data


--------------------------------------------------------------------------------


data HeroName
    = Malfurion
    | Rexxar
    | Jaina
    | Uther
    | Anduin
    | Valeera
    | Thrall
    | Gul'dan
    | Garrosh
    deriving (Show, Eq, Ord, Enum, Data, Typeable)





