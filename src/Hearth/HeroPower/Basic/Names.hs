{-# LANGUAGE DeriveDataTypeable #-}


module Hearth.HeroPower.Basic.Names where


import Data.Data


--------------------------------------------------------------------------------


data BasicHeroPowerName
    = ArmorUp
    | DaggerMastery
    | Fireblast
    | LesserHeal
    | LifeTap
    | Reinforce
    | Shapeshift
    | SteadyShot
    | TotemicCall
    deriving (Show, Read, Eq, Ord, Enum, Data, Typeable)


