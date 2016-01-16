{-# LANGUAGE DeriveDataTypeable #-}


module Hearth.HeroPowerName where


--------------------------------------------------------------------------------


import Data.Data


--------------------------------------------------------------------------------


data HeroPowerName
    = ArmorUp
    | DaggerMastery
    | Fireblast
    | LesserHeal
    | LifeTap
    | Reinforce
    | Shapeshift
    | SteadyShot
    | TotemicCall
    deriving (Show, Eq, Ord, Enum, Data, Typeable)



