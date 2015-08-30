{-# LANGUAGE DeriveDataTypeable #-}


module Hearth.HeroPowerName where


--------------------------------------------------------------------------------


import Data.Data


--------------------------------------------------------------------------------


data HeroPowerName
    = Fireblast
    | LifeTap
    deriving (Show, Eq, Ord, Enum, Data, Typeable)



