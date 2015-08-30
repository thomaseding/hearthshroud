{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}


module Hearth.CardName where


--------------------------------------------------------------------------------


import Data.Data
import Hearth.Set.Basic.Names
import Hearth.Set.Classic.Names


--------------------------------------------------------------------------------


data CardName :: * where
    BasicCardName :: BasicCardName -> CardName
    ClassicCardName :: ClassicCardName -> CardName
    deriving (Show, Eq, Ord, Data, Typeable)





