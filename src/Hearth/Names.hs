{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}


module Hearth.Names where


--------------------------------------------------------------------------------


import Data.Data
import Hearth.Names.Basic
import Hearth.Names.Classic
import Hearth.Names.Hero


--------------------------------------------------------------------------------


data HeroPowerName :: * where
    BasicHeroPowerName :: BasicHeroPowerName -> HeroPowerName
    deriving (Show, Eq, Ord, Data, Typeable)


data HeroName :: * where
    BasicHeroName :: BasicHeroName -> HeroName
    deriving (Show, Eq, Ord, Data, Typeable)


data CardName :: * where
    BasicCardName :: BasicCardName -> CardName
    ClassicCardName :: ClassicCardName -> CardName
    deriving (Show, Eq, Ord, Data, Typeable)





