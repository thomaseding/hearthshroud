{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}


module Hearth.CardName where


--------------------------------------------------------------------------------


import Data.Data
import Hearth.CardSet.Basic.Names
import Hearth.CardSet.Classic.Names


--------------------------------------------------------------------------------


data CardName :: * where
    BasicCardName :: BasicCardName -> CardName
    ClassicCardName :: ClassicCardName -> CardName
    deriving (Eq, Ord, Data, Typeable)


showCardName :: CardName -> String
showCardName = takeWhile (/= '_') . \case
    BasicCardName name -> show name
    ClassicCardName name -> show name



