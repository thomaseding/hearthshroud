{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}


module Hearth.Model.Authoring.CardName (
    CardName(..),
    showCardName,
) where


--------------------------------------------------------------------------------


import Data.Data
import Data.Namespace
import Hearth.CardSet.Basic.Names
import Hearth.CardSet.Classic.Names
import Text.LambdaOptions.Parseable
import Text.Read (readMaybe)


--------------------------------------------------------------------------------


data CardName :: * where
    BasicCardName :: BasicCardName -> CardName
    ClassicCardName :: ClassicCardName -> CardName
    ExternalCardName :: Namespace -> String -> CardName
    deriving (Eq, Ord, Data, Typeable)


showCardName :: CardName -> String
showCardName = takeWhile (/= '_') . \case
    BasicCardName name -> show name
    ClassicCardName name -> show name
    ExternalCardName _ name -> show name


instance Parseable CardName where
    parse = simpleParse $ \str -> case readMaybe str of
        Just name -> Just $ BasicCardName name
        Nothing -> case readMaybe str of
            Just name -> Just $ ClassicCardName name
            Nothing -> Nothing



