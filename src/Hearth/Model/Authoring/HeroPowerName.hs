{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}


module Hearth.Model.Authoring.HeroPowerName (
    HeroPowerName(..),
    showHeroPowerName,
) where


--------------------------------------------------------------------------------


import Data.Data
import Data.Namespace
import Hearth.Authored.HeroPower.Basic.Names
import Text.LambdaOptions.Parseable
import Text.Read (readMaybe)


--------------------------------------------------------------------------------


data HeroPowerName :: * where
    BasicHeroPowerName :: BasicHeroPowerName -> HeroPowerName
    ExternalHeroPowerName :: Namespace -> String -> HeroPowerName
    deriving (Eq, Ord, Data, Typeable)


showHeroPowerName :: HeroPowerName -> String
showHeroPowerName = takeWhile (/= '_') . \case
    BasicHeroPowerName name -> show name
    ExternalHeroPowerName _ name -> show name


instance Parseable HeroPowerName where
    parse = simpleParse $ \str -> case readMaybe str of
        Just name -> Just $ BasicHeroPowerName name
        Nothing -> Nothing



