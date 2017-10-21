{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}


module Hearth.Model.Authoring.HeroName (
    HeroName(..),
    showHeroName,
) where


--------------------------------------------------------------------------------


import Data.Data
import Data.Namespace
import Hearth.Authored.Hero.Basic.Names
import Text.LambdaOptions.Parseable
import Text.Read (readMaybe)


--------------------------------------------------------------------------------


data HeroName :: * where
    BasicHeroName :: BasicHeroName -> HeroName
    ExternalHeroName :: Namespace -> String -> HeroName
    deriving (Eq, Ord, Data, Typeable)


showHeroName :: HeroName -> String
showHeroName = takeWhile (/= '_') . \case
    BasicHeroName name -> show name
    ExternalHeroName _ name -> show name


instance Parseable HeroName where
    parse = simpleParse $ \str -> case readMaybe str of
        Just name -> Just $ BasicHeroName name
        Nothing -> Nothing



