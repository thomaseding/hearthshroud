{-# LANGUAGE TemplateHaskell #-}


module Hearth.Cards where


--------------------------------------------------------------------------------


import Control.Error.TH
import Data.List
import Data.Ord
import qualified Hearth.Cards.Basic as Basic
import qualified Hearth.Cards.Classic as Classic
import Hearth.Names
import Hearth.Model (DeckCard, deckCardName)


--------------------------------------------------------------------------------


cardByName :: CardName -> DeckCard
cardByName name = let
    mCard = flip find cards $ \card -> deckCardName card == name
    in case mCard of 
        Just card -> card
        Nothing -> $logicError 'cardByName $ "Card does not exist: " ++ show name


cards :: [DeckCard]
cards = sortBy (comparing $ dropWhile (/= ' ') . show . deckCardName) $ concat [
    Basic.cards,
    Classic.cards ]









