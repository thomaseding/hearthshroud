{-# LANGUAGE TemplateHaskell #-}


module Hearth.Cards where


--------------------------------------------------------------------------------


import Control.Error.TH
import Data.List
import Data.Ord
import Hearth.CardName
import Hearth.Model (DeckCard, deckCardName)
import qualified Hearth.Set.Basic.Cards as Basic
import qualified Hearth.Set.Classic.Cards as Classic


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









