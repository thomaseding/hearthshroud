{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}


module Hearth.Cards where


--------------------------------------------------------------------------------


import Control.Error.TH
import Data.List
import Data.Ord
import Hearth.CardName
import Hearth.Model
import qualified Hearth.Set.Basic.Cards as Basic
import qualified Hearth.Set.Classic.Cards as Classic


--------------------------------------------------------------------------------


cardUniverse :: [DeckCard]
cardUniverse = sortBy (comparing $ dropWhile (/= ' ') . showCardName . deckCardName) $ concat [
    Basic.cards,
    Classic.cards ]


cardByName :: CardName -> DeckCard
cardByName name = let
    mCard = flip find cardUniverse $ \card -> deckCardName card == name
    in case mCard of 
        Just card -> card
        Nothing -> $logicError 'cardByName $ "Card does not exist: " ++ showCardName name


class GetCardName a where
    cardName :: a -> CardName


instance GetCardName DeckCard where
    cardName = \case
        DeckCardMinion x -> cardName x
        DeckCardSpell x -> cardName x


instance GetCardName Minion where
    cardName = cardName . _minionMeta


instance GetCardName Spell where
    cardName = cardName . _spellMeta


instance GetCardName CardMeta where
    cardName = _cardMetaName







