{-# LANGUAGE ConstraintKinds #-}
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


cardUniverse :: (UserConstraint k) => [Card k]
cardUniverse = sortBy (comparing $ dropWhile (/= ' ') . showCardName . cardName) $ concat [
    Basic.cards,
    Classic.cards ]


cardByName :: (UserConstraint k) => CardName -> Card k
cardByName name = let
    mCard = flip find cardUniverse $ \card -> cardName card == name
    in case mCard of 
        Just card -> card
        Nothing -> $logicError 'cardByName $ "Card does not exist: " ++ showCardName name


class GetCardName a where
    cardName :: a -> CardName


instance GetCardName (Card k) where
    cardName = \case
        CardMinion x -> cardName x
        CardSpell x -> cardName x
        CardWeapon x -> cardName x


instance GetCardName (DeckCard k) where
    cardName = \case
        DeckCardMinion x -> cardName x
        DeckCardSpell x -> cardName x
        DeckCardWeapon x -> cardName x


instance GetCardName (HandCard k) where
    cardName = \case
        HandCardMinion x -> cardName x
        HandCardSpell x -> cardName x
        HandCardWeapon x -> cardName x


instance GetCardName (MinionCard k) where
    cardName = cardName . _minionMeta


instance GetCardName (SpellCard k) where
    cardName = cardName . _spellMeta


instance GetCardName (WeaponCard k) where
    cardName = cardName . _weaponMeta


instance GetCardName CardMeta where
    cardName = _cardMetaName







