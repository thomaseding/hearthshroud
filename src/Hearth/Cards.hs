{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}


module Hearth.Cards where


--------------------------------------------------------------------------------


import Control.Error.TH
import Data.List
import Hearth.CardName
import Hearth.Model
import qualified Hearth.CardSet.Basic.Cards as Basic
import qualified Hearth.CardSet.Classic.Cards as Classic


--------------------------------------------------------------------------------


entireUniverse :: Universe
entireUniverse = Universe $ concat [
    Basic.cards,
    Classic.cards ]


cardByName :: Universe -> CardName -> Card
cardByName (Universe cards) name = let
    mCard = flip find cards $ \card -> cardName card == name
    in case mCard of 
        Just card -> card
        Nothing -> $logicError 'cardByName $ "Card does not exist: " ++ showCardName name


class GetCardName a where
    cardName :: a -> CardName


instance GetCardName Card where
    cardName = \case
        CardMinion x -> cardName x
        CardSpell x -> cardName x
        CardWeapon x -> cardName x


instance GetCardName DeckCard where
    cardName = \case
        DeckCardMinion x -> cardName x
        DeckCardSpell x -> cardName x
        DeckCardWeapon x -> cardName x


instance GetCardName HandCard where
    cardName = \case
        HandCardMinion x -> cardName x
        HandCardSpell x -> cardName x
        HandCardWeapon x -> cardName x


instance GetCardName MinionCard where
    cardName = cardName . _minionMeta


instance GetCardName SpellCard where
    cardName = cardName . _spellMeta


instance GetCardName WeaponCard where
    cardName = cardName . _weaponMeta


instance GetCardName CardMeta where
    cardName = _cardMetaName







