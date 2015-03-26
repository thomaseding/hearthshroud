{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}


module Hearth.Model where


--------------------------------------------------------------------------------


import Control.Lens
import Control.Newtype
import Data.Data
import Data.Monoid
import Hearth.Names
import GHC.Generics


--------------------------------------------------------------------------------


newtype Turn = Turn Int
    deriving (Show, Eq, Ord, Data, Typeable)


newtype Cost = Cost Int
    deriving (Show, Eq, Ord, Data, Typeable, Enum, Num, Real, Integral)


newtype Attack = Attack Int
    deriving (Show, Eq, Ord, Data, Typeable, Enum, Num, Real, Integral)


newtype Armor = Armor Int
    deriving (Show, Eq, Ord, Data, Typeable, Enum, Num, Real, Integral)


newtype Health = Health Int
    deriving (Show, Eq, Ord, Data, Typeable, Enum, Num, Real, Integral)


newtype Damage = Damage Int
    deriving (Show, Eq, Ord, Data, Typeable, Enum, Num, Real, Integral)


newtype PlayerHandle = PlayerHandle Int
    deriving (Show, Eq, Ord, Data, Typeable)


data Effect :: * where
    Effect :: Effect
    --WithElects :: [Elect] -> [Effect] -> Effect
    deriving (Show, Eq, Ord, Data, Typeable)


data Ability :: * where
    Charge :: Ability
    deriving (Show, Eq, Ord, Data, Typeable)


data Enchantment :: * where
    FrozenUntil :: Turn -> Enchantment
    deriving (Show, Eq, Ord, Data, Typeable)


data Spell = Spell {
    --_spellEffects :: [SpellEffect],
    _spellName :: CardName
} deriving (Show, Eq, Ord, Data, Typeable)
makeLenses ''Spell


data Minion = Minion {
    _minionAttack :: Attack,
    _minionHealth :: Health,
    _minionName :: CardName
} deriving (Show, Eq, Ord, Data, Typeable)
makeLenses ''Minion


data BoardMinion = BoardMinion {
    _boardMinionCurrAttack :: Attack,
    _boardMinionCurrHealth :: Health,
    _boardMinionEnchantments :: [Enchantment],
    _boardMinion :: Minion
} deriving (Show, Eq, Ord, Data, Typeable)
makeLenses ''BoardMinion


data DeckMinion = DeckMinion {
    _deckMinion :: Minion
} deriving (Show, Eq, Ord, Data, Typeable)
makeLenses ''DeckMinion


data HandMinion = HandMinion {
    --_handMinionEffects :: [HandEffect]  -- Think Bolvar
    _handMinion :: Minion
} deriving (Show, Eq, Ord, Data, Typeable)
makeLenses ''HandMinion


data HeroPower = HeroPower {
    _heroPowerCost :: Cost,
    _heroPowerEffects :: [Effect]
} deriving (Show, Eq, Ord, Data, Typeable)
makeLenses ''HeroPower


data Hero = Hero {
    _heroAttack :: Attack,
    _heroHealth :: Health,
    _heroPower :: HeroPower,
    _heroName :: HeroName
} deriving (Show, Eq, Ord, Data, Typeable)
makeLenses ''Hero


data BoardHero = BoardHero {
    _boardHeroCurrHealth :: Health,
    _boardHeroArmor :: Armor,
    _boardHero :: Hero
} deriving (Show, Eq, Ord, Data, Typeable)
makeLenses ''BoardHero


data HandCard :: * where
    HandCardMinion :: HandMinion -> HandCard
    deriving (Show, Eq, Ord, Data, Typeable)


data DeckCard :: * where
    DeckCardMinion :: DeckMinion -> DeckCard
    deriving (Show, Eq, Ord, Data, Typeable)


newtype Hand = Hand {
    _handCards :: [HandCard]
} deriving (Show, Eq, Ord, Monoid, Generic, Data, Typeable)
makeLenses ''Hand
instance Newtype Hand


newtype Deck = Deck {
    _deckCards :: [DeckCard]
} deriving (Show, Eq, Ord, Monoid, Generic, Data, Typeable)
makeLenses ''Deck
instance Newtype Deck


data Player = Player {
    _playerHandle :: PlayerHandle,
    _playerDeck :: Deck,
    _playerExcessDrawCount :: Int,
    _playerHand :: Hand,
    _playerMinions :: [BoardMinion],
    _playerHero :: BoardHero
} deriving (Show, Eq, Ord, Data, Typeable)
makeLenses ''Player


data GameState = GameState {
    _gameTurn :: Turn,
    _gamePlayerTurnOrder :: [PlayerHandle],
    _gamePlayers :: [Player]
} deriving (Show, Eq, Ord, Data, Typeable)
makeLenses ''GameState












