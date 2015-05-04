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
import Data.Data
import Data.Monoid
import Hearth.Names
import GHC.Generics


--------------------------------------------------------------------------------


data Result = Success | Failure
    deriving (Show, Eq, Ord, Data, Typeable)


newtype Turn = Turn Int
    deriving (Show, Eq, Ord, Data, Typeable)


newtype BoardPos = BoardPos Int
    deriving (Show, Eq, Ord, Data, Typeable)


newtype Mana = Mana Int
    deriving (Show, Eq, Ord, Data, Typeable, Enum, Num, Real, Integral)


newtype Attack = Attack { unAttack :: Int }
    deriving (Show, Eq, Ord, Data, Typeable, Enum, Num, Real, Integral)


newtype Armor = Armor { unArmor :: Int }
    deriving (Show, Eq, Ord, Data, Typeable, Enum, Num, Real, Integral)


newtype Health = Health { unHealth :: Int }
    deriving (Show, Eq, Ord, Data, Typeable, Enum, Num, Real, Integral)


newtype Damage = Damage Int
    deriving (Show, Eq, Ord, Data, Typeable, Enum, Num, Real, Integral)


newtype RawHandle = RawHandle Int
    deriving (Show, Eq, Ord, Enum, Num, Real, Integral, Data, Typeable)


newtype MinionHandle = MinionHandle RawHandle
    deriving (Show, Eq, Ord, Data, Typeable)


newtype PlayerHandle = PlayerHandle RawHandle
    deriving (Show, Eq, Ord, Data, Typeable)


data CrystalState :: * where
    CrystalFull :: CrystalState
    CrystalEmpty :: CrystalState
    deriving (Show, Eq, Ord, Data, Typeable)


data Cost :: * where
    ManaCost :: Mana -> Cost
    deriving (Show, Eq, Ord, Data, Typeable)


data Elect :: * where
    ControllerOf :: MinionHandle -> (PlayerHandle -> Effect) -> Elect
    TargetNoneOf :: [MinionHandle] -> (MinionHandle -> Effect) -> Elect
    AllOther :: [MinionHandle] -> (MinionHandle -> Effect) -> Elect
    deriving (Typeable)


instance Show Elect where
    show _ = "Elect"


data Effect :: * where
    With :: Elect -> Effect
    DrawCards :: Int -> PlayerHandle -> Effect
    KeywordEffect :: KeywordEffect -> Effect
    DealDamage :: Damage -> MinionHandle -> Effect
    deriving (Show, Typeable)


data KeywordEffect :: * where
    Silence :: MinionHandle -> KeywordEffect
    deriving (Show, Typeable)


data Ability :: * where
    KeywordAbility :: KeywordAbility -> Ability
    deriving (Show, Typeable)


data KeywordAbility :: * where
    Battlecry :: (MinionHandle -> Effect) -> KeywordAbility
    Charge :: KeywordAbility
    DivineShield :: KeywordAbility
    Taunt :: KeywordAbility
    deriving (Typeable)


instance Show KeywordAbility where
    show _ = "KeywordAbility"


data Enchantment :: * where
    FrozenUntil :: Turn -> Enchantment
    deriving (Show, Eq, Ord, Data, Typeable)


data Spell = Spell {
    --_spellEffects :: [SpellEffect],
    _spellName :: CardName
} deriving (Show, Eq, Ord, Data, Typeable)
makeLenses ''Spell


data Minion = Minion {
    _minionCost :: Cost,
    _minionAttack :: Attack,
    _minionHealth :: Health,
    _minionAbilities :: [Ability],
    _minionName :: CardName
} deriving (Show, Typeable)
makeLenses ''Minion


data BoardMinion = BoardMinion {
    _boardMinionCurrAttack :: Attack,
    _boardMinionCurrHealth :: Health,
    _boardMinionEnchantments :: [Enchantment],
    _boardMinionAbilities :: [Ability],
    _boardMinionAttackCount :: Either Int Int,
    _boardMinionHandle :: MinionHandle,
    _boardMinion :: Minion
} deriving (Show, Typeable)
makeLenses ''BoardMinion


data DeckMinion = DeckMinion {
    _deckMinion :: Minion
} deriving (Show, Typeable)
makeLenses ''DeckMinion


data HandMinion = HandMinion {
    --_handMinionEffects :: [HandEffect]  -- Think Bolvar, *Giants, Freezing Trap
    _handMinion :: Minion
} deriving (Show, Typeable)
makeLenses ''HandMinion


data HeroPower = HeroPower {
    _heroPowerCost :: Cost,
    _heroPowerEffects :: [Effect]
} deriving (Show, Typeable)
makeLenses ''HeroPower


data Hero = Hero {
    _heroAttack :: Attack,
    _heroHealth :: Health,
    _heroPower :: HeroPower,
    _heroName :: HeroName
} deriving (Show, Typeable)
makeLenses ''Hero


data BoardHero = BoardHero {
    _boardHeroCurrHealth :: Health,
    _boardHeroArmor :: Armor,
    _boardHero :: Hero
} deriving (Show, Typeable)
makeLenses ''BoardHero


data HandCard :: * where
    HandCardMinion :: HandMinion -> HandCard
    deriving (Show, Typeable)


data DeckCard :: * where
    DeckCardMinion :: DeckMinion -> DeckCard
    deriving (Show, Typeable)


newtype Hand = Hand {
    _handCards :: [HandCard]
} deriving (Show, Monoid, Generic, Typeable)
makeLenses ''Hand


newtype Deck = Deck {
    _deckCards :: [DeckCard]
} deriving (Show, Monoid, Generic, Typeable)
makeLenses ''Deck


data Player = Player {
    _playerHandle :: PlayerHandle,
    _playerDeck :: Deck,
    _playerExcessDrawCount :: Int,
    _playerHand :: Hand,
    _playerMinions :: [BoardMinion],
    _playerTotalManaCrystals :: Int,
    _playerEmptyManaCrystals :: Int,
    _playerHero :: BoardHero
} deriving (Show, Typeable)
makeLenses ''Player


data GameState = GameState {
    _gameTurn :: Turn,
    _gameHandleSeed :: RawHandle,
    _gamePlayerTurnOrder :: [PlayerHandle],
    _gamePlayers :: [Player]
} deriving (Show, Typeable)
makeLenses ''GameState


data GameSnapshot = GameSnapshot {
    _snapshotGameState :: GameState
} deriving (Show, Typeable)
makeLenses ''GameSnapshot


data GameResult :: * where
    GameResult :: GameResult
    deriving (Show, Eq, Ord, Typeable)


deckCardName :: DeckCard -> CardName
deckCardName = \case
    DeckCardMinion minion -> minion^.deckMinion.minionName


handCardName :: HandCard -> CardName
handCardName = \case
    HandCardMinion minion -> minion^.handMinion.minionName




























































