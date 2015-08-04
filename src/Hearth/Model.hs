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
    deriving (Show, Eq, Ord, Enum, Data, Typeable)


newtype Mana = Mana Int
    deriving (Show, Eq, Ord, Data, Typeable, Enum, Num, Real, Integral)


newtype Attack = Attack { unAttack :: Int }
    deriving (Show, Eq, Ord, Data, Typeable, Enum, Num, Real, Integral)


newtype Armor = Armor { unArmor :: Int }
    deriving (Show, Eq, Ord, Data, Typeable, Enum, Num, Real, Integral)


newtype Health = Health { unHealth :: Int }
    deriving (Show, Eq, Ord, Data, Typeable, Enum, Num, Real, Integral)


newtype Damage = Damage { unDamage :: Int }
    deriving (Show, Eq, Ord, Data, Typeable, Enum, Num, Real, Integral)


newtype RawHandle = RawHandle Int
    deriving (Show, Eq, Ord, Enum, Num, Real, Integral, Data, Typeable)


newtype MinionHandle = MinionHandle RawHandle
    deriving (Show, Eq, Ord, Data, Typeable)


newtype SpellHandle = SpellHandle RawHandle
    deriving (Show, Eq, Ord, Data, Typeable)


newtype PlayerHandle = PlayerHandle RawHandle
    deriving (Show, Eq, Ord, Data, Typeable)


type CharacterHandle = Either PlayerHandle MinionHandle


data CrystalState :: * where
    CrystalFull :: CrystalState
    CrystalEmpty :: CrystalState
    CrystalTemporary :: CrystalState
    deriving (Show, Eq, Ord, Data, Typeable)


data Cost :: * where
    ManaCost :: Mana -> Cost
    deriving (Show, Eq, Ord, Data, Typeable)


data Targeted
data AtRandom


data family ElectCont a :: *


data instance ElectCont Targeted
    = Targeted (Elect Targeted)
    | Effect Effect


newtype instance ElectCont AtRandom
    = FromRandom Effect


data Elect :: * -> * where
    CasterOf :: SpellHandle -> (PlayerHandle -> ElectCont a) -> Elect a
    OpponentOf :: PlayerHandle -> (PlayerHandle -> ElectCont a) -> Elect a
    ControllerOf :: MinionHandle -> (PlayerHandle -> ElectCont a) -> Elect a
    AnyCharacter :: (CharacterHandle -> ElectCont a) -> Elect a
    AnyEnemy :: (CharacterHandle -> ElectCont a) -> Elect a
    AnotherCharacter :: CharacterHandle -> (CharacterHandle -> ElectCont a) -> Elect a
    AnotherMinion :: MinionHandle -> (MinionHandle -> ElectCont a) -> Elect a
    AnotherFriendlyMinion :: MinionHandle -> (MinionHandle -> ElectCont a) -> Elect a
    OtherCharacters :: CharacterHandle -> ([CharacterHandle] -> ElectCont a) -> Elect a
    OtherEnemies :: CharacterHandle -> ([CharacterHandle] -> ElectCont a) -> Elect a
    deriving (Typeable)


data Effect :: * where
    Elect :: Elect AtRandom -> Effect
    With :: With -> Effect
    Sequence :: [Effect] -> Effect
    DrawCards :: PlayerHandle -> Int -> Effect
    KeywordEffect :: KeywordEffect -> Effect
    DealDamage :: CharacterHandle -> Damage -> Effect
    Enchant :: MinionHandle -> [Enchantment] -> Effect
    GiveAbility :: MinionHandle -> [Ability] -> Effect
    GainManaCrystal :: CrystalState -> PlayerHandle -> Effect
    deriving (Typeable)


data With :: * where
    EachMinion :: [MinionHandle] -> (MinionHandle -> Effect) -> With
    EachPlayer :: [PlayerHandle] -> (PlayerHandle -> Effect) -> With
    EachCharacter :: [CharacterHandle] -> (CharacterHandle -> Effect) -> With


data KeywordEffect :: * where
    Silence :: MinionHandle -> KeywordEffect
    deriving (Show, Typeable)


data Ability :: * where
    KeywordAbility :: KeywordAbility -> Ability
    deriving (Show, Typeable)


data KeywordAbility :: * where
    Battlecry :: (MinionHandle -> ElectCont Targeted) -> KeywordAbility
    Charge :: KeywordAbility
    DivineShield :: KeywordAbility
    Enrage :: [Ability] -> [Enchantment] -> KeywordAbility
    Taunt :: KeywordAbility
    deriving (Typeable)


instance Show KeywordAbility where
    show _ = "KeywordAbility"


data Enchantment :: * where
    StatsDelta :: Attack -> Health -> Enchantment
    --FrozenUntil :: Turn -> Enchantment
    deriving (Show, Eq, Ord, Data, Typeable)


type SpellEffect = SpellHandle -> ElectCont Targeted


instance Show SpellEffect where
    show _ = "SpellEffect"


data Spell = Spell {
    _spellCost :: Cost,
    _spellEffect :: SpellEffect,
    _spellName :: CardName
} deriving (Show, Typeable)
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
    _boardMinionDamage :: Damage,
    _boardMinionEnchantments :: [Enchantment],
    _boardMinionAbilities :: [Ability],
    _boardMinionAttackCount :: Int,
    _boardMinionNewlySummoned :: Bool,
    _boardMinionHandle :: MinionHandle,
    _boardMinion :: Minion
} deriving (Typeable)
makeLenses ''BoardMinion


data DeckMinion = DeckMinion {
    _deckMinion :: Minion,
    _deckSpell :: Spell
} deriving (Show, Typeable)
makeLenses ''DeckMinion


type HeroPowerEffect = PlayerHandle -> ElectCont Targeted


data HeroPower = HeroPower {
    _heroPowerCost :: Cost,
    _heroPowerEffect :: HeroPowerEffect
} deriving (Typeable)
makeLenses ''HeroPower


data Hero = Hero {
    _heroAttack :: Attack,
    _heroHealth :: Health,
    _heroPower :: HeroPower,
    _heroName :: HeroName
} deriving (Typeable)
makeLenses ''Hero


data BoardHero = BoardHero {
    _boardHeroDamage :: Damage,
    _boardHeroArmor :: Armor,
    _boardHeroAttackCount :: Int,
    _boardHero :: Hero
} deriving (Typeable)
makeLenses ''BoardHero


data HandCard :: * where
    HandCardMinion ::Minion -> HandCard
    HandCardSpell :: Spell -> HandCard
    deriving (Show, Typeable)


data DeckCard :: * where
    DeckCardMinion :: Minion -> DeckCard
    DeckCardSpell :: Spell -> DeckCard
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
    _playerTemporaryManaCrystals :: Int,
    _playerHero :: BoardHero
} deriving (Typeable)
makeLenses ''Player


data GameState = GameState {
    _gameTurn :: Turn,
    _gameHandleSeed :: RawHandle,
    _gamePlayerTurnOrder :: [PlayerHandle],
    _gamePlayers :: [Player]
} deriving (Typeable)
makeLenses ''GameState


data GameSnapshot = GameSnapshot {
    _snapshotGameState :: GameState
} deriving (Typeable)
makeLenses ''GameSnapshot


data GameResult :: * where
    GameResult :: GameResult
    deriving (Show, Eq, Ord, Typeable)


deckCardName :: DeckCard -> CardName
deckCardName = \case
    DeckCardMinion minion -> minion^.minionName
    DeckCardSpell spell -> spell^.spellName


handCardName :: HandCard -> CardName
handCardName = \case
    HandCardMinion minion -> minion^.minionName
    HandCardSpell spell -> spell^.spellName




























































