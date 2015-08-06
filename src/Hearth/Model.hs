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


import Control.Lens hiding (Each)
import Data.Data
import Data.Function
import Data.Monoid (Monoid)
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


data Character


data Handle :: * -> * where
    SpellHandle :: RawHandle -> Handle Spell
    MinionHandle :: RawHandle -> Handle Minion
    PlayerHandle :: RawHandle -> Handle Player
    MinionCharacter :: MinionHandle -> Handle Character
    PlayerCharacter :: PlayerHandle -> Handle Character
    deriving (Typeable)


mapHandle :: (Handle Spell -> b) -> (Handle Minion -> b) -> (Handle Player -> b) -> (Handle Character -> b) -> (Handle a -> b)
mapHandle spell minion player character = \case
    h @ SpellHandle {} -> spell h
    h @ MinionHandle {} -> minion h
    h @ PlayerHandle {} -> player h
    h @ MinionCharacter {} -> character h
    h @ PlayerCharacter {} -> character h


applyRawHandle :: (RawHandle -> b) -> Handle a -> b
applyRawHandle f = \case
    SpellHandle h -> f h
    MinionHandle h -> f h
    PlayerHandle h -> f h
    MinionCharacter h -> applyRawHandle f h
    PlayerCharacter h -> applyRawHandle f h


instance Show (Handle a) where
    show = applyRawHandle show


instance Eq (Handle a) where
    (==) = on (==) $ applyRawHandle id


instance Ord (Handle a) where
    (<=) = on (<=) $ applyRawHandle id


type SpellHandle = Handle Spell
type MinionHandle = Handle Minion
type PlayerHandle = Handle Player
type CharacterHandle = Handle Character


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


data family ElectionEffect a :: *


data instance ElectionEffect Targeted
    = Targeted (Elect Targeted)
    | Effect Effect


newtype instance ElectionEffect AtRandom
    = FromRandom Effect


data Elect :: * -> * where
    AnyCharacter :: (CharacterHandle -> ElectionEffect a) -> Elect a
    AnyEnemy :: (CharacterHandle -> ElectionEffect a) -> Elect a
    AnotherCharacter :: CharacterHandle -> (CharacterHandle -> ElectionEffect a) -> Elect a
    AnotherMinion :: MinionHandle -> (MinionHandle -> ElectionEffect a) -> Elect a
    AnotherFriendlyMinion :: MinionHandle -> (MinionHandle -> ElectionEffect a) -> Elect a
    deriving (Typeable)


data Effect :: * where
    Elect :: Elect AtRandom -> Effect
    With :: With -> Effect
    ForEach :: [Handle a] -> ((Handle a) -> Effect) -> Effect
    Sequence :: [Effect] -> Effect
    DrawCards :: PlayerHandle -> Int -> Effect
    KeywordEffect :: KeywordEffect -> Effect
    DealDamage :: CharacterHandle -> Damage -> Effect
    Enchant :: MinionHandle -> [Enchantment] -> Effect
    GiveAbility :: MinionHandle -> [Ability] -> Effect
    GainManaCrystal :: CrystalState -> PlayerHandle -> Effect
    deriving (Typeable)


data All :: * where
    OtherCharacters :: CharacterHandle -> ([CharacterHandle] -> Effect) -> All
    OtherEnemies :: CharacterHandle -> ([CharacterHandle] -> Effect) -> All


data Unique :: * where
    CasterOf :: SpellHandle -> (PlayerHandle -> Effect) -> Unique
    OpponentOf :: PlayerHandle -> (PlayerHandle -> Effect) -> Unique
    ControllerOf :: MinionHandle -> (PlayerHandle -> Effect) -> Unique


data With :: * where
    All :: All -> With
    Unique :: Unique -> With


data KeywordEffect :: * where
    Silence :: MinionHandle -> KeywordEffect
    deriving (Show, Typeable)


data Ability :: * where
    KeywordAbility :: KeywordAbility -> Ability
    deriving (Typeable)


data KeywordAbility :: * where
    Battlecry :: (MinionHandle -> ElectionEffect Targeted) -> KeywordAbility
    Charge :: KeywordAbility
    DivineShield :: KeywordAbility
    Enrage :: [Ability] -> [Enchantment] -> KeywordAbility
    Taunt :: KeywordAbility
    deriving (Typeable)


data Enchantment :: * where
    StatsDelta :: Attack -> Health -> Enchantment
    --FrozenUntil :: Turn -> Enchantment
    deriving (Show, Eq, Ord, Data, Typeable)


type SpellEffect = SpellHandle -> ElectionEffect Targeted


data Spell = Spell {
    _spellCost :: Cost,
    _spellEffect :: SpellEffect,
    _spellName :: CardName
} deriving (Typeable)


data Minion = Minion {
    _minionCost :: Cost,
    _minionAttack :: Attack,
    _minionHealth :: Health,
    _minionAbilities :: [Ability],
    _minionName :: CardName
} deriving (Typeable)


data BoardMinion = BoardMinion {
    _boardMinionDamage :: Damage,
    _boardMinionEnchantments :: [Enchantment],
    _boardMinionAbilities :: [Ability],
    _boardMinionAttackCount :: Int,
    _boardMinionNewlySummoned :: Bool,
    _boardMinionHandle :: MinionHandle,
    _boardMinion :: Minion
} deriving (Typeable)


data DeckMinion = DeckMinion {
    _deckMinion :: Minion,
    _deckSpell :: Spell
} deriving (Typeable)


type HeroPowerEffect = PlayerHandle -> ElectionEffect Targeted


data HeroPower = HeroPower {
    _heroPowerCost :: Cost,
    _heroPowerEffect :: HeroPowerEffect
} deriving (Typeable)


data Hero = Hero {
    _heroAttack :: Attack,
    _heroHealth :: Health,
    _heroPower :: HeroPower,
    _heroName :: HeroName
} deriving (Typeable)


data BoardHero = BoardHero {
    _boardHeroDamage :: Damage,
    _boardHeroArmor :: Armor,
    _boardHeroAttackCount :: Int,
    _boardHero :: Hero
} deriving (Typeable)


data HandCard :: * where
    HandCardMinion ::Minion -> HandCard
    HandCardSpell :: Spell -> HandCard
    deriving (Typeable)


data DeckCard :: * where
    DeckCardMinion :: Minion -> DeckCard
    DeckCardSpell :: Spell -> DeckCard
    deriving (Typeable)


newtype Hand = Hand {
    _handCards :: [HandCard]
} deriving (Monoid, Generic, Typeable)


newtype Deck = Deck {
    _deckCards :: [DeckCard]
} deriving (Monoid, Generic, Typeable)


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


data GameState = GameState {
    _gameTurn :: Turn,
    _gameHandleSeed :: RawHandle,
    _gamePlayerTurnOrder :: [PlayerHandle],
    _gamePlayers :: [Player]
} deriving (Typeable)


data GameSnapshot = GameSnapshot {
    _snapshotGameState :: GameState
} deriving (Typeable)


data GameResult :: * where
    GameResult :: GameResult
    deriving (Show, Eq, Ord, Typeable)


-- Unfortunately I can't make the lenses alongside
-- their data declarations. See GHC ticket:
--   https://ghc.haskell.org/trac/ghc/ticket/10743
makeLenses ''Spell
makeLenses ''Minion
makeLenses ''BoardMinion
makeLenses ''DeckMinion
makeLenses ''HeroPower
makeLenses ''Hero
makeLenses ''BoardHero
makeLenses ''Hand
makeLenses ''Deck
makeLenses ''Player
makeLenses ''GameState
makeLenses ''GameSnapshot


deckCardName :: DeckCard -> CardName
deckCardName = \case
    DeckCardMinion minion -> minion^.minionName
    DeckCardSpell spell -> spell^.spellName


handCardName :: HandCard -> CardName
handCardName = \case
    HandCardMinion minion -> minion^.minionName
    HandCardSpell spell -> spell^.spellName



























































