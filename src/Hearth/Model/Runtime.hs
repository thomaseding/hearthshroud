{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}


module Hearth.Model.Runtime where


--------------------------------------------------------------------------------


import Control.Lens
import Data.Data
import GHC.Generics -- XXX: Is this different than Data.Generics?
import Hearth.Model.Authoring


--------------------------------------------------------------------------------


pattern MaxHandSize :: Int
pattern MaxHandSize = 10


pattern MaxManaCrystals :: Int
pattern MaxManaCrystals = 10


pattern MaxBoardMinionsPerPlayer :: Int
pattern MaxBoardMinionsPerPlayer = 7


--------------------------------------------------------------------------------


data Scoped :: * -> * where
    Begin :: a -> Scoped a
    End :: a -> Scoped a
    deriving (Eq, Ord)


data Phase :: * where
    BeginTurnPhase :: Phase
    EndTurnPhase :: Phase
    BattlecryPhase :: Phase
    DeathrattlePhase :: Phase
    ChooseOnePhase :: Phase
    SpellPhase :: Phase
    HeroPowerPhase :: Phase
    AttackResolutionPhase :: Phase
    TriggeredEffectPhase :: Phase
    deriving (Show, Typeable)


data Universe :: * where
    Universe :: [Card] -> Universe


unUniverse :: Universe -> [Card]
unUniverse (Universe u) = u


data CastSpell = CastSpell {
    _castSpellHandle :: Handle 'Spell',
    _castSpell :: SpellCard
} deriving (Typeable)


data BoardWeapon = BoardWeapon {
    _boardWeaponDurability :: Durability,
    _boardWeaponEnchantments :: [AnyEnchantment 'Weapon'],
    _boardWeaponAbilities :: [Ability 'Weapon'],
    _boardWeaponHandle :: Handle 'Weapon',
    _boardWeapon :: WeaponCard
} deriving (Typeable)


data BoardMinion = BoardMinion {
    _boardMinionDamage :: Damage,
    _boardMinionEnchantments :: [AnyEnchantment 'Minion'],
    _boardMinionAbilities :: [Ability 'Minion'],
    _boardMinionAttackCount :: Int,
    _boardMinionNewlySummoned :: Bool,
    _boardMinionPendingDestroy :: Bool,
    _boardMinionHandle :: Handle 'Minion',
    _boardMinion :: MinionCard
} deriving (Typeable)



data BoardHero = BoardHero {
    _boardHeroDamage :: Damage,
    _boardHeroArmor :: Armor,
    _boardHeroAttackCount :: Int,
    _boardHeroPower :: HeroPower,
    _boardHeroPowerCount :: Int,
    _boardHero :: Hero
} deriving (Typeable)


data HandCard :: * where
    HandCardMinion :: MinionCard -> HandCard
    HandCardSpell :: SpellCard -> HandCard
    HandCardWeapon :: WeaponCard -> HandCard
    deriving (Typeable)


data DeckCard :: * where
    DeckCardMinion :: MinionCard -> DeckCard
    DeckCardSpell :: SpellCard -> DeckCard
    DeckCardWeapon :: WeaponCard -> DeckCard
    deriving (Typeable)


newtype Hand = Hand {
    _handCards :: [HandCard]
} deriving (Monoid, Generic, Typeable)


newtype Deck = Deck {
    _deckCards :: [DeckCard]
} deriving (Monoid, Generic, Typeable)


data PlayerObject = PlayerObject {
    _playerHandle :: Handle 'Player',
    _playerDeck :: Deck,
    _playerExcessDrawCount :: Int,
    _playerHand :: Hand,
    _playerWeapon :: Maybe (BoardWeapon),
    _playerMinions :: [BoardMinion],
    _playerSpells :: [CastSpell],
    _playerEnchantments :: [AnyEnchantment 'Player'],
    _playerTotalManaCrystals :: Int,
    _playerEmptyManaCrystals :: Int,
    _playerTemporaryManaCrystals :: Int,
    _playerHero :: BoardHero
} deriving (Typeable)


data GameState = GameState {
    _gameUniverse :: Universe,
    _gameTurn :: Turn,
    _gameHandleSeed :: Int,
    _gamePlayerTurnOrder :: [Handle 'Player'],
    _gameEffectObservers :: [EventListener],
    _gameRootMinion :: Maybe (Handle 'Minion'), -- Used to disable targeting the battlecry/choose-one minion.
    _gamePlayers :: [PlayerObject]
} deriving (Typeable)


data GameSnapshot = GameSnapshot {
    _snapshotGameState :: GameState
} deriving (Typeable)


data GameResult :: * where
    GameResult :: GameResult
    deriving (Show, Eq, Ord, Typeable)


--------------------------------------------------------------------------------


-- Unfortunately I can't make the lenses alongside
-- their data declarations. See GHC ticket:
--   https://ghc.haskell.org/trac/ghc/ticket/10743
makeLenses ''CastSpell
makeLenses ''BoardWeapon
makeLenses ''BoardMinion
makeLenses ''BoardHero
makeLenses ''Hand
makeLenses ''Deck
makeLenses ''PlayerObject
makeLenses ''GameState
makeLenses ''GameSnapshot





