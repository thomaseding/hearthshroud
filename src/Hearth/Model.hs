{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
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
import GHC.Generics
import Hearth.CardName
import Hearth.HeroName
import Hearth.HeroPowerName


--------------------------------------------------------------------------------


data Result = Success | Failure String
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
    MinionCharacter :: Handle Minion -> Handle Character
    PlayerCharacter :: Handle Player -> Handle Character
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


newtype HandleList a = HandleList [Handle a]


data CrystalState :: * where
    CrystalFull :: CrystalState
    CrystalEmpty :: CrystalState
    CrystalTemporary :: CrystalState
    deriving (Show, Eq, Ord, Data, Typeable)


data Cost :: * where
    ManaCost :: Mana -> Cost
    deriving (Show, Eq, Ord, Data, Typeable)


data Selection = Targeted | AtRandom


data Restriction :: * -> * where
    RestrictMinion :: Restriction Character -> Restriction Minion
    RestrictPlayer :: Restriction Character -> Restriction Player
    OwnedBy :: Handle Player -> Restriction a
    Is :: Handle a -> Restriction a
    Not :: Handle a -> Restriction a
    IsDamageSource :: DamageSource -> Restriction Character
    WithAttack :: Comparison -> Attack -> Restriction Character
    WithHealth :: Comparison -> Health -> Restriction Character
    Damaged :: Restriction Character
    Undamaged :: Restriction Character
    IsMinion :: Restriction Character
    AdjacentTo :: Handle Minion -> Restriction Minion


data Comparison
    = Less
    | LessEqual
    | Equal
    | GreaterEqual
    | Greater
    deriving (Show, Eq, Ord)


data Elect :: Selection -> * where
    OwnerOf :: Handle a -> (Handle Player -> Elect s) -> Elect s
    OpponentOf :: Handle Player -> (Handle Player -> Elect s) -> Elect s
    A :: A s -> Elect s
    All :: All s -> Elect s
    Effect :: Effect -> Elect s
    Choice :: [Elect Targeted] -> Elect Targeted
    deriving (Typeable)


data A :: Selection -> * where
    Minion :: [Restriction Minion] -> (Handle Minion -> Elect s) -> A s
    Player :: [Restriction Player] -> (Handle Player -> Elect s) -> A s
    Character :: [Restriction Character] -> (Handle Character -> Elect s) -> A s


data All :: Selection -> * where
    Minions :: [Restriction Minion] -> (HandleList Minion -> Elect s) -> All s
    Players :: [Restriction Player] -> (HandleList Player -> Elect s) -> All s
    Characters :: [Restriction Character] -> (HandleList Character -> Elect s) -> All s


data DamageSource :: * where
    Fatigue :: DamageSource
    DamagingCharacter :: Handle Character -> DamageSource
    DamagingSpell :: Handle Spell -> DamageSource


data Effect :: * where
    Elect :: Elect AtRandom -> Effect
    DoNothing :: Effect
    Unreferenced :: Handle a -> Effect
    ForEach :: HandleList a -> ((Handle a) -> Effect) -> Effect
    Sequence :: [Effect] -> Effect
    If :: Condition -> Effect -> Effect -> Effect
    DrawCards :: Handle Player -> Int -> Effect
    DealDamage :: Handle Character -> Damage -> DamageSource -> Effect
    Enchant :: Handle Minion -> AnyEnchantment -> Effect
    GrantAbilities :: Handle Minion -> [Ability] -> Effect
    GainManaCrystals :: Handle Player -> Int -> CrystalState -> Effect
    DestroyMinion :: Handle Minion -> Effect
    RestoreHealth :: Handle Character -> Health -> Effect
    Transform :: Handle Minion -> Minion -> Effect
    Silence :: Handle Minion -> Effect
    GainArmor :: Handle Player -> Armor -> Effect
    Freeze :: Handle Character -> Effect
    Observing :: Effect -> EventListener -> Effect
    deriving (Typeable)


data EventListener :: * where
    SpellIsCast :: (Handle Spell -> Elect AtRandom) -> EventListener
    DamageIsDealt :: (Handle Character -> Damage -> DamageSource -> Elect AtRandom) -> EventListener


data Condition :: * where
    Satisfies :: Handle a -> [Restriction a] -> Condition


-- TODO: Need to adjust damage of minions when auras disappear (and also when they appear?)
data Aura :: * where
    AuraOwnerOf :: Handle a -> (Handle Player -> Aura) -> Aura
    AuraOpponentOf :: Handle Player -> (Handle Player -> Aura) -> Aura
    While :: Handle a -> [Restriction a] -> Aura -> Aura
    EachMinion :: [Restriction Minion] -> (Handle Minion -> Aura) -> Aura
    Has :: Handle Minion -> Enchantment Continuous -> Aura


data Ability :: * where
    Whenever :: (Handle Minion -> EventListener) -> Ability
    Aura :: (Handle Minion -> Aura) -> Ability
    Battlecry :: (Handle Minion -> Elect Targeted) -> Ability
    Deathrattle :: (Handle Minion -> Elect AtRandom) -> Ability
    Charge :: Ability
    DivineShield :: Ability
    Enrage :: [Ability] -> [Enchantment Continuous] -> Ability
    Taunt :: Ability
    deriving (Typeable)


data Continuous
data Limited


data Scoped :: * -> * where
    Begin :: a -> Scoped a
    End :: a -> Scoped a
    deriving (Eq, Ord)


data Phase :: * where
    BeginTurnPhase :: Phase
    EndTurnPhase :: Phase
    BattlecryPhase :: Phase
    DeathrattlePhase :: Phase
    SpellPhase :: Phase
    HeroPowerPhase :: Phase
    AttackResolutionPhase :: Phase
    TriggeredEffectPhase :: Phase
    deriving (Show, Typeable)


data TimePoint :: * where
    Delay :: Int -> TimePoint -> TimePoint
    BeginOfTurn :: TimePoint
    EndOfTurn :: TimePoint
    deriving (Show, Typeable, Eq, Ord)


data Enchantment :: * -> * where
    Until :: TimePoint -> Enchantment Continuous -> Enchantment Limited
    StatsDelta :: Attack -> Health -> Enchantment Continuous
    StatsScale :: Attack -> Health -> Enchantment Continuous
    ChangeStat :: Either Attack Health -> Enchantment Continuous
    SwapStats :: Enchantment Continuous
    Frozen :: Enchantment Continuous
    deriving (Typeable)


deriving instance Eq (Enchantment a)


data AnyEnchantment :: * where
    Continuous :: Enchantment Continuous -> AnyEnchantment
    Limited :: Enchantment Limited -> AnyEnchantment
    deriving (Typeable)


data Rarity :: * where
    Free :: Rarity
    Common :: Rarity
    Rare :: Rarity
    Epic :: Rarity
    Legendary :: Rarity
    deriving (Show, Eq, Ord)


data Class :: * where
    Neutral :: Class
    Paladin :: Class
    Warrior :: Class
    Shaman :: Class
    Druid :: Class
    Hunter :: Class
    Priest :: Class
    Mage :: Class
    Warlock :: Class
    Rogue :: Class
    deriving (Show, Eq, Ord)


data Collectibility :: * where
    Collectible :: Collectibility
    Uncollectible :: Collectibility
    deriving (Show, Eq, Ord)


data CardMeta = CardMeta {
    _cardMetaName :: CardName,
    _cardMetaClass :: Class,
    _cardMetaRarity :: Rarity,
    _cardMetaCollectibility :: Collectibility }
    deriving (Typeable)


type SpellEffect = Handle Spell -> Elect Targeted


data Spell = Spell' {
    _spellCost :: Cost,
    _spellEffect :: SpellEffect,
    _spellMeta :: CardMeta
} deriving (Typeable)


data CastSpell = CastSpell {
    _castSpellHandle :: Handle Spell,
    _castSpell :: Spell
} deriving (Typeable)


data Minion = Minion' {
    _minionCost :: Cost,
    _minionAttack :: Attack,
    _minionHealth :: Health,
    _minionAbilities :: [Ability],
    _minionMeta :: CardMeta
} deriving (Typeable)


data BoardMinion = BoardMinion {
    _boardMinionDamage :: Damage,
    _boardMinionEnchantments :: [AnyEnchantment],
    _boardMinionAbilities :: [Ability],
    _boardMinionAttackCount :: Int,
    _boardMinionNewlySummoned :: Bool,
    _boardMinionPendingDestroy :: Bool,
    _boardMinionHandle :: Handle Minion,
    _boardMinion :: Minion
} deriving (Typeable)


data DeckMinion = DeckMinion {
    _deckMinion :: Minion,
    _deckSpell :: Spell
} deriving (Typeable)


type HeroPowerEffect = Handle Player -> Elect Targeted


data HeroPower = HeroPower {
    _heroPowerCost :: Cost,
    _heroPowerEffect :: HeroPowerEffect,
    _heroPowerName :: HeroPowerName
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
    _boardHeroPower :: HeroPower,
    _boardHeroPowerCount :: Int,
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


data Player = Player' {
    _playerHandle :: Handle Player,
    _playerDeck :: Deck,
    _playerExcessDrawCount :: Int,
    _playerHand :: Hand,
    _playerMinions :: [BoardMinion],
    _playerSpells :: [CastSpell],
    _playerTotalManaCrystals :: Int,
    _playerEmptyManaCrystals :: Int,
    _playerTemporaryManaCrystals :: Int,
    _playerHero :: BoardHero
} deriving (Typeable)


data GameState = GameState {
    _gameTurn :: Turn,
    _gameHandleSeed :: RawHandle,
    _gamePlayerTurnOrder :: [Handle Player],
    _gameEffectObservers :: [EventListener],
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
makeLenses ''CardMeta
makeLenses ''Spell
makeLenses ''CastSpell
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
    DeckCardMinion minion -> minion^.minionMeta.cardMetaName
    DeckCardSpell spell -> spell^.spellMeta.cardMetaName


handCardName :: HandCard -> CardName
handCardName = \case
    HandCardMinion minion -> minion^.minionMeta.cardMetaName
    HandCardSpell spell -> spell^.spellMeta.cardMetaName



























































