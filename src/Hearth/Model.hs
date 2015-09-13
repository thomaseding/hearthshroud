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
{-# LANGUAGE PatternSynonyms #-}
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
import Data.Set (Set)
import GHC.Generics
import GHC.Prim (Constraint)
import Hearth.CardName
import Hearth.HeroName
import Hearth.HeroPowerName


--------------------------------------------------------------------------------


data Result = Success | Failure String
    deriving (Show, Eq, Ord, Data, Typeable)


newtype Turn = Turn Int
    deriving (Show, Eq, Ord, Data, Typeable)


newtype BoardIndex = BoardIndex Int
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


newtype Durability = Durability { unDurability :: Int }
    deriving (Show, Eq, Ord, Data, Typeable, Enum, Num, Real, Integral)


pattern MaxHandSize = 10
pattern MaxManaCrystals = 10
pattern MaxBoardMinionsPerPlayer = 7


data RawHandle :: * where
    RawHandle :: (Typeable userData) => userData -> Int -> RawHandle
    deriving (Typeable)


instance Show RawHandle where
    show (RawHandle _ x) = show x


instance Eq RawHandle where
    (RawHandle _ x) == (RawHandle _ y) = x == y


instance Ord RawHandle where
    (RawHandle _ x) <= (RawHandle _ y) = x <= y


data Spell deriving (Typeable)
data Weapon deriving (Typeable)
data Minion deriving (Typeable)
data Player deriving (Typeable)
data Character deriving (Typeable)


type UserConstraint (k :: (* -> Constraint)) = (k Spell, k Minion, k Player, k Character)


data Handle :: * -> * where
    SpellHandle :: RawHandle -> Handle Spell
    WeaponHandle :: RawHandle -> Handle Weapon
    MinionHandle :: RawHandle -> Handle Minion
    PlayerHandle :: RawHandle -> Handle Player
    MinionCharacter :: Handle Minion -> Handle Character
    PlayerCharacter :: Handle Player -> Handle Character
    deriving (Typeable)


mapHandle :: (Handle Spell -> b) -> (Handle Weapon -> b) -> (Handle Minion -> b) -> (Handle Player -> b) -> (Handle Character -> b) -> (Handle a -> b)
mapHandle spell weapon minion player character = \case
    h @ SpellHandle {} -> spell h
    h @ WeaponHandle {} -> weapon h
    h @ MinionHandle {} -> minion h
    h @ PlayerHandle {} -> player h
    h @ MinionCharacter {} -> character h
    h @ PlayerCharacter {} -> character h


applyRawHandle :: (RawHandle -> b) -> Handle a -> b
applyRawHandle f = \case
    SpellHandle h -> f h
    WeaponHandle h -> f h
    MinionHandle h -> f h
    PlayerHandle h -> f h
    MinionCharacter h -> applyRawHandle f h
    PlayerCharacter h -> applyRawHandle f h


class CastHandle a where
    castHandle :: Handle b -> Maybe (Handle a)


instance CastHandle Spell where
    castHandle = \case
        h @ SpellHandle {} -> Just h
        _ -> Nothing


instance CastHandle Character where
    castHandle = \case
        SpellHandle {} -> Nothing
        WeaponHandle {} -> Nothing
        h @ MinionHandle {} -> Just $ MinionCharacter h
        h @ PlayerHandle {} -> Just $ PlayerCharacter h
        h @ MinionCharacter {} -> Just h
        h @ PlayerCharacter {} -> Just h


instance Show (Handle a) where
    show = applyRawHandle show


instance Eq (Handle a) where
    (==) = on (==) $ applyRawHandle id


instance Ord (Handle a) where
    (<=) = on (<=) $ applyRawHandle id


data HandleList :: * -> * where
    HandleList :: (Typeable userData) => userData -> [Handle a] -> HandleList a


handleList :: [Handle a] -> HandleList a
handleList = HandleList ()


class HasUserData a where
    getUserData :: (Typeable u) => a -> Maybe u
    setUserData :: (Typeable u) => a -> u -> a


instance HasUserData RawHandle where
    getUserData (RawHandle u _) = cast u
    setUserData (RawHandle _ n) u = RawHandle u n


instance HasUserData (Handle a) where
    getUserData = applyRawHandle getUserData
    setUserData = \case
        SpellHandle h -> SpellHandle . setUserData h
        WeaponHandle h -> WeaponHandle . setUserData h
        MinionHandle h -> MinionHandle . setUserData h
        PlayerHandle h -> PlayerHandle . setUserData h
        MinionCharacter h -> MinionCharacter . setUserData h
        PlayerCharacter h -> PlayerCharacter . setUserData h


instance HasUserData (HandleList a) where
    getUserData (HandleList u _) = cast u
    setUserData (HandleList _ hs) u = HandleList u hs


data CrystalState :: * where
    CrystalFull :: CrystalState
    CrystalEmpty :: CrystalState
    CrystalTemporary :: CrystalState
    deriving (Show, Eq, Ord, Data, Typeable)


data Cost :: * where
    ManaCost :: Mana -> Cost
    deriving (Show, Eq, Ord, Data, Typeable)


data Selection = Targeted | AtRandom


infixr 2 `Or`
infixr 3 `And`
infixr 5 `Satisfies`

data Condition :: * where
    Or :: Condition -> Condition -> Condition
    And :: Condition -> Condition -> Condition
    Satisfies :: Handle a -> [Requirement a] -> Condition


data Requirement :: * -> * where
    RequireMinion :: Requirement Character -> Requirement Minion
    RequirePlayer :: Requirement Character -> Requirement Player
    OwnedBy :: Handle Player -> Requirement a
    Is :: Handle a -> Requirement a
    Not :: Handle a -> Requirement a
    IsDamageSource :: DamageSource -> Requirement a
    WithAttack :: Comparison -> Attack -> Requirement Character
    WithHealth :: Comparison -> Health -> Requirement Character
    Damaged :: Requirement Character
    Undamaged :: Requirement Character
    IsMinion :: Requirement Character
    AdjacentTo :: Handle Minion -> Requirement Minion
    HasMaxManaCrystals :: Requirement Player
    HasType :: MinionType -> Requirement Minion
    HasMinion :: [Requirement Minion] -> Requirement Player


data Comparison
    = Less
    | LessEqual
    | Equal
    | GreaterEqual
    | Greater
    deriving (Show, Eq, Ord)


data Elect :: (* -> Constraint) -> Selection -> * where
    OwnerOf :: (k a) => Handle a -> (Handle Player -> Elect k s) -> Elect k s
    OpponentOf :: Handle Player -> (Handle Player -> Elect k s) -> Elect k s
    A :: A k s -> Elect k s
    All :: All k s -> Elect k s
    Effect :: Effect k -> Elect k s
    Choice :: [Elect k a] -> Elect k a
    deriving (Typeable)


data A :: (* -> Constraint) -> Selection -> * where
    Minion :: [Requirement Minion] -> (Handle Minion -> Elect k s) -> A k s
    Player :: [Requirement Player] -> (Handle Player -> Elect k s) -> A k s
    Character :: [Requirement Character] -> (Handle Character -> Elect k s) -> A k s


data All :: (* -> Constraint) -> Selection -> * where
    Minions :: [Requirement Minion] -> (HandleList Minion -> Elect k s) -> All k s
    Players :: [Requirement Player] -> (HandleList Player -> Elect k s) -> All k s
    Characters :: [Requirement Character] -> (HandleList Character -> Elect k s) -> All k s


data DamageSource :: * where
    Fatigue :: DamageSource
    DamagingCharacter :: Handle Character -> DamageSource
    DamagingSpell :: Handle Spell -> DamageSource


data BoardLocation :: * where
    RightOf :: Handle Minion -> BoardLocation
    Rightmost :: BoardLocation


data Effect :: (* -> Constraint) -> * where
    Elect :: Elect k AtRandom -> Effect k
    DoNothing :: Effect k
    Unreferenced :: (k a) => Handle a -> Effect k
    ForEach :: (k a) => HandleList a -> (Handle a -> Effect k) -> Effect k
    Sequence :: [Effect k] -> Effect k
    If :: Condition -> Effect k -> Effect k -> Effect k
    DrawCards :: Handle Player -> Int -> Effect k
    DealDamage :: Handle Character -> Damage -> DamageSource -> Effect k
    Enchant :: (k a) => Handle a -> AnyEnchantment k a -> Effect k
    GainManaCrystals :: Handle Player -> Int -> CrystalState -> Effect k
    DestroyMinion :: Handle Minion -> Effect k
    RestoreHealth :: Handle Character -> Health -> Effect k
    RestoreToFullHealth :: Handle Character -> Effect k
    Transform :: Handle Minion -> MinionCard k -> Effect k
    Silence :: Handle Minion -> Effect k
    GainArmor :: Handle Player -> Armor -> Effect k
    Freeze :: Handle Character -> Effect k
    Observing :: Effect k -> EventListener k -> Effect k
    PutInHand :: Handle Player -> Card k -> Effect k
    Summon :: Handle Player -> MinionCard k -> BoardLocation -> Effect k
    RandomMissiles :: [Requirement Character] -> Int -> Handle Spell -> Effect k
    DiscardAtRandom :: Handle Player -> Effect k
    TakeControl :: Handle Player -> Handle Minion -> Effect k
    deriving (Typeable)


data EventListener :: (* -> Constraint) -> * where
    SpellIsCast :: (Handle Spell -> Elect k AtRandom) -> EventListener k
    DamageIsDealt :: (Handle Character -> Damage -> DamageSource -> Elect k AtRandom) -> EventListener k
    HealthIsRestored :: (Handle Character -> Health -> Elect k AtRandom) -> EventListener k
    EndOfTurnEvent :: (Handle Player -> Elect k AtRandom) -> EventListener k


-- TODO: Need to adjust damage of minions when auras disappear (and also when they appear?)
data Aura :: (* -> Constraint) -> * where
    AuraOwnerOf :: (k a) => Handle a -> (Handle Player -> Aura k) -> Aura k
    AuraOpponentOf :: Handle Player -> (Handle Player -> Aura k) -> Aura k
    While :: (k a) => Handle a -> [Requirement a] -> Aura k -> Aura k
    EachMinion :: [Requirement Minion] -> (Handle Minion -> Aura k) -> Aura k
    Has :: Handle Minion -> Enchantment k Continuous Minion -> Aura k
    HasAbility :: Handle Minion -> Ability k Minion -> Aura k


data Ability :: (* -> Constraint) -> * -> * where
    Whenever :: (k a) => (Handle a -> EventListener k) -> Ability k a
    Aura :: (k a) => (Handle a -> Aura k) -> Ability k a
    Battlecry :: (k a) => (Handle a -> Elect k Targeted) -> Ability k a
    Deathrattle :: (k a) => (Handle a -> Elect k AtRandom) -> Ability k a
    Charge :: Ability k Minion
    DivineShield :: Ability k Minion
    Enrage :: [Ability k Minion] -> [Enchantment k Continuous Minion] -> Ability k Minion
    Taunt :: Ability k Minion
    SpellDamage :: Int -> Ability k a
    Windfury :: Ability k Minion
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


data Enchantment :: (* -> Constraint) -> * -> * -> * where
    MinionEnchantment :: Enchantment k t Character -> Enchantment k t Minion
    PlayerEnchantment :: Enchantment k t Character -> Enchantment k t Player
    Until :: TimePoint -> Enchantment k Continuous a -> Enchantment k Limited a
    DelayedEffect :: TimePoint -> Effect k -> Enchantment k Limited Minion
    StatsDelta :: Attack -> Health -> Enchantment k Continuous Character
    StatsScale :: Attack -> Health -> Enchantment k Continuous Minion
    ChangeStat :: Either Attack Health -> Enchantment k Continuous Minion
    SwapStats :: Enchantment k Continuous Minion
    Grant :: (k a) => Ability k a -> Enchantment k Continuous a
    Frozen :: Enchantment k Continuous Character
    deriving (Typeable)


data AnyEnchantment :: (* -> Constraint) -> * -> * where
    Continuous :: Enchantment k Continuous a -> AnyEnchantment k a
    Limited :: Enchantment k Limited a -> AnyEnchantment k a
    deriving (Typeable)


data MinionType :: * where
    Beast :: MinionType
    Demon :: MinionType
    Dragon :: MinionType
    Mech :: MinionType
    Murloc :: MinionType
    Pirate :: MinionType
    Totem :: MinionType
    deriving (Show, Eq, Ord)


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


type SpellEffect k = Handle Spell -> Elect k Targeted


data SpellCard k = SpellCard {
    _spellCost :: Cost,
    _spellEffect :: SpellEffect k,
    _spellMeta :: CardMeta
} deriving (Typeable)


data CastSpell k = CastSpell {
    _castSpellHandle :: Handle Spell,
    _castSpell :: SpellCard k
} deriving (Typeable)


data WeaponCard k = WeaponCard {
    _weaponCost :: Cost,
    _weaponAttack :: Attack,
    _weaponDurability :: Durability,
    _weaponAbilities :: [Ability k Weapon],
    _weaponMeta :: CardMeta
} deriving (Typeable)


data BoardWeapon k = BoardWeapon {
    _boardWeaponDurability :: Durability,
    _boardWeaponEnchantments :: [AnyEnchantment k Weapon],
    _boardWeaponAbilities :: [Ability k Weapon],
    _boardWeaponHandle :: Handle Weapon,
    _boardWeapon :: WeaponCard k
} deriving (Typeable)


data MinionCard k = MinionCard {
    _minionCost :: Cost,
    _minionTypes :: Set MinionType,
    _minionAttack :: Attack,
    _minionHealth :: Health,
    _minionAbilities :: [Ability k Minion],
    _minionMeta :: CardMeta
} deriving (Typeable)


data BoardMinion k = BoardMinion {
    _boardMinionDamage :: Damage,
    _boardMinionEnchantments :: [AnyEnchantment k Minion],
    _boardMinionAbilities :: [Ability k Minion],
    _boardMinionAttackCount :: Int,
    _boardMinionNewlySummoned :: Bool,
    _boardMinionPendingDestroy :: Bool,
    _boardMinionHandle :: Handle Minion,
    _boardMinion :: MinionCard k
} deriving (Typeable)


type HeroPowerEffect k = Handle Player -> Elect k Targeted


data HeroPower k = HeroPower {
    _heroPowerCost :: Cost,
    _heroPowerEffect :: HeroPowerEffect k,
    _heroPowerName :: HeroPowerName
} deriving (Typeable)


data Hero k = Hero {
    _heroAttack :: Attack,
    _heroHealth :: Health,
    _heroPower :: HeroPower k,
    _heroName :: HeroName
} deriving (Typeable)


data BoardHero k = BoardHero {
    _boardHeroDamage :: Damage,
    _boardHeroArmor :: Armor,
    _boardHeroAttackCount :: Int,
    _boardHeroPower :: HeroPower k,
    _boardHeroPowerCount :: Int,
    _boardHero :: Hero k
} deriving (Typeable)


data HandCard :: (* -> Constraint) -> * where
    HandCardMinion :: MinionCard k -> HandCard k
    HandCardSpell :: SpellCard k -> HandCard k
    HandCardWeapon :: WeaponCard k -> HandCard k
    deriving (Typeable)


data DeckCard :: (* -> Constraint) -> * where
    DeckCardMinion :: MinionCard k -> DeckCard k
    DeckCardSpell :: SpellCard k -> DeckCard k
    DeckCardWeapon :: WeaponCard k -> DeckCard k
    deriving (Typeable)


data Card :: (* -> Constraint) -> * where
    CardMinion :: MinionCard k -> Card k
    CardSpell :: SpellCard k -> Card k
    CardWeapon :: WeaponCard k -> Card k
    deriving (Typeable)


newtype Hand k = Hand {
    _handCards :: [HandCard k]
} deriving (Monoid, Generic, Typeable)


newtype Deck k = Deck {
    _deckCards :: [DeckCard k]
} deriving (Monoid, Generic, Typeable)


data PlayerObject k = PlayerObject {
    _playerHandle :: Handle Player,
    _playerDeck :: Deck k,
    _playerExcessDrawCount :: Int,
    _playerHand :: Hand k,
    _playerWeapon :: Maybe (BoardWeapon k),
    _playerMinions :: [BoardMinion k],
    _playerSpells :: [CastSpell k],
    _playerEnchantments :: [AnyEnchantment k Player],
    _playerTotalManaCrystals :: Int,
    _playerEmptyManaCrystals :: Int,
    _playerTemporaryManaCrystals :: Int,
    _playerHero :: BoardHero k
} deriving (Typeable)


data GameState (k :: * -> Constraint) = GameState {
    _gameTurn :: Turn,
    _gameHandleSeed :: Int,
    _gamePlayerTurnOrder :: [Handle Player],
    _gameEffectObservers :: [EventListener k],
    _gamePlayers :: [PlayerObject k]
} deriving (Typeable)


data GameSnapshot k = GameSnapshot {
    _snapshotGameState :: GameState k
} deriving (Typeable)


data GameResult :: * where
    GameResult :: GameResult
    deriving (Show, Eq, Ord, Typeable)


-- Unfortunately I can't make the lenses alongside
-- their data declarations. See GHC ticket:
--   https://ghc.haskell.org/trac/ghc/ticket/10743
makeLenses ''CardMeta
makeLenses ''SpellCard
makeLenses ''CastSpell
makeLenses ''WeaponCard
makeLenses ''BoardWeapon
makeLenses ''MinionCard
makeLenses ''BoardMinion
makeLenses ''HeroPower
makeLenses ''Hero
makeLenses ''BoardHero
makeLenses ''Hand
makeLenses ''Deck
makeLenses ''PlayerObject
makeLenses ''GameState
makeLenses ''GameSnapshot




























































