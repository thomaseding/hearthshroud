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
data Minion deriving (Typeable)
data Player deriving (Typeable)
data Character deriving (Typeable)


type UserConstraint (c :: (* -> Constraint)) = (c Spell, c Minion, c Player, c Character)


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


class CastHandle a where
    castHandle :: Handle b -> Maybe (Handle a)


instance CastHandle Spell where
    castHandle = \case
        h @ SpellHandle {} -> Just h
        _ -> Nothing


instance CastHandle Character where
    castHandle = \case
        SpellHandle {} -> Nothing
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


type SpellHandle = Handle Spell
type MinionHandle = Handle Minion
type PlayerHandle = Handle Player
type CharacterHandle = Handle Character


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
    OwnerOf :: (c a) => Handle a -> (Handle Player -> Elect c s) -> Elect c s
    OpponentOf :: Handle Player -> (Handle Player -> Elect c s) -> Elect c s
    A :: A c s -> Elect c s
    All :: All c s -> Elect c s
    Effect :: Effect c -> Elect c s
    Choice :: [Elect c a] -> Elect c a
    deriving (Typeable)


data A :: (* -> Constraint) -> Selection -> * where
    Minion :: [Requirement Minion] -> (Handle Minion -> Elect c s) -> A c s
    Player :: [Requirement Player] -> (Handle Player -> Elect c s) -> A c s
    Character :: [Requirement Character] -> (Handle Character -> Elect c s) -> A c s


data All :: (* -> Constraint) -> Selection -> * where
    Minions :: [Requirement Minion] -> (HandleList Minion -> Elect c s) -> All c s
    Players :: [Requirement Player] -> (HandleList Player -> Elect c s) -> All c s
    Characters :: [Requirement Character] -> (HandleList Character -> Elect c s) -> All c s


data DamageSource :: * where
    Fatigue :: DamageSource
    DamagingCharacter :: Handle Character -> DamageSource
    DamagingSpell :: Handle Spell -> DamageSource


data BoardLocation :: * where
    RightOf :: Handle Minion -> BoardLocation
    Rightmost :: BoardLocation


data Effect :: (* -> Constraint) -> * where
    Elect :: Elect c AtRandom -> Effect c
    DoNothing :: Effect c
    Unreferenced :: (c a) => Handle a -> Effect c
    ForEach :: (c a) => HandleList a -> (Handle a -> Effect c) -> Effect c
    Sequence :: [Effect c] -> Effect c
    If :: Condition -> Effect c -> Effect c -> Effect c
    DrawCards :: Handle Player -> Int -> Effect c
    DealDamage :: Handle Character -> Damage -> DamageSource -> Effect c
    Enchant :: (c a) => Handle a -> AnyEnchantment c a -> Effect c
    GainManaCrystals :: Handle Player -> Int -> CrystalState -> Effect c
    DestroyMinion :: Handle Minion -> Effect c
    RestoreHealth :: Handle Character -> Health -> Effect c
    RestoreToFullHealth :: Handle Character -> Effect c
    Transform :: Handle Minion -> MinionCard c -> Effect c
    Silence :: Handle Minion -> Effect c
    GainArmor :: Handle Player -> Armor -> Effect c
    Freeze :: Handle Character -> Effect c
    Observing :: Effect c -> EventListener c -> Effect c
    PutInHand :: Handle Player -> Card c -> Effect c
    Summon :: Handle Player -> MinionCard c -> BoardLocation -> Effect c
    RandomMissiles :: [Requirement Character] -> Int -> Handle Spell -> Effect c
    DiscardAtRandom :: Handle Player -> Effect c
    TakeControl :: Handle Player -> Handle Minion -> Effect c
    deriving (Typeable)


data EventListener :: (* -> Constraint) -> * where
    SpellIsCast :: (Handle Spell -> Elect c AtRandom) -> EventListener c
    DamageIsDealt :: (Handle Character -> Damage -> DamageSource -> Elect c AtRandom) -> EventListener c
    HealthIsRestored :: (Handle Character -> Health -> Elect c AtRandom) -> EventListener c
    EndOfTurnEvent :: (Handle Player -> Elect c AtRandom) -> EventListener c


-- TODO: Need to adjust damage of minions when auras disappear (and also when they appear?)
data Aura :: (* -> Constraint) -> * where
    AuraOwnerOf :: (c a) => Handle a -> (Handle Player -> Aura c) -> Aura c
    AuraOpponentOf :: Handle Player -> (Handle Player -> Aura c) -> Aura c
    While :: (c a) => Handle a -> [Requirement a] -> Aura c -> Aura c
    EachMinion :: [Requirement Minion] -> (Handle Minion -> Aura c) -> Aura c
    Has :: Handle Minion -> Enchantment c Continuous Minion -> Aura c
    HasAbility :: Handle Minion -> Ability c Minion -> Aura c


data Ability :: (* -> Constraint) -> * -> * where
    Whenever :: (c a) => (Handle a -> EventListener c) -> Ability c a
    Aura :: (c a) => (Handle a -> Aura c) -> Ability c a
    Battlecry :: (c a) => (Handle a -> Elect c Targeted) -> Ability c a
    Deathrattle :: (c a) => (Handle a -> Elect c AtRandom) -> Ability c a
    Charge :: Ability c Minion
    DivineShield :: Ability c Minion
    Enrage :: [Ability c Minion] -> [Enchantment c Continuous Minion] -> Ability c Minion
    Taunt :: Ability c Minion
    SpellDamage :: Int -> Ability c a
    Windfury :: Ability c Minion
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
    MinionEnchantment :: Enchantment c t Character -> Enchantment c t Minion
    PlayerEnchantment :: Enchantment c t Character -> Enchantment c t Player
    Until :: TimePoint -> Enchantment c Continuous a -> Enchantment c Limited a
    DelayedEffect :: TimePoint -> Effect c -> Enchantment c Limited Minion
    StatsDelta :: Attack -> Health -> Enchantment c Continuous Character
    StatsScale :: Attack -> Health -> Enchantment c Continuous Minion
    ChangeStat :: Either Attack Health -> Enchantment c Continuous Minion
    SwapStats :: Enchantment c Continuous Minion
    Grant :: (c a) => Ability c a -> Enchantment c Continuous a
    Frozen :: Enchantment c Continuous Character
    deriving (Typeable)


data AnyEnchantment :: (* -> Constraint) -> * -> * where
    Continuous :: Enchantment c Continuous a -> AnyEnchantment c a
    Limited :: Enchantment c Limited a -> AnyEnchantment c a
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


type SpellEffect c = Handle Spell -> Elect c Targeted


data SpellCard c = SpellCard {
    _spellCost :: Cost,
    _spellEffect :: SpellEffect c,
    _spellMeta :: CardMeta
} deriving (Typeable)


data CastSpell c = CastSpell {
    _castSpellHandle :: Handle Spell,
    _castSpell :: SpellCard c
} deriving (Typeable)


data MinionCard c = MinionCard {
    _minionCost :: Cost,
    _minionTypes :: Set MinionType,
    _minionAttack :: Attack,
    _minionHealth :: Health,
    _minionAbilities :: [Ability c Minion],
    _minionMeta :: CardMeta
} deriving (Typeable)


data BoardMinion c = BoardMinion {
    _boardMinionDamage :: Damage,
    _boardMinionEnchantments :: [AnyEnchantment c Minion],
    _boardMinionAbilities :: [Ability c Minion],
    _boardMinionAttackCount :: Int,
    _boardMinionNewlySummoned :: Bool,
    _boardMinionPendingDestroy :: Bool,
    _boardMinionHandle :: Handle Minion,
    _boardMinion :: MinionCard c
} deriving (Typeable)


type HeroPowerEffect c = Handle Player -> Elect c Targeted


data HeroPower c = HeroPower {
    _heroPowerCost :: Cost,
    _heroPowerEffect :: HeroPowerEffect c,
    _heroPowerName :: HeroPowerName
} deriving (Typeable)


data Hero c = Hero {
    _heroAttack :: Attack,
    _heroHealth :: Health,
    _heroPower :: HeroPower c,
    _heroName :: HeroName
} deriving (Typeable)


data BoardHero c = BoardHero {
    _boardHeroDamage :: Damage,
    _boardHeroArmor :: Armor,
    _boardHeroAttackCount :: Int,
    _boardHeroPower :: HeroPower c,
    _boardHeroPowerCount :: Int,
    _boardHero :: Hero c
} deriving (Typeable)


data HandCard :: (* -> Constraint) -> * where
    HandCardMinion :: MinionCard c -> HandCard c
    HandCardSpell :: SpellCard c -> HandCard c
    deriving (Typeable)


data DeckCard :: (* -> Constraint) -> * where
    DeckCardMinion :: MinionCard c -> DeckCard c
    DeckCardSpell :: SpellCard c -> DeckCard c
    deriving (Typeable)


data Card :: (* -> Constraint) -> * where
    CardMinion :: MinionCard c -> Card c
    CardSpell :: SpellCard c -> Card c
    deriving (Typeable)


newtype Hand c = Hand {
    _handCards :: [HandCard c]
} deriving (Monoid, Generic, Typeable)


newtype Deck c = Deck {
    _deckCards :: [DeckCard c]
} deriving (Monoid, Generic, Typeable)


data PlayerObject c = PlayerObject {
    _playerHandle :: Handle Player,
    _playerDeck :: Deck c,
    _playerExcessDrawCount :: Int,
    _playerHand :: Hand c,
    _playerMinions :: [BoardMinion c],
    _playerSpells :: [CastSpell c],
    _playerEnchantments :: [AnyEnchantment c Player],
    _playerTotalManaCrystals :: Int,
    _playerEmptyManaCrystals :: Int,
    _playerTemporaryManaCrystals :: Int,
    _playerHero :: BoardHero c
} deriving (Typeable)


data GameState (c :: * -> Constraint) = GameState {
    _gameTurn :: Turn,
    _gameHandleSeed :: Int,
    _gamePlayerTurnOrder :: [Handle Player],
    _gameEffectObservers :: [EventListener c],
    _gamePlayers :: [PlayerObject c]
} deriving (Typeable)


data GameSnapshot c = GameSnapshot {
    _snapshotGameState :: GameState c
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




























































