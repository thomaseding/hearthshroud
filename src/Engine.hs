{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}


module Engine where


--------------------------------------------------------------------------------


import Control.Applicative
import Control.Monad.Prompt
import Control.Monad.Random
import Control.Monad.State
import Names


--------------------------------------------------------------------------------


newtype Turn = Turn Int
    deriving (Show, Eq, Ord, Enum, Num, Real, Integral)


newtype Cost = Cost Int
    deriving (Show, Eq, Ord, Enum, Num, Real, Integral)


newtype Attack = Attack Int
    deriving (Show, Eq, Ord, Enum, Num, Real, Integral)


newtype Armor = Armor Int
    deriving (Show, Eq, Ord, Enum, Num, Real, Integral)


newtype Health = Health Int
    deriving (Show, Eq, Ord, Enum, Num, Real, Integral)


data Effect :: * where
    Effect :: Effect
    --WithElects :: [Elect] -> [Effect] -> Effect
    deriving (Show, Eq, Ord)


data Ability :: * where
    Charge :: Ability
    deriving (Show, Eq, Ord)


data Enchantment :: * where
    FrozenUntil :: Turn -> Enchantment
    deriving (Show, Eq, Ord)


data Spell = Spell {
    --_spellEffects :: [SpellEffect],
    _spellName :: CardName
} deriving (Show, Eq, Ord)


data Minion = Minion {
    _minionAttack :: Attack,
    _minionHealth :: Health,
    _minionName :: CardName
} deriving (Show, Eq, Ord)


data BoardMinion = BoardMinion {
    _boardMinionCurrAttack :: Attack,
    _boardMinionCurrHealth :: Health,
    _boardMinionEnchantments :: [Enchantment],
    _boardMinion :: Minion
} deriving (Show, Eq, Ord)


data DeckMinion = DeckMinion {
    _deckMinion :: Minion
} deriving (Show, Eq, Ord)


data HandMinion = HandMinion {
    --_handMinionEffects :: [HandEffect]  -- Think Bolvar
    _handMinion :: Minion
} deriving (Show, Eq, Ord)


data HeroPower = HeroPower {
    _heroPowerCost :: Cost,
    _heroPowerEffects :: [Effect]
} deriving (Show, Eq, Ord)


data Hero = Hero {
    _heroAttack :: Attack,
    _heroHealth :: Health,
    _heroPower :: HeroPower,
    _heroName :: HeroName
} deriving (Show, Eq, Ord)


data BoardHero = BoardHero {
    _boardHeroCurrHealth :: Health,
    _boardHeroArmor :: Armor,
    _boardHero :: Hero
} deriving (Show, Eq, Ord)


data HandCard :: * where
    HandCardMinion :: HandMinion -> HandCard
    HandCardSpell :: Spell -> HandCard
    deriving (Show, Eq, Ord)


data DeckCard :: * where
    DeckCardMinion :: DeckMinion -> DeckCard
    DeckCardSpell :: Spell -> DeckCard
    deriving (Show, Eq, Ord)


data Hand = Hand {
    _handCards :: [HandCard]
} deriving (Show, Eq, Ord)


data Deck = Deck {
    _deckCards :: [DeckCard]
} deriving (Show, Eq, Ord)


data Player = Player {
    _playerDeck :: Deck,
    _playerHand :: Hand,
    _playerMinions :: [BoardMinion],
    _playerHero :: BoardHero
} deriving (Show, Eq, Ord)


data GameState = GameState {
    _turn :: Turn,
    _players :: [Player]
} deriving (Show, Eq, Ord)


data HearthPrompt :: * -> * where
    ShuffleDeck :: Deck -> HearthPrompt Deck
deriving instance Show (HearthPrompt a)
deriving instance Eq (HearthPrompt a)
deriving instance Ord (HearthPrompt a)


newtype Hearth m a = Hearth {
    unHearth :: StateT GameState m a
} deriving (Functor, Applicative, Monad, MonadState GameState, MonadIO, MonadTrans, MonadRandom)


type HearthMonad m = MonadPrompt HearthPrompt m


instance (HearthMonad m) => MonadPrompt HearthPrompt (Hearth m) where
    prompt = lift . prompt


data GameResult :: * where
    GameResult :: GameResult
    deriving (Show, Eq, Ord)


data PlayerData = PlayerData Hero Deck
    deriving (Show, Eq, Ord)


runHearth :: (HearthMonad m) => [PlayerData] -> m GameResult
runHearth = evalStateT (unHearth runGame) . mkGameState


mkGameState :: [PlayerData] -> GameState
mkGameState playerData = GameState {
    _turn = 1, 
    _players = map mkPlayer playerData }


mkPlayer :: PlayerData -> Player
mkPlayer (PlayerData hero deck) = Player {
    _playerDeck = deck,
    _playerHand = Hand [],
    _playerMinions = [],
    _playerHero = mkBoardHero hero }


mkBoardHero :: Hero -> BoardHero
mkBoardHero hero = BoardHero {
    _boardHeroCurrHealth = _heroHealth hero,
    _boardHeroArmor = 0,
    _boardHero = hero }


runGame :: (HearthMonad m) => Hearth m GameResult
runGame = do
    return GameResult






