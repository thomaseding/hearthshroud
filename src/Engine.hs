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
import Data.Word
import Names


--------------------------------------------------------------------------------


data NonEmpty a = NonEmpty a [a]
    deriving (Show, Eq, Ord)


nonEmptyToList :: NonEmpty a -> [a]
nonEmptyToList (NonEmpty x xs) = x : xs


listToNonEmpty :: [a] -> NonEmpty a
listToNonEmpty (x : xs) = NonEmpty x xs


newtype Turn = Turn Word
    deriving (Show, Eq, Ord, Enum, Num, Real, Integral)


newtype Cost = Cost Int
    deriving (Show, Eq, Ord, Enum, Num, Real, Integral)


newtype Attack = Attack Int
    deriving (Show, Eq, Ord, Enum, Num, Real, Integral)


newtype Armor = Armor Int
    deriving (Show, Eq, Ord, Enum, Num, Real, Integral)


newtype Health = Health Int
    deriving (Show, Eq, Ord, Enum, Num, Real, Integral)


newtype PlayerHandle = PlayerHandle Word
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
    _playerHandle :: PlayerHandle,
    _playerDeck :: Deck,
    _playerHand :: Hand,
    _playerMinions :: [BoardMinion],
    _playerHero :: BoardHero
} deriving (Show, Eq, Ord)


data GameState = GameState {
    _gameTurn :: Turn,
    _gamePlayerTurnOrder :: [PlayerHandle],
    _gamePlayers :: [Player]
} deriving (Show, Eq, Ord)


data HearthPrompt :: * -> * where
    PromptShuffle :: a -> HearthPrompt a
    PromptPickRandom :: NonEmpty a -> HearthPrompt a
deriving instance (Show a) => Show (HearthPrompt a)
deriving instance (Eq a) => Eq (HearthPrompt a)
deriving instance (Ord a) => Ord (HearthPrompt a)


newtype Hearth m a = Hearth {
    unHearth :: StateT GameState m a
} deriving (Functor, Applicative, Monad, MonadState GameState, MonadIO, MonadTrans)


type HearthMonad m = MonadPrompt HearthPrompt m


instance (HearthMonad m) => MonadPrompt HearthPrompt (Hearth m) where
    prompt = lift . prompt


data GameResult :: * where
    GameResult :: GameResult
    deriving (Show, Eq, Ord)


data PlayerData = PlayerData Hero Deck
    deriving (Show, Eq, Ord)


runHearth :: (HearthMonad m) => NonEmpty PlayerData -> m GameResult
runHearth = evalStateT (unHearth runGame) . mkGameState


mkGameState :: NonEmpty PlayerData -> GameState
mkGameState playerDatas = GameState {
    _gameTurn = 1,
    _gamePlayerTurnOrder = [],
    _gamePlayers = zipWith mkPlayer [0..] $ nonEmptyToList playerDatas }


mkPlayer :: PlayerHandle -> PlayerData -> Player
mkPlayer handle (PlayerData hero deck) = Player {
    _playerHandle = handle,
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
    initGame
    return GameResult


getPlayerHandles :: (HearthMonad m) => Hearth m [PlayerHandle]
getPlayerHandles = liftM (map _playerHandle) $ gets _gamePlayers


initGame :: (HearthMonad m) => Hearth m ()
initGame = do
    flipCoin
    mapM_ initHand =<< getPlayerHandles


flipCoin :: (HearthMonad m) => Hearth m ()
flipCoin = getPlayerHandles >>= \handles -> do
    handle <- prompt $ PromptPickRandom $ listToNonEmpty handles
    let handles' = dropWhile (/= handle) $ cycle handles
    error $ show handles'  -- TODO
    modify $ \st -> st { _gamePlayerTurnOrder = handles' }


initHand :: (HearthMonad m) => PlayerHandle -> Hearth m ()
initHand handle = do
    shuffle handle
    drawCards handle 666
    error "TODO"


drawCards :: (HearthMonad m) => PlayerHandle -> Int -> Hearth m ()
drawCards playerHandle = flip replicateM_ $ drawCard playerHandle


drawCard :: (HearthMonad m) => PlayerHandle -> Hearth m ()
drawCard playerHandle = undefined


shuffle :: (HearthMonad m) => PlayerHandle -> Hearth m ()
shuffle handle = do
    let deck = Deck undefined
    deck'@Deck{} <- prompt $ PromptShuffle deck
    error $ show deck'







