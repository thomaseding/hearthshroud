{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}


module Engine where


--------------------------------------------------------------------------------


import Control.Applicative
import Control.Lens
import Control.Monad.Prompt
import Control.Monad.State
import Data.List
import Data.Monoid
import Names


--------------------------------------------------------------------------------


data NonEmpty a = NonEmpty a [a]
    deriving (Show, Eq, Ord)


nonEmptyToList :: NonEmpty a -> [a]
nonEmptyToList (NonEmpty x xs) = x : xs


listToNonEmpty :: [a] -> NonEmpty a
listToNonEmpty (x : xs) = NonEmpty x xs


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


newtype PlayerHandle = PlayerHandle Int
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
makeLenses ''Spell


data Minion = Minion {
    _minionAttack :: Attack,
    _minionHealth :: Health,
    _minionName :: CardName
} deriving (Show, Eq, Ord)
makeLenses ''Minion


data BoardMinion = BoardMinion {
    _boardMinionCurrAttack :: Attack,
    _boardMinionCurrHealth :: Health,
    _boardMinionEnchantments :: [Enchantment],
    _boardMinion :: Minion
} deriving (Show, Eq, Ord)
makeLenses ''BoardMinion


data DeckMinion = DeckMinion {
    _deckMinion :: Minion
} deriving (Show, Eq, Ord)
makeLenses ''DeckMinion


data HandMinion = HandMinion {
    --_handMinionEffects :: [HandEffect]  -- Think Bolvar
    _handMinion :: Minion
} deriving (Show, Eq, Ord)
makeLenses ''HandMinion


data HeroPower = HeroPower {
    _heroPowerCost :: Cost,
    _heroPowerEffects :: [Effect]
} deriving (Show, Eq, Ord)
makeLenses ''HeroPower


data Hero = Hero {
    _heroAttack :: Attack,
    _heroHealth :: Health,
    _heroPower :: HeroPower,
    _heroName :: HeroName
} deriving (Show, Eq, Ord)
makeLenses ''Hero


data BoardHero = BoardHero {
    _boardHeroCurrHealth :: Health,
    _boardHeroArmor :: Armor,
    _boardHero :: Hero
} deriving (Show, Eq, Ord)
makeLenses ''BoardHero


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
makeLenses ''Hand


instance Monoid Hand where
    mempty = Hand []
    Hand cs `mappend` Hand cs' = Hand $ cs ++ cs'


data Deck = Deck {
    _deckCards :: [DeckCard]
} deriving (Show, Eq, Ord)
makeLenses ''Deck


instance Monoid Deck where
    mempty = Deck []
    Deck cs `mappend` Deck cs' = Deck $ cs ++ cs'


data Player = Player {
    _playerHandle :: PlayerHandle,
    _playerDeck :: Deck,
    _playerHand :: Hand,
    _playerMinions :: [BoardMinion],
    _playerHero :: BoardHero
} deriving (Show, Eq, Ord)
makeLenses ''Player


data GameState = GameState {
    _gameTurn :: Turn,
    _gamePlayerTurnOrder :: [PlayerHandle],
    _gamePlayers :: [Player]
} deriving (Show, Eq, Ord)
makeLenses ''GameState


data HearthPrompt :: * -> * where
    PromptShuffle :: a -> HearthPrompt a
    PromptPickRandom :: NonEmpty a -> HearthPrompt a
    PromptMulligan :: PlayerHandle -> HearthPrompt [HandCard]
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


guardedPrompt :: (MonadPrompt p m) => p a -> (a -> Bool) -> m a
guardedPrompt p f = prompt p >>= \x -> case f x of
    True -> return x
    False -> guardedPrompt p f


isSubsetOf :: (Ord a) => [a] -> [a] -> Bool
isSubsetOf = undefined


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
getPlayerHandles = gets $ map _playerHandle . _gamePlayers


playerByHandle :: PlayerHandle -> Traversal' GameState Player
playerByHandle handle = gamePlayers.traversed.f
    where
        f = filtered $ \player -> player^.playerHandle == handle


initGame :: (HearthMonad m) => Hearth m ()
initGame = do
    flipCoin
    zipWithM_ initHand (4 : repeat 3) =<< getPlayerHandles


flipCoin :: (HearthMonad m) => Hearth m ()
flipCoin = getPlayerHandles >>= \handles -> do
    handle <- prompt $ PromptPickRandom $ listToNonEmpty handles
    let handles' = dropWhile (/= handle) $ cycle handles
    gamePlayerTurnOrder .= handles'


initHand :: (HearthMonad m) => Int -> PlayerHandle -> Hearth m ()
initHand numCards handle = do
    shuffle handle
    handCards <- drawCards handle numCards
    keptCards <- guardedPrompt (PromptMulligan handle) (`isSubsetOf` handCards)
    let tossedCards = handCards \\ keptCards
        tossedCards' = map toDeckCard tossedCards
    drawCards handle (length tossedCards) >>= \case
        [] -> return ()
        _ -> do
            playerByHandle handle.playerDeck <>= Deck tossedCards'
            shuffle handle
    

toDeckCard :: HandCard -> DeckCard
toDeckCard = undefined


drawCards :: (HearthMonad m) => PlayerHandle -> Int -> Hearth m [HandCard]
drawCards handle = flip replicateM $ drawCard handle


drawCard :: (HearthMonad m) => PlayerHandle -> Hearth m HandCard
drawCard handle = undefined


shuffle :: (HearthMonad m) => PlayerHandle -> Hearth m ()
shuffle handle = do
    let deck = Deck undefined
    deck'@Deck{} <- prompt $ PromptShuffle deck
    error $ show deck'







