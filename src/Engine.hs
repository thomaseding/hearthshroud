{-# LANGUAGE ConstraintKinds #-}
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


module Engine where


--------------------------------------------------------------------------------


import Control.Applicative
import Control.Lens
import Control.Monad.Prompt
import Control.Monad.State
import Data.Function
import Data.List
import Data.List.Ordered
import Data.Maybe
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
    deriving (Show, Eq, Ord)


data DeckCard :: * where
    DeckCardMinion :: DeckMinion -> DeckCard
    deriving (Show, Eq, Ord)


newtype Hand = Hand {
    _handCards :: [HandCard]
} deriving (Show, Eq, Ord, Monoid)
makeLenses ''Hand


newtype Deck = Deck {
    _deckCards :: [DeckCard]
} deriving (Show, Eq, Ord, Monoid)
makeLenses ''Deck


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


data LogEvent :: * where
    LogFunctionEntered :: String -> LogEvent
    LogFunctionExited :: String -> LogEvent
    deriving (Show, Eq, Ord)


data HearthPrompt :: * -> * where
    PromptLogEvent :: LogEvent -> HearthPrompt ()
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


class LogCall a where
    logCall :: String -> a -> a


instance (HearthMonad m) => LogCall (Hearth m a) where
    logCall :: String -> Hearth m a -> Hearth m a
    logCall funcName m = do
        prompt $ PromptLogEvent $ LogFunctionEntered funcName
        x <- m
        prompt $ PromptLogEvent $ LogFunctionExited funcName
        return x


instance (HearthMonad m) => LogCall (a -> Hearth m b) where
    logCall :: String -> (a -> Hearth m b) -> (a -> Hearth m b)
    logCall msg f = logCall msg . f


instance (HearthMonad m) => LogCall (a -> b -> Hearth m c) where
    logCall :: String -> (a -> b -> Hearth m c) -> (a -> b -> Hearth m c)
    logCall msg f = logCall msg . f


data GameResult :: * where
    GameResult :: GameResult
    deriving (Show, Eq, Ord)


data PlayerData = PlayerData Hero Deck
    deriving (Show, Eq, Ord)


guardedPrompt :: (MonadPrompt p m) => p a -> (a -> Bool) -> m a
guardedPrompt p f = prompt p >>= \x -> case f x of
    True -> return x
    False -> guardedPrompt p f


viewM :: (MonadState s m) => Getting a s a -> m a
viewM ln = gets $ view ln


toListOfM :: (MonadState s m) => Getting (Endo [a]) s a -> m [a]
toListOfM ln = gets $ toListOf ln


isSubsetOf :: (Ord a) => [a] -> [a] -> Bool
isSubsetOf = subset `on` sort


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
runGame = logCall "runGame" $ do
    initGame
    return GameResult


getPlayerHandles :: (HearthMonad m) => Hearth m [PlayerHandle]
getPlayerHandles = toListOfM $ gamePlayers.traversed.playerHandle


getPlayer :: PlayerHandle -> Lens' GameState Player
getPlayer handle f st = fmap put' get'
    where
        players = st^.gamePlayers
        put' player = let
            g p = case p^.playerHandle == handle of
                True -> player
                False -> p
            in set gamePlayers (map g players) st
        get' = f $ fromJust $ find (\p -> p^.playerHandle == handle) players


initGame :: (HearthMonad m) => Hearth m ()
initGame = logCall "initGame" $ do
    flipCoin
    zipWithM_ initHand (4 : repeat 3) =<< getPlayerHandles


flipCoin :: (HearthMonad m) => Hearth m ()
flipCoin = logCall "flipCoin" $ getPlayerHandles >>= \handles -> do
    handle <- prompt $ PromptPickRandom $ listToNonEmpty handles
    let handles' = dropWhile (/= handle) $ cycle handles
    gamePlayerTurnOrder .= handles'


initHand :: (HearthMonad m) => Int -> PlayerHandle -> Hearth m ()
initHand numCards handle = logCall "initHand" $ do
    shuffleDeck handle
    handCards <- drawCards handle numCards
    keptCards <- guardedPrompt (PromptMulligan handle) (`isSubsetOf` handCards)
    let tossedCards = handCards \\ keptCards
        tossedCards' = map handToDeck tossedCards
    drawCards handle (length tossedCards) >>= \case
        [] -> return ()
        _ -> do
            getPlayer handle.playerDeck <>= Deck tossedCards'
            shuffleDeck handle


class DeckToHand d h | d -> h where
    deckToHand :: d -> h


instance DeckToHand DeckCard HandCard where
    deckToHand = \case
        DeckCardMinion m -> HandCardMinion $ deckToHand m


instance DeckToHand DeckMinion HandMinion where
    deckToHand = HandMinion . _deckMinion


class HandToDeck h d | h -> d where
    handToDeck :: h -> d


instance HandToDeck HandCard DeckCard where
    handToDeck = \case
        HandCardMinion m -> DeckCardMinion $ handToDeck m


instance HandToDeck HandMinion DeckMinion where
    handToDeck = DeckMinion . _handMinion


drawCards :: (HearthMonad m) => PlayerHandle -> Int -> Hearth m [HandCard]
drawCards handle = logCall "drawCards" $ liftM catMaybes . flip replicateM (drawCard handle)


drawCard :: (HearthMonad m) => PlayerHandle -> Hearth m (Maybe HandCard)
drawCard handle = logCall "drawCard" $ do
    player <- viewM $ getPlayer handle
    case player^.playerDeck of
        Deck [] -> return Nothing -- TODO: Take damage
        Deck (c:cs) -> do
            case player^.playerHand.handCards.to length of
                10 -> return Nothing
                _ -> do
                    let c' = deckToHand c
                    getPlayer handle.playerDeck .= Deck cs
                    getPlayer handle.playerHand <>= Hand [c']
                    return $ Just c'


shuffleDeck :: (HearthMonad m) => PlayerHandle -> Hearth m ()
shuffleDeck handle = logCall "shuffleDeck" $ do
    deck <- viewM $ getPlayer handle.playerDeck
    deck' <- guardedPrompt (PromptShuffle deck) $ on (==) (sort . _deckCards) deck
    getPlayer handle.playerDeck .= deck'






