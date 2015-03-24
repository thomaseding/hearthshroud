{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}


module Hearth.Engine where


--------------------------------------------------------------------------------


import Control.Applicative
import Control.Lens
import Control.Lens.Helper
import Control.Lens.Internal.Zoom (Zoomed, Focusing)
import Control.Monad.Prompt
import Control.Monad.Reader
import Control.Monad.State
import Data.Function
import Data.List
import Data.List.Ordered
import Data.Maybe
import Data.Monoid
import Data.NonEmpty (NonEmpty(..))
import qualified Data.NonEmpty as NonEmpty
import Hearth.DeckToHand
import Hearth.HandToDeck
import Hearth.LogEvent
import Hearth.Model
import Hearth.Names
import Hearth.Prompt
import Language.Haskell.TH.Syntax (Name)


--------------------------------------------------------------------------------


newtype Hearth' st m a = Hearth {
    unHearth :: StateT st m a
} deriving (Functor, Applicative, Monad, MonadState st, MonadIO, MonadTrans)


instance (Monad m) => MonadReader st (Hearth' st m) where
    ask = get
    local f m = do
        st <- get
        modify f
        x <- m
        put st
        return x


type Hearth = Hearth' GameState
type HearthMonad m = MonadPrompt HearthPrompt m


type instance Zoomed (Hearth' st m) = Focusing m


instance Monad m => Zoom (Hearth' st m) (Hearth' st' m) st st' where
    zoom lens = Hearth . zoom lens . unHearth


instance (HearthMonad m) => MonadPrompt HearthPrompt (Hearth' st m) where
    prompt = lift . prompt


class LogCall a where
    logCall :: Name -> a -> a


instance (HearthMonad m) => LogCall (Hearth' st m a) where
    logCall funcName m = do
        prompt $ PromptLogEvent $ LogFunctionEntered funcName
        x <- m
        prompt $ PromptLogEvent $ LogFunctionExited funcName
        return x


instance (HearthMonad m) => LogCall (a -> Hearth' st m b) where
    logCall msg f = logCall msg . f


instance (HearthMonad m) => LogCall (a -> b -> Hearth' st m c) where
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


isSubsetOf :: (Ord a) => [a] -> [a] -> Bool
isSubsetOf = subset `on` sort


runHearth :: (HearthMonad m) => NonEmpty PlayerData -> m GameResult
runHearth = evalStateT (unHearth runGame) . mkGameState


mkGameState :: NonEmpty PlayerData -> GameState
mkGameState playerDatas = GameState {
    _gameTurn = 1,
    _gamePlayerTurnOrder = [],
    _gamePlayers = zipWith mkPlayer [0..] $ NonEmpty.toList playerDatas }


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
runGame = logCall 'runGame $ do
    initGame
    return GameResult


getPlayerHandles :: (HearthMonad m) => Hearth m [PlayerHandle]
getPlayerHandles = toListOfM $ gamePlayers.traversed.playerHandle


type PlayerLens = Lens' GameState Player


getPlayerLens :: PlayerHandle -> PlayerLens
getPlayerLens handle f st = fmap put' get'
    where
        players = st^.gamePlayers
        put' player = let
            g p = case p^.playerHandle == handle of
                True -> player
                False -> p
            in set gamePlayers (map g players) st
        get' = f $ fromJust $ find (\p -> p^.playerHandle == handle) players


initGame :: (HearthMonad m) => Hearth m ()
initGame = logCall 'initGame $ do
    flipCoin
    zipWithM_ (\n h -> initHand n $ getPlayerLens h) (4 : repeat 3) =<< getPlayerHandles


flipCoin :: (HearthMonad m) => Hearth m ()
flipCoin = logCall 'flipCoin $ getPlayerHandles >>= \handles -> do
    handle <- prompt $ PromptPickRandom $ NonEmpty.fromList handles
    let handles' = dropWhile (/= handle) $ cycle handles
    gamePlayerTurnOrder .= handles'


initHand :: (HearthMonad m) => Int -> PlayerLens -> Hearth m ()
initHand numCards playerLens = logCall 'initHand $ do
    shuffleDeck playerLens
    handCards <- drawCards playerLens numCards
    handle <- view $ playerLens.playerHandle
    keptCards <- guardedPrompt (PromptMulligan handle) (`isSubsetOf` handCards)
    let tossedCards = handCards \\ keptCards
        tossedCards' = map handToDeck tossedCards
    drawCards playerLens (length tossedCards) >>= \case
        [] -> return ()
        _ -> do
            playerLens.playerDeck <>= Deck tossedCards'
            shuffleDeck playerLens


drawCards :: (HearthMonad m) => PlayerLens -> Int -> Hearth m [HandCard]
drawCards playerLens = logCall 'drawCards $ liftM catMaybes . flip replicateM (drawCard playerLens)


drawCard :: (HearthMonad m) => PlayerLens -> Hearth m (Maybe HandCard)
drawCard playerLens = logCall 'drawCard $ zoom playerLens $ do
    playerDeck >>=. \case
        Deck [] -> return Nothing -- TODO: Take damage
        Deck (c:cs) -> do
            playerDeck .= Deck cs
            playerHand.handCards.to length >>=. \case
                10 -> return Nothing
                _ -> do
                    let c' = deckToHand c
                    playerHand <>= Hand [c']
                    return $ Just c'


shuffleDeck :: (HearthMonad m) => PlayerLens -> Hearth m ()
shuffleDeck playerLens = logCall 'shuffleDeck $ zoom playerLens $ do
    deck <- view playerDeck
    deck' <- guardedPrompt (PromptShuffle deck) $ on (==) (sort . _deckCards) deck
    playerDeck .= deck'






