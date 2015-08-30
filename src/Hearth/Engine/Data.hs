{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}


module Hearth.Engine.Data where


--------------------------------------------------------------------------------


import Control.Applicative
import Control.Lens
import Control.Lens.Internal.Zoom (Zoomed, Focusing)
import Control.Monad.Prompt
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.State.Local
import Data.Data
import Hearth.DebugEvent
import Hearth.Model
import Hearth.Prompt
import Language.Haskell.TH.Syntax (Name)


--------------------------------------------------------------------------------


type Pair a = (a, a)


--------------------------------------------------------------------------------


newtype Hearth' st m a = Hearth {
    unHearth :: StateT st m a
} deriving (Functor, Applicative, Monad, MonadState st, MonadIO, MonadTrans)


instance (Monad m) => MonadReader st (Hearth' st m) where
    ask = get
    local = stateLocal


type Hearth = Hearth' GameState
type HearthMonad m = (MonadPrompt HearthPrompt m)


type instance Zoomed (Hearth' st m) = Focusing m


instance Monad m => Zoom (Hearth' st m) (Hearth' st' m) st st' where
    zoom l = Hearth . zoom l . unHearth


class LogCall a where
    logCall :: Name -> a -> a


--------------------------------------------------------------------------------


instance (HearthMonad m) => LogCall (Hearth' st m a) where
    logCall funcName m = do
        lift $ prompt $ PromptDebugEvent $ FunctionEntered funcName
        x <- m
        lift $ prompt $ PromptDebugEvent $ FunctionExited funcName
        return x


instance (HearthMonad m) => LogCall (a -> Hearth' st m z) where
    logCall msg f = logCall msg . f


instance (HearthMonad m) => LogCall (a -> b -> Hearth' st m z) where
    logCall msg f = logCall msg . f


instance (HearthMonad m) => LogCall (a -> b -> c -> Hearth' st m z) where
    logCall msg f = logCall msg . f


--------------------------------------------------------------------------------


data PlayerData = PlayerData Hero Deck
    deriving (Typeable)


--------------------------------------------------------------------------------


class HandToDeck h d | h -> d where
    handToDeck :: h -> d


instance HandToDeck HandCard DeckCard where
    handToDeck = \case
        HandCardMinion minion -> DeckCardMinion minion
        HandCardSpell spell -> DeckCardSpell spell


--------------------------------------------------------------------------------


class DeckToHand d h | d -> h where
    deckToHand :: d -> h


instance DeckToHand DeckCard HandCard where
    deckToHand = \case
        DeckCardMinion minion -> HandCardMinion minion
        DeckCardSpell spell -> HandCardSpell spell








