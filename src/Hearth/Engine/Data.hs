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
import GHC.Prim (Constraint)
import Hearth.DebugEvent
import Hearth.Model
import Hearth.Prompt
import Language.Haskell.TH.Syntax (Name)


--------------------------------------------------------------------------------


type Pair a = (a, a)


--------------------------------------------------------------------------------


newtype Hearth' (c :: * -> Constraint) st m a = Hearth {
    unHearth :: StateT st m a
} deriving (Functor, Applicative, Monad, MonadState st, MonadIO, MonadTrans)


instance (Monad m) => MonadReader st (Hearth' c st m) where
    ask = get
    local = stateLocal


type Hearth (c :: * -> Constraint) = Hearth' c (GameState c)
type HearthMonad' c m = (MonadPrompt (HearthPrompt c) m)
type HearthMonad c m = (UserConstraint c, HearthMonad' c m)


type instance Zoomed (Hearth' c st m) = Focusing m


instance Monad m => Zoom (Hearth' c st m) (Hearth' c st' m) st st' where
    zoom l = Hearth . zoom l . unHearth


class LogCall a where
    logCall :: Name -> a -> a


--------------------------------------------------------------------------------


instance (HearthMonad' c m) => LogCall (Hearth' c st m a) where
    logCall funcName m = do
        lift $ prompt $ PromptDebugEvent $ FunctionEntered funcName
        x <- m
        lift $ prompt $ PromptDebugEvent $ FunctionExited funcName
        return x


instance (HearthMonad' c m) => LogCall (a -> Hearth' c st m z) where
    logCall msg f = logCall msg . f


instance (HearthMonad' c m) => LogCall (a -> b -> Hearth' c st m z) where
    logCall msg f = logCall msg . f


instance (HearthMonad' k m) => LogCall (a -> b -> c -> Hearth' k st m z) where
    logCall msg f = logCall msg . f


--------------------------------------------------------------------------------


data PlayerData c = PlayerData (Hero c) (Deck c)
    deriving (Typeable)


--------------------------------------------------------------------------------


class ToCard a where
    toCard :: a c -> Card c


instance ToCard HandCard where
    toCard = \case
        HandCardMinion minion -> CardMinion minion
        HandCardSpell spell -> CardSpell spell


instance ToCard DeckCard where
    toCard = \case
        DeckCardMinion minion -> CardMinion minion
        DeckCardSpell spell -> CardSpell spell


--------------------------------------------------------------------------------


class ToHandCard a where
    toHandCard :: a c -> HandCard c


instance ToHandCard Card where
    toHandCard = \case
        CardMinion minion -> HandCardMinion minion
        CardSpell spell -> HandCardSpell spell


instance ToHandCard DeckCard where
    toHandCard = toHandCard . toCard


--------------------------------------------------------------------------------


class ToDeckCard a where
    toDeckCard :: a c -> DeckCard c


instance ToDeckCard Card where
    toDeckCard = \case
        CardMinion minion -> DeckCardMinion minion
        CardSpell spell -> DeckCardSpell spell


instance ToDeckCard HandCard where
    toDeckCard = toDeckCard . toCard










