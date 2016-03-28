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
import Hearth.Authoring.Combinators
import Hearth.DebugEvent
import Hearth.Model
import Hearth.Prompt
import Language.Haskell.TH.Syntax (Name)


--------------------------------------------------------------------------------


type Pair a = (a, a)


--------------------------------------------------------------------------------


newtype Hearth' (k :: * -> Constraint) st m a = Hearth {
    unHearth :: StateT st m a
} deriving (Functor, Applicative, Monad, MonadState st, MonadIO, MonadTrans)


instance (Monad m) => MonadReader st (Hearth' k st m) where
    ask = get
    local = stateLocal


type Hearth (k :: * -> Constraint) = Hearth' k (GameState k)
type HearthMonad' k m = (MonadPrompt (HearthPrompt k) m)
type HearthMonad k m = (UserConstraint k, HearthMonad' k m)


type instance Zoomed (Hearth' k st m) = Focusing m


instance Monad m => Zoom (Hearth' k st m) (Hearth' k st' m) st st' where
    zoom l = Hearth . zoom l . unHearth


class LogCall a where
    logCall :: Name -> a -> a


--------------------------------------------------------------------------------


instance (HearthMonad' k m) => LogCall (Hearth' k st m a) where
    logCall funcName m = do
        lift $ prompt $ PromptDebugEvent $ FunctionEntered funcName
        x <- m
        lift $ prompt $ PromptDebugEvent $ FunctionExited funcName
        return x


instance (HearthMonad' k m) => LogCall (a -> Hearth' k st m z) where
    logCall msg f = logCall msg . f


instance (HearthMonad' k m) => LogCall (a -> b -> Hearth' k st m z) where
    logCall msg f = logCall msg . f


instance (HearthMonad' k m) => LogCall (a -> b -> c -> Hearth' k st m z) where
    logCall msg f = logCall msg . f


--------------------------------------------------------------------------------


data PlayerData k = PlayerData (Hero k) (Deck k)
    deriving (Typeable)


--------------------------------------------------------------------------------


class ToHandCard a where
    toHandCard :: a k -> HandCard k


instance ToHandCard Card where
    toHandCard = \case
        CardMinion minion -> HandCardMinion minion
        CardSpell spell -> HandCardSpell spell
        CardWeapon weapon -> HandCardWeapon weapon


instance ToHandCard DeckCard where
    toHandCard = toHandCard . toCard


--------------------------------------------------------------------------------


class ToDeckCard a where
    toDeckCard :: a k -> DeckCard k


instance ToDeckCard Card where
    toDeckCard = \case
        CardMinion minion -> DeckCardMinion minion
        CardSpell spell -> DeckCardSpell spell
        CardWeapon weapon -> DeckCardWeapon weapon


instance ToDeckCard HandCard where
    toDeckCard = toDeckCard . toCard










