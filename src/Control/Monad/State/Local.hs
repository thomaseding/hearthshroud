module Control.Monad.State.Local where


--------------------------------------------------------------------------------


import Control.Monad.State


--------------------------------------------------------------------------------


stateLocal :: (MonadState st m) => (st -> st) -> m a -> m a
stateLocal f m = do
    st <- get
    modify f
    x <- m
    put st
    return x




