module Control.Lens.Helper where


--------------------------------------------------------------------------------


import Control.Lens
import Control.Monad.State
import Data.Monoid


--------------------------------------------------------------------------------


toListOfM :: (MonadState s m) => Getting (Endo [a]) s a -> m [a]
toListOfM lens = gets $ toListOf lens


infixl 1 >>=.
(>>=.) :: MonadState s m => Getting a s a -> (a -> m b) -> m b
lens >>=. f = use lens >>= f



