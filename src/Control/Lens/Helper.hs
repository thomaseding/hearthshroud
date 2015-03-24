module Control.Lens.Helper where


--------------------------------------------------------------------------------


import Control.Lens
import Control.Monad.Reader
import Data.Monoid


--------------------------------------------------------------------------------


toListOfM :: (MonadReader s m) => Getting (Endo [a]) s a -> m [a]
toListOfM lens = asks $ toListOf lens


infixl 1 >>=.
(>>=.) :: (MonadReader s m) => Getting a s a -> (a -> m b) -> m b
lens >>=. f = view lens >>= f



