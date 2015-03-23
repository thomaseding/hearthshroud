module Data.NonEmpty where


--------------------------------------------------------------------------------


data NonEmpty a = NonEmpty a [a]
    deriving (Show, Eq, Ord)


toList :: NonEmpty a -> [a]
toList (NonEmpty x xs) = x : xs


fromList :: [a] -> NonEmpty a
fromList (x : xs) = NonEmpty x xs




