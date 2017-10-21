{-# LANGUAGE DeriveDataTypeable #-}


module Data.Namespace where


import Data.Data


--------------------------------------------------------------------------------


data Namespace = Namespace String
    deriving (Show, Eq, Ord, Data, Typeable)



