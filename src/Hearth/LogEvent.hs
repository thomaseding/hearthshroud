{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}


module Hearth.LogEvent where


--------------------------------------------------------------------------------


import Language.Haskell.TH.Syntax (Name)


--------------------------------------------------------------------------------


data LogEvent :: * where
    LogFunctionEntered :: Name -> LogEvent
    LogFunctionExited :: Name -> LogEvent
    deriving (Show, Eq, Ord)






