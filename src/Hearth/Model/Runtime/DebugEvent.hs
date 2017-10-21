{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}


module Hearth.Model.Runtime.DebugEvent where


--------------------------------------------------------------------------------


import Language.Haskell.TH.Syntax (Name)


--------------------------------------------------------------------------------


data DebugEvent :: * where
    DiagnosticMessage :: String -> DebugEvent
    FunctionEntered :: Name -> DebugEvent
    FunctionExited :: Name -> DebugEvent
    deriving (Show, Eq, Ord)






