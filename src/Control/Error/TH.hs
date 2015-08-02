{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Control.Error.TH (
    todo,
    logicError,
    debugShow,
    runtimeError,
) where


import Language.Haskell.TH


withLocatedError :: Q Exp -> Q Exp
withLocatedError f = do
    let e = locatedError =<< location
    appE f e


locatedError :: Loc -> Q Exp
locatedError loc = do
    let postfix = " at " ++ formatLoc loc
    [| \msg -> error (msg ++ $(litE $ stringL postfix)) |]


formatLoc :: Loc -> String
formatLoc loc = let
    file = loc_filename loc
    (line, col) = loc_start loc
    in concat [file, ":", show line, ":", show col]


formatDesc :: String -> Name -> String -> String
formatDesc kind func msg = kind ++ ": " ++ show msg ++ " in " ++ nameBase func ++ " "


todo :: Q Exp
todo = withLocatedError [| \e f msg -> e $ formatDesc "TODO" f msg |]


logicError :: Q Exp
logicError = withLocatedError [| \e f msg -> e $ formatDesc "LogicError" f msg |]


runtimeError :: Q Exp
runtimeError = withLocatedError [| \e f msg -> e $ formatDesc "RuntimeError" f msg |]


debugShow :: Q Exp
debugShow = withLocatedError [| \e f msg -> e $ formatDesc "DebugMessage" f msg |]



