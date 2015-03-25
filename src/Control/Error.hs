{-# LANGUAGE TemplateHaskell #-}


module Control.Error (
    todo,
    logicError,
    debugShow,
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


todo :: Q Exp
todo = withLocatedError [| \e -> e "TODO" |]


logicError :: Q Exp
logicError = withLocatedError [| \e msg -> e $ "Logic error: " ++ msg |]


debugShow :: Q Exp
debugShow = withLocatedError [| \e x -> e $ "Debug message: " ++ show x |]



