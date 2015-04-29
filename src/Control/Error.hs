{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Control.Error (
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


class Stringy a where
    stringy :: a -> String


instance Stringy String where
    stringy = show


instance Stringy Name where
    stringy = stringy . nameBase


todo :: Q Exp
todo = withLocatedError [| \e msg -> e $ "TODO " ++ stringy msg |]


logicError :: Q Exp
logicError = withLocatedError [| \e msg -> e $ "Logic error: " ++ stringy msg |]


runtimeError :: Q Exp
runtimeError = withLocatedError [| \e msg -> e $ "Runtime error: " ++ stringy msg |]


debugShow :: Q Exp
debugShow = withLocatedError [| \e x -> e $ "Debug message: " ++ show x |]



