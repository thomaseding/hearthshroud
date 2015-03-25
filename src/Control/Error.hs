{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}


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


class Stringy a where
    stringy :: a -> String


instance Stringy String where
    stringy = id


instance Stringy Name where
    stringy = nameBase


forceStringy :: (Stringy a) => a -> a
forceStringy = id


todo :: Q Exp
todo = withLocatedError [| \e msg -> e $ "TODO " ++ show (stringy msg) |]


logicError :: Q Exp
logicError = withLocatedError [| \e msg -> e $ "Logic error: " ++ (forceStringy msg) |]


debugShow :: Q Exp
debugShow = withLocatedError [| \e x -> e $ "Debug message: " ++ show x |]



