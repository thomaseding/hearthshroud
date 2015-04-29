{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Hearth.Client.Console.SGRString where


import Data.String
import System.Console.ANSI


type SGRString = [Either SGR Char]


instance IsString SGRString where
    fromString = map Right


sgrShow :: (Show a) => a -> SGRString
sgrShow = fromString . show


sgr :: [SGR] -> SGRString
sgr = map Left


sgrColor :: (ColorIntensity, Color) -> SGRString
sgrColor = return . Left . uncurry (SetColor Foreground)


