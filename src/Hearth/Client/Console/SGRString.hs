{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Hearth.Client.Console.SGRString where


import Data.Either
import Data.String
import System.Console.ANSI


newtype SGRString = SGRString { unSGRString :: [Either SGR Char] }


instance IsString SGRString where
    fromString = SGRString . map Right


sgrShow :: (Show a) => a -> SGRString
sgrShow = fromString . show


sgr :: [SGR] -> SGRString
sgr = SGRString . map Left


sgrColor :: (ColorIntensity, Color) -> SGRString
sgrColor = SGRString . return . Left . uncurry (SetColor Foreground)


sgrToStr :: SGRString -> String
sgrToStr = rights . unSGRString


infixr 5 +++
(+++) :: SGRString -> SGRString -> SGRString
x +++ y = SGRString $ unSGRString x ++ unSGRString y


