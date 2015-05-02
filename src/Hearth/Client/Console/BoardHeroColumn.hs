{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module Hearth.Client.Console.BoardHeroColumn (
    boardHeroColumn
) where


--------------------------------------------------------------------------------


import Hearth.Model
import Hearth.Client.Console.SGRString
import System.Console.ANSI


--------------------------------------------------------------------------------


boardHeroColumn :: BoardHero -> [SGRString]
boardHeroColumn = concat . withEach [
    txt "Health",
    txt "------",
    toTxt . unHealth . _boardHeroCurrHealth,
    txt "",
    txt "Armor",
    txt "-----",
    toTxt . unArmor . _boardHeroArmor ]
    where
        txt str = return . (sgrColor (Dull, Green) ++) . const str
        toTxt = return . (sgrColor (Vivid, Green) ++) . sgrShow


withEach :: [a -> b] -> a -> [b]
withEach = flip $ \a -> map ($ a)











