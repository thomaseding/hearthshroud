{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}


module Hearth.Client.Console.BoardHeroColumn (
    boardHeroColumn
) where


--------------------------------------------------------------------------------


import Control.Error
import Control.Lens
import Data.List
import Hearth.Model
import Hearth.Cards


--------------------------------------------------------------------------------


boardHeroColumn :: BoardHero -> [String]
boardHeroColumn = concat . withEach [
    txt "Health",
    txt "------",
    toTxt . unHealth . _boardHeroCurrHealth,
    txt "",
    txt "Armor",
    txt "-----",
    toTxt . unArmor . _boardHeroArmor ]
    where
        txt str = return . const str
        toTxt = return . show


withEach :: [a -> b] -> a -> [b]
withEach = flip $ \a -> map ($ a)











