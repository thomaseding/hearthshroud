{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}


module Hearth.Client.Console.PlayerColumn (
    playerColumn
) where


--------------------------------------------------------------------------------


import Control.Error
import Control.Lens
import Data.List
import Hearth.Model
import Hearth.Cards
import Hearth.Client.Console.BoardHeroColumn
import Hearth.Client.Console.DeckColumn


--------------------------------------------------------------------------------


playerColumn :: Player -> [String]
playerColumn = concat . withEach [
    deckColumn . _playerDeck,
    txt "",
    boardHeroColumn . _playerHero ]
    where
        txt str = return . const str



withEach :: [a -> b] -> a -> [b]
withEach = flip $ \a -> map ($ a)





