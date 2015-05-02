{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module Hearth.Client.Console.PlayerColumn (
    playerColumn
) where


--------------------------------------------------------------------------------


import Hearth.Model
import Hearth.Client.Console.BoardHeroColumn
import Hearth.Client.Console.DeckColumn
import Hearth.Client.Console.ManaColumn
import Hearth.Client.Console.SGRString


--------------------------------------------------------------------------------


playerColumn :: Player -> [SGRString]
playerColumn = concat . withEach [
    deckColumn . _playerDeck,
    txt "",
    manaColumn,
    txt "",
    boardHeroColumn . _playerHero ]
    where
        txt str = return . const str



withEach :: [a -> b] -> a -> [b]
withEach = flip $ \a -> map ($ a)





