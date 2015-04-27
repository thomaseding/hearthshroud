{-# LANGUAGE LambdaCase #-}


module Hearth.Client.Console.DeckColumn (
    deckColumn
) where


--------------------------------------------------------------------------------


import Control.Lens
import Data.List
import Hearth.Model
import Hearth.Cards


--------------------------------------------------------------------------------


deckColumn :: Deck -> [String]
deckColumn (Deck cs) = [
    "DECK",
    "----",
    show $ length cs ]







