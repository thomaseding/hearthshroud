{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}


module Hearth.Client.Console.DeckColumn (
    deckColumn
) where


--------------------------------------------------------------------------------


import Control.Lens
import Data.List
import Hearth.Model
import Hearth.Cards
import Hearth.Client.Console.SGRString
import System.Console.ANSI


--------------------------------------------------------------------------------


deckColumn :: Deck -> [SGRString]
deckColumn (Deck cs) = [
    sgrColor (Dull, Green) ++ "Deck",
    sgrColor (Dull, Green) ++ "----",
    sgrColor (Vivid, Green) ++ sgrShow (length cs) ]







