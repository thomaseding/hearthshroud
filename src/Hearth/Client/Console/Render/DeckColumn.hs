{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}


module Hearth.Client.Console.Render.DeckColumn (
    deckColumn
) where


--------------------------------------------------------------------------------


import Hearth.Model
import Hearth.Client.Console.SGRString
import System.Console.ANSI


--------------------------------------------------------------------------------


deckColumn :: Deck c -> [SGRString]
deckColumn (Deck cs) = [
    sgrColor (Dull, Green) ++ "Deck",
    sgrColor (Dull, Green) ++ "----",
    sgrColor (Vivid, Green) ++ sgrShow (length cs) ]







