{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}


module Hearth.Client.Console.Render.DeckColumn (
    deckColumn
) where


--------------------------------------------------------------------------------


import Control.Lens
import Hearth.Engine
import Hearth.Model
import Hearth.Client.Console.SGRString
import System.Console.ANSI


--------------------------------------------------------------------------------


deckColumn :: (HearthMonad k m) => Handle Player -> Hearth k m [SGRString]
deckColumn player = view $ getPlayer player.playerDeck.to deckColumn'


deckColumn' :: Deck k -> [SGRString]
deckColumn' (Deck cs) = [
    sgrColor (Dull, Green) ++ "Deck",
    sgrColor (Dull, Green) ++ "----",
    sgrColor (Vivid, Green) ++ sgrShow (length cs) ]







