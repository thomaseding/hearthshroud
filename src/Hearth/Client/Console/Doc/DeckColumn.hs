{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}


module Hearth.Client.Console.Render.DeckColumn (
    deckColumn
) where


--------------------------------------------------------------------------------


import Control.Lens
import Hearth.Client.Console.SGRString
import Hearth.Engine
import Hearth.Model.Authoring
import Hearth.Model.Runtime
import System.Console.ANSI


--------------------------------------------------------------------------------


deckColumn :: (HearthMonad m) => Handle 'Player' -> Hearth m [SGRString]
deckColumn player = view $ getPlayer player.playerDeck.to deckColumn'


deckColumn' :: Deck -> [SGRString]
deckColumn' (Deck cs) = [
    sgrColor (Dull, Green) +++ "Deck",
    sgrColor (Dull, Green) +++ "----",
    sgrColor (Vivid, Green) +++ sgrShow (length cs) ]







