{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module Hearth.Client.Console.Render.PlayerColumn (
    playerColumn
) where


--------------------------------------------------------------------------------


import Control.Lens
import Hearth.Engine
import Hearth.Model
import Hearth.Client.Console.Render.BoardHeroColumn
import Hearth.Client.Console.Render.DeckColumn
import Hearth.Client.Console.Render.ManaColumn
import Hearth.Client.Console.SGRString


--------------------------------------------------------------------------------


playerColumn :: (HearthMonad m) => Player -> Hearth m [SGRString]
playerColumn player = do
    bhc <- boardHeroColumn player
    return $ concat [
        deckColumn $ player^.playerDeck,
        txt "",
        manaColumn player,
        txt "",
        bhc ]
    where
        txt str = [str]






