{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module Hearth.Client.Console.Render.PlayerColumn (
    playerColumn
) where


--------------------------------------------------------------------------------


import Hearth.Client.Console.Render.BoardHeroColumn
import Hearth.Client.Console.Render.DeckColumn
import Hearth.Client.Console.Render.HeroPowerColumn
import Hearth.Client.Console.Render.ManaColumn
import Hearth.Client.Console.Render.WeaponColumn
import Hearth.Client.Console.SGRString
import Hearth.Engine
import Hearth.Model.Authoring


--------------------------------------------------------------------------------


playerColumn :: (HearthMonad m) => Handle 'Player' -> Hearth m [SGRString]
playerColumn player = do
    deckCol <- deckColumn player
    manaCol <- manaColumn player
    heroCol <- boardHeroColumn player
    heroPowerCol <- heroPowerColumn player
    weaponCol <- weaponColumn player
    return $ concat [
        deckCol,
        txt "",
        manaCol,
        txt "",
        heroPowerCol,
        txt "",
        heroCol,
        txt "",
        weaponCol ]
    where
        txt str = [str]






