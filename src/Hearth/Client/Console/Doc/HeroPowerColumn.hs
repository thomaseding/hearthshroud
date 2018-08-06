{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}


module Hearth.Client.Console.Render.HeroPowerColumn (
    heroPowerColumn
) where


--------------------------------------------------------------------------------


import Control.Lens
import Hearth.Client.Console.SGRString
import Hearth.Engine
import Hearth.Model.Authoring
import Hearth.Model.Runtime
import System.Console.ANSI


--------------------------------------------------------------------------------


heroPowerColumn :: (HearthMonad m) => Handle 'Player' -> Hearth m [SGRString]
heroPowerColumn player = view $ getPlayer player.to heroPowerColumn'


heroPowerColumn' :: PlayerObject -> [SGRString]
heroPowerColumn' player = let
    hero = player^.playerHero
    count = hero^.boardHeroPowerCount
    powerStr = case count of
        0 -> "[*]"
        _ -> "[ ]"
    in [sgrColor (Dull, Green) +++ "Power",
        sgrColor (Dull, Green) +++ "-----",
        sgrColor (Vivid, Green) +++ powerStr ]







