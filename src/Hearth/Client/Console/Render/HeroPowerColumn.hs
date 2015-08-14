{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}


module Hearth.Client.Console.Render.HeroPowerColumn (
    heroPowerColumn
) where


--------------------------------------------------------------------------------


import Control.Lens
import Hearth.Model
import Hearth.Client.Console.SGRString
import System.Console.ANSI


--------------------------------------------------------------------------------


heroPowerColumn :: Player -> [SGRString]
heroPowerColumn player = let
    hero = player^.playerHero
    count = hero^.boardHeroPowerCount
    powerStr = case count of
        0 -> "[*]"
        _ -> "[ ]"
    in [sgrColor (Dull, Green) ++ "Power",
        sgrColor (Dull, Green) ++ "-----",
        sgrColor (Vivid, Green) ++ powerStr ]







