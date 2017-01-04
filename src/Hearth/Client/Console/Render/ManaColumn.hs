{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}


module Hearth.Client.Console.Render.ManaColumn (
    manaColumn
) where


--------------------------------------------------------------------------------


import Control.Lens
import Hearth.Engine
import Hearth.Model
import Hearth.Client.Console.SGRString
import System.Console.ANSI


--------------------------------------------------------------------------------


manaColumn :: (HearthMonad k m) => Handle Player -> Hearth k m [SGRString]
manaColumn player = view $ getPlayer player.to manaColumn'


manaColumn' :: PlayerObject k -> [SGRString]
manaColumn' player = let
    totalMana = player^.playerTotalManaCrystals
    emptyMana = player^.playerEmptyManaCrystals
    currMana = totalMana - emptyMana
    in [sgrColor (Dull, Green) +++ "Mana",
        sgrColor (Dull, Green) +++ "----",
        sgrColor (Vivid, Green) +++ sgrShow currMana +++ ":" +++ sgrShow totalMana ]







