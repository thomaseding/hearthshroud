{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}


module Hearth.Client.Console.Render.ManaColumn (
    manaColumn
) where


--------------------------------------------------------------------------------


import Control.Lens
import Hearth.Client.Console.SGRString
import Hearth.Engine
import Hearth.Model.Authoring
import Hearth.Model.Runtime
import System.Console.ANSI


--------------------------------------------------------------------------------


manaColumn :: (HearthMonad m) => Handle 'Player' -> Hearth m [SGRString]
manaColumn player = view $ getPlayer player.to manaColumn'


manaColumn' :: PlayerObject -> [SGRString]
manaColumn' player = let
    totalMana = player^.playerTotalManaCrystals
    emptyMana = player^.playerEmptyManaCrystals
    currMana = totalMana - emptyMana
    in [sgrColor (Dull, Green) +++ "Mana",
        sgrColor (Dull, Green) +++ "----",
        sgrColor (Vivid, Green) +++ sgrShow currMana +++ ":" +++ sgrShow totalMana ]







