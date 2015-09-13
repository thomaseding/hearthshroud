{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module Hearth.Client.Console.Render.BoardHeroColumn (
    boardHeroColumn
) where


--------------------------------------------------------------------------------


import Control.Lens
import Hearth.Engine
import Hearth.Model
import Hearth.Client.Console.SGRString
import System.Console.ANSI


--------------------------------------------------------------------------------


boardHeroColumn :: (HearthMonad k m) => Handle Player -> Hearth k m [SGRString]
boardHeroColumn player = dynamic $ do
    Attack attack <- viewAttack player
    Health health <- viewRemainingHealth $ PlayerCharacter player
    Armor armor <- view $ getPlayer player.playerHero.boardHeroArmor
    return $ concat [
        txt "Attack",
        txt "------",
        toTxt attack,
        txt "",
        txt "Health",
        txt "------",
        toTxt health,
        txt "",
        txt "Armor",
        txt "-----",
        toTxt armor ]
    where
        txt str = [sgrColor (Dull, Green) ++ str]
        toTxt x = [sgrColor (Vivid, Green) ++ sgrShow x]












