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


boardHeroColumn :: (HearthMonad k m) => PlayerObject k -> Hearth k m [SGRString]
boardHeroColumn player = do
    let pHandle = player^.playerHandle
        hero = player^.playerHero
    health <- dynamic $ viewRemainingHealth $ PlayerCharacter pHandle
    return $ concat [
        txt "Health",
        txt "------",
        toTxt $ unHealth health,
        txt "",
        txt "Armor",
        txt "-----",
        toTxt $ unArmor $ hero^.boardHeroArmor ]
    where
        txt str = [sgrColor (Dull, Green) ++ str]
        toTxt x = [sgrColor (Vivid, Green) ++ sgrShow x]












