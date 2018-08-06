{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module Hearth.Client.Console.Doc.BoardHeroColumn (
    boardHeroColumn,
) where


--------------------------------------------------------------------------------


import Control.Lens
import Hearth.Client.Console.Doc.Display
import Hearth.Engine
import Hearth.Model.Authoring
import Hearth.Model.Runtime


--------------------------------------------------------------------------------


boardHeroColumn :: (HearthMonad m) => Handle 'Player' -> Hearth m (Doc Display)
boardHeroColumn player = dynamic $ do
    Attack attack <- viewAttack player
    Health health <- viewRemainingHealth $ PlayerCharacter player
    Armor armor <- view $ getPlayer player.playerHero.boardHeroArmor

    return $ vcat [
        labelColor "Attack",
        dashSeparator,
        valueColor attack,
        dashSeparator,
        labelColor "Health",
        dashSeparator,
        valueColor health,
        dashSeparator,
        labelColor "Armor",
        dashSeparator,
        valueColor armor ]


