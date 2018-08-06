{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}


module Hearth.Client.Console.Render.WeaponColumn (
    weaponColumn
) where


--------------------------------------------------------------------------------


import Control.Lens
import Data.String
import Hearth.Client.Console.SGRString
import Hearth.Engine
import Hearth.Model.Authoring
import Hearth.Model.Runtime
import System.Console.ANSI


--------------------------------------------------------------------------------


weaponColumn :: (HearthMonad m) => Handle 'Player' -> Hearth m [SGRString]
weaponColumn player = dynamic $ do
    weaponStr <- view (getPlayer player.playerWeapon) >>= return . \case
        Nothing -> "None"
        Just weapon -> let
            Attack attack = weapon^.boardWeapon.weaponAttack
            Durability durability = weapon^.boardWeaponDurability
            in show attack ++ "/" ++ show durability
    return [
        sgrColor (Dull, Green) +++ "Weapon",
        sgrColor (Dull, Green) +++ "------",
        sgrColor (Vivid, Green) +++ fromString weaponStr ]







