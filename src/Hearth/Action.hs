{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}


module Hearth.Action where


--------------------------------------------------------------------------------


import Hearth.Model.Authoring
import Hearth.Model.Runtime


--------------------------------------------------------------------------------


data Action :: * where
    ActionPlayerConceded :: Handle 'Player' -> Action
    ActionEndTurn :: Action
    ActionPlayMinion :: HandCard -> BoardIndex -> Action
    ActionPlaySpell :: HandCard -> Action
    ActionPlayWeapon :: HandCard -> Action
    ActionAttack :: Handle 'Character' -> Handle 'Character' -> Action
    ActionHeroPower :: Action





