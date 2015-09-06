{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}


module Hearth.Action where


--------------------------------------------------------------------------------


import Hearth.Model


--------------------------------------------------------------------------------


data Action :: * where
    ActionPlayerConceded :: Handle Player -> Action
    ActionEndTurn :: Action
    ActionPlayMinion :: HandCard -> BoardIndex -> Action
    ActionPlaySpell :: HandCard -> Action
    ActionAttack :: Handle Character -> Handle Character -> Action
    ActionHeroPower :: Action





