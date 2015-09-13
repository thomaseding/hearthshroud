{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}


module Hearth.Action where


--------------------------------------------------------------------------------


import GHC.Prim (Constraint)
import Hearth.Model


--------------------------------------------------------------------------------


data Action :: (* -> Constraint) -> * where
    ActionPlayerConceded :: Handle Player -> Action c
    ActionEndTurn :: Action c
    ActionPlayMinion :: HandCard c -> BoardIndex -> Action c
    ActionPlaySpell :: HandCard c -> Action c
    ActionAttack :: Handle Character -> Handle Character -> Action c
    ActionHeroPower :: Action c





