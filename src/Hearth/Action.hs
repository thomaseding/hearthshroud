{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}


module Hearth.Action where


--------------------------------------------------------------------------------


import GHC.Exts (Constraint)
import Hearth.Model


--------------------------------------------------------------------------------


data Action :: (* -> Constraint) -> * where
    ActionPlayerConceded :: Handle Player -> Action k
    ActionEndTurn :: Action k
    ActionPlayMinion :: HandCard k -> BoardIndex -> Action k
    ActionPlaySpell :: HandCard k -> Action k
    ActionPlayWeapon :: HandCard k -> Action k
    ActionAttack :: Handle Character -> Handle Character -> Action k
    ActionHeroPower :: Action k





