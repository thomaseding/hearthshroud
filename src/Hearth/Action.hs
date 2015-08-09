{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}


module Hearth.Action where


--------------------------------------------------------------------------------


import Hearth.Model


--------------------------------------------------------------------------------


data Action :: * where
    ActionPlayerConceded :: Handle Player -> Action
    ActionEndTurn :: Action
    ActionPlayMinion :: HandCard -> BoardPos -> Action
    ActionPlaySpell :: HandCard -> Action
    ActionAttack :: Handle Character -> Handle Character -> Action





