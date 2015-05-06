{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}


module Hearth.Action where


--------------------------------------------------------------------------------


import Hearth.Model


--------------------------------------------------------------------------------


data Action :: * where
    ActionPlayerConceded :: PlayerHandle -> Action
    ActionEndTurn :: Action
    ActionPlayMinion :: HandCard -> BoardPos -> Action
    ActionPlaySpell :: HandCard -> Action
    ActionAttackMinion :: BoardMinion -> BoardMinion -> Action
    deriving (Show)





