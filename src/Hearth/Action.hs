{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}


module Hearth.Action where


--------------------------------------------------------------------------------


import Hearth.Model


--------------------------------------------------------------------------------


data Action :: * where
    ActionPlayerConceded :: PlayerHandle -> Action
    ActionEndTurn :: Action
    ActionPlayCard :: HandCard -> BoardPos -> Action
    ActionAttackMinion :: BoardMinion -> BoardMinion -> Action
    deriving (Show)





