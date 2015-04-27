{-# LANGUAGE LambdaCase #-}


module Hearth.Client.Console.HandColumn (
    handColumn
) where


--------------------------------------------------------------------------------


import Control.Lens
import Data.List
import Hearth.Model
import Hearth.Cards
import Hearth.Names


--------------------------------------------------------------------------------


handColumn :: Hand -> [String]
handColumn (Hand cs) = let
    cs' = map cardColumn $ reverse cs
    in concat $ intersperse [""] cs'


cardColumn :: HandCard -> [String]
cardColumn = \case
    HandCardMinion (HandMinion minion) -> minionColumn minion


minionColumn :: Minion -> [String]
minionColumn minion = let
    parens s = "(" ++ s ++ ")"
    name = show $ case minion^.minionName of
        BasicCardName name -> name
    mana = parens $ show $ case minion^.minionCost of
        ManaCost (Mana mana) -> mana
    attack = show $ unAttack $ minion^.minionAttack
    health = show $ unHealth $ minion^.minionHealth
    header = unwords [name, mana]
    stats = attack ++ "/" ++ health
    in [header, stats]




