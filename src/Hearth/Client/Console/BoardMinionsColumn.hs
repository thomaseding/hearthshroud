{-# LANGUAGE LambdaCase #-}


module Hearth.Client.Console.BoardMinionsColumn (
    boardMinionsColumn
) where


--------------------------------------------------------------------------------


import Control.Lens
import Data.List
import Hearth.Model
import Hearth.Cards
import Hearth.Names


--------------------------------------------------------------------------------


boardMinionsColumn :: [BoardMinion] -> [String]
boardMinionsColumn = intercalate [[]] . map boardMinionColumn


boardMinionColumn :: BoardMinion -> [String]
boardMinionColumn bm = let
    minion = _boardMinion bm
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





