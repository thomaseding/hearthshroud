{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}


module Hearth.Client.Console.HandColumn (
    handColumn
) where


--------------------------------------------------------------------------------


import Control.Lens
import Data.List
import Hearth.Model
import Hearth.Cards
import Hearth.Client.Console.SGRString
import Hearth.Names
import System.Console.ANSI


--------------------------------------------------------------------------------


handColumn :: Hand -> [SGRString]
handColumn (Hand cs) = let
    cs' = map cardColumn $ zip [1..] $ reverse cs
    in concat $ intersperse [""] cs'


cardColumn :: (Int, HandCard) -> [SGRString]
cardColumn = \case
    (idx, HandCardMinion (HandMinion minion)) -> minionColumn (idx, minion)


minionColumn :: (Int, Minion) -> [SGRString]
minionColumn (idx, minion) = let
    parens s = "(" ++ s ++ ")"
    name = sgrColor (Vivid, Green) ++ (sgrShow $ case minion^.minionName of
        BasicCardName name -> name)
    mana = sgrColor (Vivid, White) ++ (parens $ sgrShow $ case minion^.minionCost of
        ManaCost (Mana mana) -> mana)
    attack = sgrColor (Vivid, Black) ++ sgrShow (unAttack $ minion^.minionAttack)
    healthColor = (Vivid, Black)
    health = sgrColor healthColor ++ sgrShow (unHealth $ minion^.minionHealth)
    index = let
        pad = if idx < 10 then " " else ""
        in sgrColor (Dull, Green) ++ sgrShow idx ++ "." ++ pad
    header = index ++ name ++ " " ++ mana
    stats = let
        c = sgrColor (Dull, White)
        in attack ++ c ++ "/" ++ health
    pad = if idx < 10 then " " else ""
    in [header, "    " ++ stats]







