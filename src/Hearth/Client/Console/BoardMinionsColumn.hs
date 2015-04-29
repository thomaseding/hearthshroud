{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}


module Hearth.Client.Console.BoardMinionsColumn (
    boardMinionsColumn
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


boardMinionsColumn :: [BoardMinion] -> [SGRString]
boardMinionsColumn = concat . reverse . foldl' f [label 0] . zip [1..] . map boardMinionColumn . zip [1..]
    where
        labels = map (\c -> "<" ++ [c] ++ ">") "ABCDEFGH"
        label idx = [sgrColor (Dull, Magenta) ++ (labels !! idx)]
        f sss (idx, ss) = (ss ++ label idx) : sss


boardMinionColumn :: (Int, BoardMinion) -> [SGRString]
boardMinionColumn (idx, bm) = let
    minion = _boardMinion bm
    parens s = "(" ++ s ++ ")"
    name = sgrColor (Vivid, Green) ++ (sgrShow $ case minion^.minionName of
        BasicCardName name -> name)
    attack = sgrColor (Vivid, Black) ++ sgrShow (unAttack $ minion^.minionAttack)
    healthColor = case bm^.boardMinionCurrHealth < bm^.boardMinion^.minionHealth of
        True -> (Vivid, Red)
        False -> (Vivid, Black)
    health = sgrColor healthColor ++ sgrShow (unHealth $ minion^.minionHealth)
    index = let
        pad = if idx < 10 then " " else ""
        in sgrColor (Dull, Green) ++ sgrShow idx ++ "." ++ pad
    header = index ++ name
    stats = let
        c = sgrColor (Dull, White)
        in attack ++ c ++ "/" ++ health
    pad = if idx < 10 then " " else ""
    in map ("   " ++) [header, "    " ++ stats]





