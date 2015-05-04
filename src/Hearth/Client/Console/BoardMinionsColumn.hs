{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}


module Hearth.Client.Console.BoardMinionsColumn (
    boardMinionsColumn
) where


--------------------------------------------------------------------------------


import Control.Lens hiding (index)
import Data.List
import Data.String
import Hearth.Model
import Hearth.Client.Console.SGRString
import Hearth.Names
import System.Console.ANSI


--------------------------------------------------------------------------------


boardMinionsColumn :: [BoardMinion] -> [SGRString]
boardMinionsColumn = concat . reverse . foldl' f [label 0] . zip [1..] . map boardMinionColumn . zip [1..]
    where
        label' idx = "<" ++ sgrShow (idx + 1 :: Int) ++ ">"
        label idx = [sgrColor (Dull, Magenta) ++ label' idx]
        f sss (idx, ss) = (ss ++ label idx) : sss


boardMinionColumn :: (Int, BoardMinion) -> [SGRString]
boardMinionColumn (idx, bm) = let
    minion = _boardMinion bm
    nameColor = case hasDivineShield bm of
        True -> sgrColor (Vivid, Red) ++ sgr [SetColor Background Vivid Yellow]
        False -> sgrColor (Vivid, Green)
    (tauntL, tauntR) = case hasTaunt bm of
        True -> ("[", ">")
        False -> ("", "")
    name = nameColor ++ tauntL ++ getMinionName minion ++ tauntR ++ sgr [SetColor Background Dull Black]
    attack = sgrColor (Vivid, Black) ++ sgrShow (unAttack $ minion^.minionAttack)
    healthColor = case bm^.boardMinionCurrHealth < minion^.minionHealth of
        True -> (Vivid, Red)
        False -> (Vivid, Black)
    health = sgrColor healthColor ++ sgrShow (unHealth $ bm^.boardMinionCurrHealth)
    index = let
        pad = if idx < 10 then " " else ""
        in sgrColor (Dull, Green) ++ sgrShow idx ++ "." ++ pad
    header = index ++ name
    stats = let
        c = sgrColor (Dull, White)
        in attack ++ c ++ "/" ++ health
    in map ("   " ++) [header, "    " ++ stats]


getMinionName :: Minion -> SGRString
getMinionName minion = fromString $ case minion^.minionName of
    BasicCardName name -> show name
    ClassicCardName name -> show name


hasDivineShield :: BoardMinion -> Bool
hasDivineShield minion = let
    abilities = minion^.boardMinionAbilities
    in flip any abilities $ \case
        KeywordAbility DivineShield -> True
        _ -> False


hasTaunt :: BoardMinion -> Bool
hasTaunt minion = let
    abilities = minion^.boardMinionAbilities
    in flip any abilities $ \case
        KeywordAbility Taunt -> True
        _ -> False







