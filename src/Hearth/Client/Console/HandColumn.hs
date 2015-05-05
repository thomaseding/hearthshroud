{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}


module Hearth.Client.Console.HandColumn (
    handColumn
) where


--------------------------------------------------------------------------------


import Control.Lens hiding (index)
import Data.List
import Data.String
import Hearth.Engine
import Hearth.Model
import Hearth.Client.Console.SGRString
import Hearth.Names
import System.Console.ANSI


--------------------------------------------------------------------------------


handColumn :: (HearthMonad m) => Hand -> Hearth m [SGRString]
handColumn (Hand cs) = return $ let
    cs' = map cardColumn $ zip [1..] $ reverse cs
    in concat $ intersperse [""] cs'


cardColumn :: (Int, HandCard) -> [SGRString]
cardColumn = \case
    (idx, HandCardMinion (HandMinion minion)) -> minionColumn (idx, minion)


minionColumn :: (Int, Minion) -> [SGRString]
minionColumn (idx, minion) = let
    parens s = "(" ++ s ++ ")"
    nameColor = case hasDivineShield minion of
        True -> sgrColor (Vivid, Red) ++ sgr [SetColor Background Vivid Yellow]
        False -> sgrColor (Vivid, Green)
    (tauntL, tauntR) = case hasTaunt minion of
        True -> ("[", ">")
        False -> ("", "")
    name = nameColor ++ tauntL ++ getMinionName minion ++ tauntR ++ sgr [SetColor Background Dull Black]
    mana = sgrColor (Vivid, White) ++ (parens $ sgrShow $ case minion^.minionCost of
        ManaCost (Mana m) -> m)
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
    in [header, "    " ++ stats]


getMinionName :: Minion -> SGRString
getMinionName minion = fromString $ case minion^.minionName of
    BasicCardName name -> show name
    ClassicCardName name -> show name


hasDivineShield :: Minion -> Bool
hasDivineShield minion = let
    abilities = minion^.minionAbilities
    in flip any abilities $ \case
        KeywordAbility DivineShield -> True
        _ -> False


hasTaunt :: Minion -> Bool
hasTaunt minion = let
    abilities = minion^.minionAbilities
    in flip any abilities $ \case
        KeywordAbility Taunt -> True
        _ -> False






