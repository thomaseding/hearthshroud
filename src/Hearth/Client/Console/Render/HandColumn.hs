{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}


module Hearth.Client.Console.Render.HandColumn (
    handColumn
) where


--------------------------------------------------------------------------------


import Control.Lens hiding (index)
import Data.List
import Data.String
import Hearth.Engine
import Hearth.Model
import Hearth.CardName
import Hearth.Cards
import Hearth.Client.Console.SGRString
import System.Console.ANSI


--------------------------------------------------------------------------------


handColumn :: (HearthMonad c m) => Hand c -> Hearth c m [SGRString]
handColumn (Hand cs) = return $ let
    cs' = map (uncurry cardColumn) $ zip [1..] $ reverse cs
    in concat $ intersperse [""] cs'


cardColumn :: Int -> HandCard c -> [SGRString]
cardColumn idx = \case
    HandCardMinion minion -> minionColumn idx minion
    HandCardSpell spell -> spellColumn idx spell


minionColumn :: Int -> MinionCard c -> [SGRString]
minionColumn idx minion = let
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


spellColumn :: Int -> SpellCard c -> [SGRString]
spellColumn idx spell = let
    nameColor = sgrColor (Vivid, Green)
    name = nameColor ++ getSpellName spell
    mana = sgrColor (Vivid, White) ++ (parens $ sgrShow $ case spell^.spellCost of
        ManaCost (Mana m) -> m)
    index = let
        pad = if idx < 10 then " " else ""
        in sgrColor (Dull, Green) ++ sgrShow idx ++ "." ++ pad
    header = index ++ name ++ " " ++ mana
    in [header, "    Spell"]


getSpellName :: SpellCard c -> SGRString
getSpellName = fromString . showCardName . cardName


getMinionName :: MinionCard c -> SGRString
getMinionName = fromString . showCardName . cardName


hasDivineShield :: MinionCard c -> Bool
hasDivineShield minion = let
    abilities = minion^.minionAbilities
    in flip any abilities $ \case
        DivineShield -> True
        _ -> False


hasTaunt :: MinionCard c -> Bool
hasTaunt minion = let
    abilities = minion^.minionAbilities
    in flip any abilities $ \case
        Taunt -> True
        _ -> False


parens :: SGRString -> SGRString
parens s = "(" ++ s ++ ")"



