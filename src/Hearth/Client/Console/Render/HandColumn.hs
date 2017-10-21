{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
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
import Hearth.Authored.Cards
import Hearth.Client.Console.SGRString
import Hearth.Engine
import Hearth.Model.Authoring
import Hearth.Model.Runtime
import System.Console.ANSI


--------------------------------------------------------------------------------


handColumn :: (HearthMonad m) => Hand -> Hearth m [SGRString]
handColumn (Hand cs) = return $ let
    cs' = map (uncurry cardColumn) $ zip [1..] $ reverse cs
    in concat $ intersperse [""] cs'


cardColumn :: Int -> HandCard -> [SGRString]
cardColumn idx = \case
    HandCardMinion minion -> minionColumn idx minion
    HandCardSpell spell -> spellColumn idx spell
    HandCardWeapon weapon -> weaponColumn idx weapon


minionColumn :: Int -> MinionCard -> [SGRString]
minionColumn idx minion = let
    nameColor = case hasDivineShield minion of
        True -> sgrColor (Vivid, Red) +++ sgr [SetColor Background Vivid Yellow]
        False -> sgrColor (Vivid, Green)
    (tauntL, tauntR) = case hasTaunt minion of
        True -> ("[", ">")
        False -> ("", "")
    name = nameColor +++ tauntL +++ getName minion +++ tauntR +++ sgr [SetColor Background Dull Black]
    mana = sgrColor (Vivid, White) +++ (parens $ sgrShow $ case minion^.minionCost of
        ManaCost (Mana m) -> m)
    attack = sgrColor (Vivid, Black) +++ sgrShow (unAttack $ minion^.minionAttack)
    healthColor = (Vivid, Black)
    health = sgrColor healthColor +++ sgrShow (unHealth $ minion^.minionHealth)
    index = let
        pad = if idx < 10 then " " else ""
        in sgrColor (Dull, Green) +++ sgrShow idx +++ "." +++ pad
    header = index +++ name +++ " " +++ mana
    stats = let
        c = sgrColor (Dull, White)
        in attack +++ c +++ "/" +++ health
    in [header, "    " +++ stats]


weaponColumn :: Int -> WeaponCard -> [SGRString]
weaponColumn idx weapon = let
    nameColor = sgrColor (Vivid, Green)
    name = nameColor +++ getName weapon
    mana = sgrColor (Vivid, White) +++ (parens $ sgrShow $ case weapon^.weaponCost of
        ManaCost (Mana m) -> m)
    attack = sgrColor (Vivid, Black) +++ sgrShow (unAttack $ weapon^.weaponAttack)
    durabilityColor = (Vivid, Black)
    durability = sgrColor durabilityColor +++ sgrShow (unDurability $ weapon^.weaponDurability)
    index = let
        pad = if idx < 10 then " " else ""
        in sgrColor (Dull, Green) +++ sgrShow idx +++ "." +++ pad
    header = index +++ name +++ " " +++ mana
    stats = let
        c = sgrColor (Dull, White)
        in attack +++ c +++ "/" +++ durability
    in [header, "    " +++ stats]


spellColumn :: Int -> SpellCard -> [SGRString]
spellColumn idx spell = let
    nameColor = sgrColor (Vivid, Green)
    name = nameColor +++ getName spell
    mana = sgrColor (Vivid, White) +++ (parens $ sgrShow $ case spell^.spellCost of
        ManaCost (Mana m) -> m)
    index = let
        pad = if idx < 10 then " " else ""
        in sgrColor (Dull, Green) +++ sgrShow idx +++ "." +++ pad
    header = index +++ name +++ " " +++ mana
    in [header, "    Spell"]


getName :: (GetCardName a) => a -> SGRString
getName = fromString . showCardName . cardName


hasDivineShield :: MinionCard -> Bool
hasDivineShield minion = let
    abilities = minion^.minionAbilities
    in flip any abilities $ \case
        DivineShield -> True
        _ -> False


hasTaunt :: MinionCard -> Bool
hasTaunt minion = let
    abilities = minion^.minionAbilities
    in flip any abilities $ \case
        Taunt -> True
        _ -> False


parens :: SGRString -> SGRString
parens s = "(" +++ s +++ ")"



