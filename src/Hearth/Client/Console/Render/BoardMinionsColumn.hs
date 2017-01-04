{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}


module Hearth.Client.Console.Render.BoardMinionsColumn (
    boardMinionsColumn
) where


--------------------------------------------------------------------------------


import Control.Lens hiding (index)
import Control.Monad
import Data.List
import Data.String
import Hearth.Engine
import Hearth.Model
import Hearth.CardName
import Hearth.Cards
import Hearth.Client.Console.SGRString
import System.Console.ANSI


--------------------------------------------------------------------------------


boardMinionsColumn :: (HearthMonad k m) => [BoardMinion k] -> Hearth k m [SGRString]
boardMinionsColumn = liftM (concat . reverse . foldl' f [label 0] . zip [1..]) . mapM boardMinionColumn . zip [1..]
    where
        label' idx = "<" +++ sgrShow (idx + 1 :: Int) +++ ">"
        label idx = [sgrColor (Dull, Magenta) +++ label' idx]
        f sss (idx, ss) = (ss ++ label idx) : sss


boardMinionColumn :: (HearthMonad k m) => (Int, BoardMinion k) -> Hearth k m [SGRString]
boardMinionColumn (idx, bm) = do
    let bmHandle = bm^.boardMinionHandle
    dynDamage <- liftM unDamage $ dynamic $ viewDamage bmHandle
    dynAttack <- liftM unAttack $ dynamic $ viewAttack bmHandle
    dynHealth <- liftM unHealth $ dynamic $ viewMaxHealth bmHandle
    let minion = _boardMinion bm
        nameColor = case hasDivineShield bm of
            True -> sgrColor (Vivid, Red) +++ sgr [SetColor Background Vivid Yellow]
            False -> sgrColor (Vivid, Green)
        (tauntL, tauntR) = case hasTaunt bm of
            True -> ("[", ">")
            False -> ("", "")
        name = nameColor +++ tauntL +++ getMinionName minion +++ tauntR +++ sgr [SetColor Background Dull Black]
        attack = sgrColor (Vivid, Black) +++ sgrShow dynAttack
        healthColor = case dynDamage > 0 of
            True -> (Vivid, Red)
            False -> (Vivid, Black)
        index = let
            pad = if idx < 10 then " " else ""
            in sgrColor (Dull, Green) +++ sgrShow idx +++ "." +++ pad
        header = index +++ name
        remainingHealth = sgrColor healthColor +++ sgrShow (dynHealth - dynDamage)
        stats = let
            c = sgrColor (Dull, White)
            in attack +++ c +++ "/" +++ remainingHealth
    return $ map ("   " +++) [header, "    " +++ stats]


getMinionName :: MinionCard k -> SGRString
getMinionName = fromString . showCardName . cardName


hasDivineShield :: BoardMinion k -> Bool
hasDivineShield minion = let
    abilities = minion^.boardMinionAbilities
    in flip any abilities $ \case
        DivineShield -> True
        _ -> False


hasTaunt :: BoardMinion k -> Bool
hasTaunt minion = let
    abilities = minion^.boardMinionAbilities
    in flip any abilities $ \case
        Taunt -> True
        _ -> False







