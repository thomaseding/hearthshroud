{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Hearth.Cards
import Hearth.Client.Console.SGRString
import System.Console.ANSI


--------------------------------------------------------------------------------


boardMinionsColumn :: (HearthMonad m) => [BoardMinion] -> Hearth m [SGRString]
boardMinionsColumn = liftM (concat . reverse . foldl' f [label 0] . zip [1..]) . mapM boardMinionColumn . zip [1..]
    where
        label' idx = "<" ++ sgrShow (idx + 1 :: Int) ++ ">"
        label idx = [sgrColor (Dull, Magenta) ++ label' idx]
        f sss (idx, ss) = (ss ++ label idx) : sss


boardMinionColumn :: (HearthMonad m) => (Int, BoardMinion) -> Hearth m [SGRString]
boardMinionColumn (idx, bm) = do
    let bmHandle = bm^.boardMinionHandle
    dmg <- liftM unDamage $ getDamage bmHandle
    dynAttack <- liftM unAttack $ dynamicAttack bmHandle
    dynHealth <- liftM unHealth $ dynamicMaxHealth bmHandle
    let minion = _boardMinion bm
        nameColor = case hasDivineShield bm of
            True -> sgrColor (Vivid, Red) ++ sgr [SetColor Background Vivid Yellow]
            False -> sgrColor (Vivid, Green)
        (tauntL, tauntR) = case hasTaunt bm of
            True -> ("[", ">")
            False -> ("", "")
        name = nameColor ++ tauntL ++ getMinionName minion ++ tauntR ++ sgr [SetColor Background Dull Black]
        attack = sgrColor (Vivid, Black) ++ sgrShow dynAttack
        healthColor = case bm^.boardMinionDamage > 0 of
            True -> (Vivid, Red)
            False -> (Vivid, Black)
        index = let
            pad = if idx < 10 then " " else ""
            in sgrColor (Dull, Green) ++ sgrShow idx ++ "." ++ pad
        header = index ++ name
        remainingHealth = sgrColor healthColor ++ sgrShow (dynHealth - dmg)
        stats = let
            c = sgrColor (Dull, White)
            in attack ++ c ++ "/" ++ remainingHealth
    return $ map ("   " ++) [header, "    " ++ stats]


getMinionName :: Minion -> SGRString
getMinionName = fromString . show . cardName


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







