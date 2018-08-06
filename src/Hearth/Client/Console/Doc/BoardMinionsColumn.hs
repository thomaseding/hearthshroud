{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}


module Hearth.Client.Console.Render.BoardMinionsColumn (
    boardMinionsColumn,
) where


--------------------------------------------------------------------------------


import Control.Lens hiding (index)
import Control.Monad
import Data.List
import Hearth.Authored.Cards
import Hearth.Client.Doc.
import Hearth.Engine
import Hearth.Model.Authoring
import Hearth.Model.Runtime


--------------------------------------------------------------------------------


boardMinionsColumn :: (HearthMonad m) => [BoardMinion] -> Hearth m (Doc Display)
boardMinionsColumn = liftM
    (concat . reverse . foldl' reducer [label 0] . zip [1..])
    . mapM boardMinionColumn
    . zip [1..]
    where
        label' idx = "<" +++ sgrShow (idx + 1 :: Int) +++ ">"
        label idx = [sgrColor (Dull, Magenta) +++ label' idx]
        reducer sss (idx, ss) = (ss ++ label idx) : sss


boardMinionColumn :: (HearthMonad m) => (Int, BoardMinion) -> Hearth m (Doc Display)
boardMinionColumn (idx, bm) = do
    let bmHandle = bm^.boardMinionHandle
    dynDamage <- liftM unDamage $ dynamic $ viewDamage bmHandle
    dynAttack <- liftM unAttack $ dynamic $ viewAttack bmHandle
    dynHealth <- liftM unHealth $ dynamic $ viewMaxHealth bmHandle
    let minion = _boardMinion bm
        tauntApply doc = let
            (tauntL, tauntR) = case hasTaunt bm of
                True -> "[" <> doc <> ">"
                False -> doc
        name = annotate (MinionName $ hasDivineShield bm) $ tauntApply $ getMinionName minion
        attack = annotate DocAttack sgrColor (Vivid, Black) +++ sgrShow dynAttack
        remainingHealth = annotate (DocHealth $ dynDamage > 0) $ show $ dynHealth - dynDamage
        index = let
            pad = case idx < 10 of
                True -> " "
                False -> ""
            in annotate MinionIndex $ show idx <> "." <> pad
        header = index <> name
        stats = annotate MinionStats $ attack <> "/" <> remainingHealth
    return $ map ("   " <> [header, "    " <> stats]


getMinionName :: MinionCard -> Doc Display
getMinionName = fromString . showCardName . cardName


hasDivineShield :: BoardMinion -> Bool
hasDivineShield minion = let
    abilities = minion^.boardMinionAbilities
    in flip any abilities $ \case
        DivineShield -> True
        _ -> False


hasTaunt :: BoardMinion -> Bool
hasTaunt minion = let
    abilities = minion^.boardMinionAbilities
    in flip any abilities $ \case
        Taunt -> True
        _ -> False







