{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}


module Hearth.Client.Console.Choices where


--------------------------------------------------------------------------------


import Control.Lens
import Control.Monad.Reader
import Hearth.Engine
import Hearth.Model


--------------------------------------------------------------------------------


possibleAttacks :: (HearthMonad k m) => Hearth k m [(Handle Character, Handle Character)]
possibleAttacks = do
    activeHandle <- getActivePlayerHandle
    activeMinions' <- view $ getPlayer activeHandle.playerMinions
    nonActiveHandle <- getNonActivePlayerHandle
    nonActiveMinions' <- view $ getPlayer nonActiveHandle.playerMinions
    let activeMinions = map characterHandle activeMinions'
        nonActiveMinions = map characterHandle nonActiveMinions'
        pairs = (PlayerCharacter activeHandle, PlayerCharacter nonActiveHandle)
             : [(a, na) | a <- activeMinions, na <- nonActiveMinions]
            ++ [(PlayerCharacter activeHandle, na) | na <- nonActiveMinions]
            ++ [(a, PlayerCharacter nonActiveHandle) | a <- activeMinions]
    flip filterM pairs $ \(activeChar, nonActiveChar) -> local id $ do
        enactAttack activeChar nonActiveChar >>= \case
            Failure {} -> return False
            Success -> return True


playableMinions :: (HearthMonad k m) => Hearth k m [(HandCard k, BoardIndex)]
playableMinions = do
    handle <- getActivePlayerHandle
    cards <- view $ getPlayer handle.playerHand.handCards
    maxPos <- view $ getPlayer handle.playerMinions.to (BoardIndex . length)
    let positions = [BoardIndex 0 .. maxPos]
    liftM concat $ forM positions $ \pos -> do
        liftM (map (, pos)) $ flip filterM (reverse cards) $ \card -> local id $ do
            playMinion handle card pos >>= \case
                Failure {} -> return False
                Success -> return True


playableSpells :: (HearthMonad k m) => Hearth k m [HandCard k]
playableSpells = do
    handle <- getActivePlayerHandle
    cards <- view $ getPlayer handle.playerHand.handCards
    flip filterM (reverse cards) $ \card -> local id $ do
        playSpell handle card >>= \case
            Failure {} -> return False
            Success -> return True


