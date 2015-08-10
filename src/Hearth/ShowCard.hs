{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}


module Hearth.ShowCard (
    showCard,
) where


--------------------------------------------------------------------------------


import Control.Applicative
import Control.Error.TH
import Control.Monad.State
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy
import Hearth.Model
import Hearth.Names


--------------------------------------------------------------------------------


data ShowState = ShowState {
    handleToString :: Map RawHandle String,
    handleSeed :: Int
} deriving (Show, Eq, Ord)


newtype ShowCard a = ShowCard {
    unShowCard :: State ShowState a
} deriving (Functor, Applicative, Monad, MonadState ShowState)


runShowCard :: ShowCard a -> a
runShowCard m = evalState (unShowCard m) $ ShowState {
    handleToString = Map.empty,
    handleSeed = 0 }


--------------------------------------------------------------------------------


rawGenHandle :: String -> ShowCard RawHandle
rawGenHandle str = do
    n <- gets handleSeed
    let handle = RawHandle n
    modify $ \st -> st {
        handleToString = Map.insert handle str $ handleToString st,
        handleSeed = n + 1 }
    return handle


class GenHandle a where
    genHandle :: String -> ShowCard (Handle a)


instance GenHandle Spell where
    genHandle = liftM SpellHandle . rawGenHandle


instance GenHandle Minion where
    genHandle = liftM MinionHandle . rawGenHandle


instance GenHandle Player where
    genHandle = liftM PlayerHandle . rawGenHandle


instance GenHandle Character where
    genHandle = liftM PlayerCharacter . genHandle


genNumberedHandle :: (GenHandle a) => String -> ShowCard (Handle a)
genNumberedHandle str = do
    n <- gets handleSeed
    genHandle $ str ++ "_" ++ show n


--------------------------------------------------------------------------------


rawReadHandle :: RawHandle -> ShowCard String
rawReadHandle handle = do
    mStr <- gets $ Map.lookup handle . handleToString
    case mStr of
        Nothing -> $logicError 'rawReadHandle "Trying to read from a non-generated handle?"
        Just str -> return str


readHandle :: Handle a -> ShowCard String
readHandle = applyRawHandle rawReadHandle


--------------------------------------------------------------------------------


itemize :: [String] -> String
itemize = \case
    [] -> ""
    [x] -> x
    [x, y] -> x ++ " and " ++ y
    [x, y, z] -> x ++ ", " ++ y ++ ", and " ++ z
    x : rest -> x ++ ", " ++ itemize rest


is :: String -> String -> Bool
is = (==)


this :: String
this = "THIS"


you :: String
you = "YOU"


opponent :: String
opponent = "OPPONENT"


--------------------------------------------------------------------------------


showCard :: HandCard -> String
showCard card = let
    name = showName card
    cost = showCost card
    bt = boxText card
    mStats = case card of
        HandCardMinion minion -> let
            Attack atk = _minionAttack minion
            Health hlt = _minionHealth minion
            in Just (atk, hlt)
        _ -> Nothing
    stats = case mStats of
        Nothing -> ""
        Just (atk, hlt) -> show atk ++ "/" ++ show hlt
    in unlines $ filter (/= "") $ lines $ unlines [
        name ++ " " ++ cost,
        bt,
        stats ]


showName :: HandCard -> String
showName card = case handCardName card of
    BasicCardName name -> show name
    ClassicCardName name -> show name


showCost :: HandCard -> String
showCost card = let
    cost = case card of
        HandCardMinion minion -> _minionCost minion
        HandCardSpell spell -> _spellCost spell
    in case cost of
        ManaCost (Mana mana) -> "(" ++ show mana ++ ")"


boxText :: HandCard -> String
boxText = runShowCard . liftM (unlines . filter (not . null) . lines) . \case
    HandCardMinion minion -> showAbilities $ _minionAbilities minion
    HandCardSpell spell -> genHandle this >>= showElect . _spellEffect spell


--------------------------------------------------------------------------------


showAbilities :: [Ability] -> ShowCard String
showAbilities = liftM unlines . mapM showAbility


showAbility :: Ability -> ShowCard String
showAbility = \case
    KeywordAbility ability -> showKeywordAbility ability


showKeywordAbility :: KeywordAbility -> ShowCard String
showKeywordAbility = \case
    Battlecry cont -> showBattlecry cont
    Deathrattle cont -> showDeathrattle cont
    Charge -> return "Charge"
    DivineShield -> return "Divine Shield"
    Enrage abilities enchantments -> showEnrage abilities enchantments
    Taunt -> return "Taunt"


showEnrage :: [Ability] -> [Enchantment] -> ShowCard String
showEnrage abilities enchantments = do
    asStr <- mapM showAbility abilities
    esStr <- mapM showEnchantment enchantments
    return $ "Enrage: " ++ itemize (asStr ++ esStr)


showDeathrattle :: (Handle Minion -> Elect AtRandom) -> ShowCard String
showDeathrattle cont = do
    effectStr <- genHandle this >>= showElect . cont
    return $ "Deathrattle: " ++ effectStr


showBattlecry :: (Handle Minion -> Elect Targeted) -> ShowCard String
showBattlecry cont = do
    effectStr <- genHandle this >>= showElect . cont
    return $ "Battlecry: " ++ effectStr


showEffect :: Effect -> ShowCard String
showEffect = \case
    AtRandom elect -> showElect elect
    ForEach handles cont -> showForEach handles cont
    Sequence effects -> showSequence effects
    DrawCards handle n -> showDrawCards handle n
    KeywordEffect effect -> showKeywordEffect effect
    DealDamage handle damage -> showDealDamage handle damage
    Enchant handle enchantments -> showEnchant handle enchantments
    GiveAbility handle abilities -> showGiveAbility handle abilities
    GainManaCrystal crystalState handle -> showGainManaCrystal crystalState handle
    DestroyMinion handle -> showDestroyMinion handle
    RestoreHealth handle amount -> showRestoreHealth handle amount


showRestoreHealth :: Handle Character -> Int -> ShowCard String
showRestoreHealth character amount = do
    characterStr <- readHandle character
    return $ "Restore " ++ show amount ++ " health on " ++ characterStr


showDestroyMinion :: Handle Minion -> ShowCard String
showDestroyMinion minion = do
    minionStr <- readHandle minion
    return $ "Destroy " ++ minionStr


showForEach :: [Handle a] -> (Handle a -> Effect) -> ShowCard String
showForEach handles cont = case handles of
    [handle] -> do
        str <- readHandle handle
        effectStr <- showEffect $ cont handle
        return $ "ForEach " ++ str ++ "s: " ++ effectStr
    _ -> $logicError 'showForEach "xxx"


showElect :: (IsSelection s) => Elect s -> ShowCard String
showElect = \case
    A x -> showA x
    All x -> showAll x
    Effect x -> showEffect x
    OwnerOf handle cont -> showOwnerOf handle cont
    OpponentOf handle cont -> showOpponentOf handle cont


showA :: (IsSelection s) => A s -> ShowCard String
showA = \case
    Minion restrictions cont -> showMinion restrictions cont
    Player restrictions cont -> showPlayer restrictions cont
    Character restrictions cont -> showCharacter restrictions cont


showPlayer :: forall s. (IsSelection s) => [Restriction Player] -> (Handle Player -> Elect s) -> ShowCard String
showPlayer restrictions cont = do
    restrictionsStr <- showRestrictions restrictions
    let sel = showSelection (Proxy :: Proxy s)
    handle <- genNumberedHandle $ sel ++ "PLAYER[" ++ restrictionsStr ++ "]"
    showElect $ cont handle


showAll :: (IsSelection s) => All s -> ShowCard String
showAll = \case
    Minions restrictions cont -> showMinions restrictions cont
    Players restrictions cont -> showPlayers restrictions cont
    Characters restrictions cont -> showCharacters restrictions cont


class IsSelection s where
    showSelection :: Proxy s -> String


instance IsSelection Targeted where
    showSelection _ = "TARGET_"


instance IsSelection AtRandom where
    showSelection _ = "RANDOM_"


showMinions :: (IsSelection s) => [Restriction Minion] -> ([Handle Minion] -> Elect s) -> ShowCard String
showMinions restrictions cont = do
    restrictionsStr <- showRestrictions restrictions
    handle <- genHandle $ "MINION[" ++ restrictionsStr ++ "]"
    showElect $ cont [handle]


showPlayers :: (IsSelection s) => [Restriction Player] -> ([Handle Player] -> Elect s) -> ShowCard String
showPlayers restrictions cont = do
    restrictionsStr <- showRestrictions restrictions
    handle <- genHandle $ "PLAYER[" ++ restrictionsStr ++ "]"
    showElect $ cont [handle]


showCharacters :: (IsSelection s) => [Restriction Character] -> ([Handle Character] -> Elect s) -> ShowCard String
showCharacters restrictions cont = do
    restrictionsStr <- showRestrictions restrictions
    handle <- genHandle $ "CHARACTER[" ++ restrictionsStr ++ "]"
    showElect $ cont [handle]


showMinion :: forall s. (IsSelection s) => [Restriction Minion] -> (Handle Minion -> Elect s) -> ShowCard String
showMinion restrictions cont = do
    restrictionsStr <- showRestrictions restrictions
    let sel = showSelection (Proxy :: Proxy s)
    handle <- genNumberedHandle $ sel ++ "MINION[" ++ restrictionsStr ++ "]"
    showElect $ cont handle


showCharacter :: forall s. (IsSelection s) => [Restriction Character] -> (Handle Character -> Elect s) -> ShowCard String
showCharacter restrictions cont = do
    restrictionsStr <- showRestrictions restrictions
    let sel = showSelection (Proxy :: Proxy s)
    handle <- genNumberedHandle $ sel ++ "CHARACTER[" ++ restrictionsStr ++ "]"
    showElect $ cont handle


showRestrictions :: [Restriction a] -> ShowCard String
showRestrictions = liftM (intercalate "," . filter (not . null)) . showRestrictions'


showRestrictions' :: [Restriction a] -> ShowCard [String]
showRestrictions' = \case
    [] -> return []
    r : rs -> showRestriction r >>= \s -> liftM (s :) $ showRestrictions' rs


showRestriction :: Restriction a -> ShowCard String
showRestriction = \case
    OwnedBy handle -> readHandle handle >>= return . \case
        (is you -> True) -> "FRIENDLY"
        _ -> "ENEMY"
    Not handle -> readHandle handle >>= return . \case
        (is this -> True) -> ""
        str -> "NOT " ++ str


showOwnerOf :: (IsSelection s) => Handle a -> (Handle Player -> Elect s) -> ShowCard String
showOwnerOf handle cont = do
    player <- readHandle handle >>= \case
        (is this -> True) -> genHandle you
        str -> genHandle ("(OWNER_OF " ++ str ++ ")")
    showElect $ cont player


showOpponentOf :: (IsSelection s) => Handle Player -> (Handle Player -> Elect s) -> ShowCard String
showOpponentOf minion cont = do
    player <- readHandle minion >>= \case
        (is you -> True) -> genHandle opponent
        str -> genHandle ("(OPPONENT_OF " ++ str ++ ")")
    showElect $ cont player


showSequence :: [Effect] -> ShowCard String
showSequence = liftM itemize . mapM showEffect


showKeywordEffect :: KeywordEffect -> ShowCard String
showKeywordEffect = \case
    Silence handle -> showSilence handle


showSilence :: Handle Minion -> ShowCard String
showSilence minion = do
    minionStr <- readHandle minion
    return $ unwords ["Silence", minionStr]


showDrawCards :: Handle Player -> Int -> ShowCard String
showDrawCards player amount = do
    playerStr <- readHandle player
    let plural = case amount of
            1 -> ""
            _ -> "s"
        drawStr = "draw" ++ plural
        cardStr = "card" ++ plural
    return $ unwords [playerStr, drawStr, show amount, cardStr]


showDealDamage :: Handle Character -> Damage -> ShowCard String
showDealDamage character (Damage amount) = do
    characterStr <- readHandle character
    return $ unwords ["Deal", show amount, "damage to", characterStr]


showEnchant :: Handle Minion -> [Enchantment] -> ShowCard String
showEnchant minion enchantments = do
    minionStr <- readHandle minion
    enchantmentsStr <- showEnchantments enchantments
    return $ unwords ["Give", minionStr, enchantmentsStr]


showGiveAbility :: Handle Minion -> [Ability] -> ShowCard String
showGiveAbility minion abilities = do
    minionStr <- readHandle minion
    abilitiesStr <- showAbilities abilities
    return $ unwords ["Give", minionStr, abilitiesStr]


showGainManaCrystal :: CrystalState -> Handle Player -> ShowCard String
showGainManaCrystal crystalState player = do
    playerStr <- readHandle player
    let crystalStr = case crystalState of
            CrystalFull -> "a Mana Crystal"
            CrystalEmpty -> "an empty Mana Crystal"
            CrystalTemporary -> "a Mana Crystal this turn only"
        gainStr = case is playerStr you of
            True -> "gain"
            False -> "gains"
    return $ unwords [playerStr, gainStr, crystalStr]


showEnchantments :: [Enchantment] -> ShowCard String
showEnchantments = liftM itemize . mapM showEnchantment


showEnchantment :: Enchantment -> ShowCard String
showEnchantment = \case
    StatsDelta (Attack x) (Health y) -> return $ showWithSign x ++ "/" ++ showWithSign y


showWithSign :: Int -> String
showWithSign n = case n > 0 of
    True -> '+' : show n
    False -> show n





