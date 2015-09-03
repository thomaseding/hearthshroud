{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
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
import Data.List.Utils (replace)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy
import Hearth.CardName
import Hearth.Model


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
    in id
        . replace "[" "|"
        . replace "]" "|"
        . replace "[]" ""
        . unlines
        . filter (/= "")
        . lines
        . unlines
        $ [ name ++ " " ++ cost, bt, stats ]


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
    Whenever cont -> showWhenever cont
    Aura aura -> showAuraAbility aura


showAuraAbility :: (Handle Minion -> Aura) -> ShowCard String
showAuraAbility cont = genHandle this >>= showAura . cont


showAura :: Aura -> ShowCard String
showAura = \case
    AuraOwnerOf handle cont -> showOwnerOf showAura handle cont
    AuraOpponentOf handle cont -> showOpponentOf showAura handle cont
    While handle restrictions cont -> showWhile handle restrictions cont
    EachMinion restrictions cont -> showEachMinion restrictions cont
    Has handle enchantments -> showHas handle enchantments


showEachMinion :: [Restriction Minion] -> (Handle Minion -> Aura) -> ShowCard String
showEachMinion restrictions cont = do
    restrictionsStr <- showRestrictions restrictions
    handle <- genHandle $ "MINION[" ++ restrictionsStr ++ "]"
    auraStr <- showAura $ cont handle
    return $ "Each minion: " ++ auraStr


showHas :: Handle Minion -> Enchantment a -> ShowCard String
showHas minion enchantment = do
    minionStr <- readHandle minion
    enchantmentStr <- showEnchantment enchantment
    return $ unwords [minionStr, "has", enchantmentStr]


showWhile :: Handle a -> [Restriction a] -> Aura -> ShowCard String
showWhile handle restrictions aura = case restrictions of
    [] -> showAura aura
    _ -> do
        handleStr <- readHandle handle
        restrictionsStr <- showRestrictions restrictions
        auraStr <- showAura aura
        return $ "While " ++ handleStr ++ "[" ++ restrictionsStr ++ "]: " ++ auraStr


showWhenever :: (Handle Minion -> EventListener) -> ShowCard String
showWhenever cont = do
    thisHandle <- genHandle this
    str <- case cont thisHandle of
        SpellIsCast listener -> showSpellIsCast listener
        DamageIsDealt listener -> showDamageIsDealt listener
    return $ "Whenever " ++ str


showSpellIsCast :: (Handle Spell -> Elect AtRandom) -> ShowCard String
showSpellIsCast listener = do
    spell <- genHandle "CAST_SPELL"
    liftM ("a spell is cast: " ++) $ showElect $ listener spell


showDamageIsDealt :: (Handle Character -> Damage -> DamageSource -> Elect AtRandom) -> ShowCard String
showDamageIsDealt listener = do
    victim <- genHandle "DAMAGED_CHARACTER"
    let damage = 666
        source = Fatigue
    liftM ("a character takes damage: " ++) $ showElect $ listener victim damage source


showKeywordAbility :: KeywordAbility -> ShowCard String
showKeywordAbility = \case
    Battlecry cont -> showBattlecry cont
    Deathrattle cont -> showDeathrattle cont
    Charge -> return "Charge"
    DivineShield -> return "Divine Shield"
    Enrage abilities enchantments -> showEnrage abilities enchantments
    Taunt -> return "Taunt"


showEnrage :: [Ability] -> [Enchantment Continuous] -> ShowCard String
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
    Elect elect -> showElect elect
    DoNothing -> return "DoNothing"
    Unreferenced handle -> showUnreferenced handle
    ForEach handles cont -> showForEach handles cont
    Sequence effects -> showSequence effects
    If cond true false -> showIf cond true false
    DrawCards handle n -> showDrawCards handle n
    DealDamage victim damage source -> showDealDamage victim damage source
    Enchant handle enchantments -> showEnchant handle enchantments
    GrantAbilities handle abilities -> showGrantAbilities handle abilities
    GainManaCrystals handle amount crystalState -> showGainManaCrystals handle amount crystalState
    DestroyMinion handle -> showDestroyMinion handle
    RestoreHealth handle amount -> showRestoreHealth handle amount
    Transform handle minion -> showTransform handle minion
    Silence handle -> showSilence handle
    GainArmor handle amount -> showGainArmor handle amount
    Freeze handle -> showFreeze handle


showIf :: Condition -> Effect -> Effect -> ShowCard String
showIf cond true false = do
    condStr <- showCondition cond
    trueStr <- showEffect true
    falseStr <- showEffect false
    return $ "If " ++ condStr ++ " then " ++ trueStr ++ " else " ++ falseStr


showCondition :: Condition -> ShowCard String
showCondition = \case
    Satisfies handle restrictions -> showSatisfies handle restrictions


showFreeze :: Handle Character -> ShowCard String
showFreeze handle = do
    str <- readHandle handle
    return $ "Freeze " ++ str


showSatisfies :: Handle a -> [Restriction a] -> ShowCard String
showSatisfies handle restrictions = case restrictions of
    [] -> return "True"
    _ -> do
        handleStr <- readHandle handle
        restrictionsStr <- showRestrictions restrictions
        return $ handleStr ++ " satisfies " ++ "[" ++ restrictionsStr ++ "]"


showUnreferenced :: Handle a -> ShowCard String
showUnreferenced handle = do
    str <- readHandle handle
    return $ "Unreferenced " ++ str


showTransform :: Handle Minion -> Minion -> ShowCard String
showTransform oldMinionHandle newMinion = do
    oldMinionStr <- readHandle oldMinionHandle
    let newCardStr = showCard $ HandCardMinion newMinion
    return $ "Transform " ++ oldMinionStr ++ " to " ++ newCardStr


showGainArmor :: Handle Player -> Armor -> ShowCard String
showGainArmor player (Armor amount) = do
    playerStr <- readHandle player
    return $ playerStr ++ " gains " ++ show amount ++ " armor"


showRestoreHealth :: Handle Character -> Health -> ShowCard String
showRestoreHealth character (Health amount) = do
    characterStr <- readHandle character
    return $ "Restore " ++ show amount ++ " health on " ++ characterStr


showDestroyMinion :: Handle Minion -> ShowCard String
showDestroyMinion minion = do
    minionStr <- readHandle minion
    return $ "Destroy " ++ minionStr


showForEach :: HandleList a -> (Handle a -> Effect) -> ShowCard String
showForEach handles cont = case handles of
    HandleList [handle] -> do
        str <- readHandle handle
        effectStr <- showEffect $ cont handle
        return $ "ForEach " ++ str ++ ": " ++ effectStr
    _ -> $logicError 'showForEach "xxx"


showElect :: (IsSelection s) => Elect s -> ShowCard String
showElect = \case
    A x -> showA x
    All x -> showAll x
    Effect x -> showEffect x
    OwnerOf handle cont -> showOwnerOf showElect handle cont
    OpponentOf handle cont -> showOpponentOf showElect handle cont
    Choice choices -> showChoice choices


showChoice :: (IsSelection s) => [Elect s] -> ShowCard String
showChoice choices = do
    st <- get
    strs <- forM choices $ \choice -> do
        str <- showElect choice
        put st
        return str
    let idxs = flip map [(1::Int) ..] $ \n -> "{" ++ show n ++ "}. "
        strs' = zipWith (++) idxs strs
    return $ unlines $ "Choose One:" : strs'


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


class IsSelection (s :: Selection) where
    showSelection :: Proxy s -> String


instance IsSelection Targeted where
    showSelection _ = "TARGET_"


instance IsSelection AtRandom where
    showSelection _ = "RANDOM_"


showMinions :: (IsSelection s) => [Restriction Minion] -> (HandleList Minion -> Elect s) -> ShowCard String
showMinions restrictions cont = do
    restrictionsStr <- showRestrictions restrictions
    handle <- genHandle $ "MINION[" ++ restrictionsStr ++ "]"
    showElect $ cont $ HandleList [handle]


showPlayers :: (IsSelection s) => [Restriction Player] -> (HandleList Player -> Elect s) -> ShowCard String
showPlayers restrictions cont = do
    restrictionsStr <- showRestrictions restrictions
    handle <- genHandle $ "PLAYER[" ++ restrictionsStr ++ "]"
    showElect $ cont $ HandleList [handle]


showCharacters :: (IsSelection s) => [Restriction Character] -> (HandleList Character -> Elect s) -> ShowCard String
showCharacters restrictions cont = do
    restrictionsStr <- showRestrictions restrictions
    handle <- genHandle $ "CHARACTER[" ++ restrictionsStr ++ "]"
    showElect $ cont $ HandleList [handle]


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
    WithMinion r -> showRestriction r
    WithPlayer r -> showRestriction r
    OwnedBy handle -> readHandle handle >>= return . \case
        (is you -> True) -> "FRIENDLY"
        (is opponent -> True) -> "ENEMY"
        str -> "OWNED_BY[" ++ str ++ "]"
    Is handle -> readHandle handle >>= \str -> return ("IS " ++ str)
    Not handle -> readHandle handle >>= \str -> return ("NOT " ++ str)
    IsDamageSource source -> showDamageSource source >>= \str -> return ("IS " ++ str)
    AttackCond ord (Attack value) -> return $ "WITH_ATTACK_" ++ show ord ++ "_" ++ show value
    Damaged -> return "DAMAGED"
    Undamaged -> return "UNDAMAGED"
    IsMinion -> return "IS_MINION"
    AdjacentTo handle -> readHandle handle >>= \str -> return ("ADJACENT_TO " ++ str)


showOwnerOf :: (x -> ShowCard String) -> Handle a -> (Handle Player -> x) -> ShowCard String
showOwnerOf showX handle cont = do
    player <- readHandle handle >>= \case
        (is this -> True) -> genHandle you
        str -> genHandle ("OWNER_OF[" ++ str ++ "]")
    showX $ cont player


showOpponentOf :: (x -> ShowCard String) -> Handle Player -> (Handle Player -> x) -> ShowCard String
showOpponentOf showX minion cont = do
    player <- readHandle minion >>= \case
        (is you -> True) -> genHandle opponent
        str -> genHandle ("OPPONENT_OF[" ++ str ++ "]")
    showX $ cont player


showSequence :: [Effect] -> ShowCard String
showSequence = liftM unlines . mapM showEffect


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


showDamageSource :: DamageSource -> ShowCard String
showDamageSource = \case
    Fatigue -> return "Fatigue"
    DamagingCharacter handle -> readHandle handle
    DamagingSpell handle -> readHandle handle


showDealDamage :: Handle Character -> Damage -> DamageSource -> ShowCard String
showDealDamage character (Damage amount) source = do
    characterStr <- readHandle character
    sourceStr <- showDamageSource source
    return $ unwords ["Deal", show amount, "damage to", characterStr, "by", sourceStr]


showEnchant :: Handle Minion -> AnyEnchantment -> ShowCard String
showEnchant minion enchantment = do
    minionStr <- readHandle minion
    enchantmentStr <- case enchantment of
        Continuous e -> showEnchantment e
        Limited e -> showEnchantment e
    return $ unwords ["Give", minionStr, enchantmentStr]


showGrantAbilities :: Handle Minion -> [Ability] -> ShowCard String
showGrantAbilities minion abilities = do
    minionStr <- readHandle minion
    abilitiesStr <- showAbilities abilities
    return $ unwords ["Grant", minionStr, abilitiesStr]


showGainManaCrystals :: Handle Player -> Int -> CrystalState -> ShowCard String
showGainManaCrystals player amount crystalState = do
    playerStr <- readHandle player
    let s = case amount of
            1 -> ""
            _ -> "s"
        crystalStr = show amount ++ case crystalState of
            CrystalFull -> " Mana Crystal" ++ s
            CrystalEmpty -> " Empty Mana Crystal" ++ s
            CrystalTemporary -> " Mana Crystal" ++ s ++ "this turn only"
        gainStr = case is playerStr you of
            True -> "gain"
            False -> "gains"
    return $ unwords [playerStr, gainStr, crystalStr]


showEnchantment :: Enchantment a -> ShowCard String
showEnchantment = \case
    StatsDelta (Attack x) (Health y) -> return $ showWithSign x ++ "/" ++ showWithSign y
    StatsScale (Attack x) (Health y) -> return $ "Scale stats by " ++ show x ++ "/" ++ show y
    ChangeStat e -> case e of
        Left (Attack x) -> return $ "Attack changed to " ++ show x
        Right (Health y) -> return $ "Health changed to " ++ show y
    SwapStats -> return "Swapped attack and health"
    Frozen -> return "Frozen"
    Until timePoint enchantment -> showUntil timePoint enchantment


showUntil :: TimePoint -> Enchantment Continuous -> ShowCard String
showUntil timePoint enchantment = do
    timePointStr <- showTimePoint timePoint
    enchantmentStr <- showEnchantment enchantment
    return $ enchantmentStr ++ " until " ++ timePointStr


showTimePoint :: TimePoint -> ShowCard String
showTimePoint = return . show


showWithSign :: Int -> String
showWithSign n = case n > 0 of
    True -> '+' : show n
    False -> show n





