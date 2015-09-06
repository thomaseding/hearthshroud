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
import Data.Proxy
import Data.Typeable (cast)
import Hearth.CardName
import Hearth.Cards
import Hearth.Model


--------------------------------------------------------------------------------


type AlgebraicSymbol = Int


data ShowState = ShowState {
    handleSeed :: Int,
    algebraicSymbolSeed :: Int
} deriving ()


newtype ShowCard a = ShowCard {
    unShowCard :: State ShowState a
} deriving (Functor, Applicative, Monad, MonadState ShowState)


runShowCard :: ShowCard a -> a
runShowCard m = evalState (unShowCard m) $ ShowState {
    handleSeed = 0,
    algebraicSymbolSeed = 0 }


--------------------------------------------------------------------------------


rawGenHandle :: String -> ShowCard RawHandle
rawGenHandle str = do
    n <- gets handleSeed
    let handle = RawHandle str n
    modify $ \st -> st { handleSeed = n + 1 }
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


proxiedGenHandle :: Handle a -> String -> ShowCard (Handle a)
proxiedGenHandle = \case
    SpellHandle {} -> genHandle
    MinionHandle {} -> genHandle
    PlayerHandle {} -> genHandle
    MinionCharacter {} -> genHandle
    PlayerCharacter {} -> genHandle


--------------------------------------------------------------------------------


rawReadHandle :: RawHandle -> ShowCard String
rawReadHandle h = case getUserData h of
    Just str -> return str
    Nothing -> $logicError 'rawReadHandle "xxx"


readHandle :: Handle a -> ShowCard String
readHandle = applyRawHandle rawReadHandle


--------------------------------------------------------------------------------


genAlgebraicSymbol :: ShowCard AlgebraicSymbol
genAlgebraicSymbol = do
    n <- gets algebraicSymbolSeed
    modify $ \st -> st { algebraicSymbolSeed = n + 1 }
    return n


readAlgebraicSymbol :: AlgebraicSymbol -> ShowCard String
readAlgebraicSymbol = return . \case
    n -> case n < 0 of
        False -> show n
        True -> let
            idx = negate $ n + 1
            in symbols !! idx
    where
        symbols = words "X Y Z W" ++ map (\n -> "N" ++ show n) [1 :: Int ..]


--------------------------------------------------------------------------------


genAlgebraicDamage :: ShowCard Damage
genAlgebraicDamage = liftM (Damage . subtract 1 . negate) genAlgebraicSymbol


readDamage :: Damage -> ShowCard String
readDamage (Damage n) = case n < 0 of
    False -> return $ show n
    True -> readAlgebraicSymbol $ negate $ n + 1


--------------------------------------------------------------------------------


genDamageSource :: String -> ShowCard DamageSource
genDamageSource = liftM DamagingSpell . genHandle


readDamageSource :: DamageSource -> ShowCard String
readDamageSource = \case
    Fatigue -> return "Fatigue"
    DamagingSpell handle -> readHandle handle
    DamagingCharacter handle -> readHandle handle


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
showName card = case cardName card of
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
    Whenever cont -> showWhenever cont
    Aura aura -> showAuraAbility aura
    Battlecry cont -> showBattlecry cont
    Deathrattle cont -> showDeathrattle cont
    Charge -> return "Charge"
    DivineShield -> return "Divine Shield"
    Enrage abilities enchantments -> showEnrage abilities enchantments
    Taunt -> return "Taunt"


showAuraAbility :: (Handle Minion -> Aura) -> ShowCard String
showAuraAbility cont = genHandle this >>= showAura . cont


showAura :: Aura -> ShowCard String
showAura = \case
    AuraOwnerOf handle cont -> showOwnerOf showAura handle cont
    AuraOpponentOf handle cont -> showOpponentOf showAura handle cont
    While handle requirements cont -> showWhile handle requirements cont
    EachMinion requirements cont -> showEachMinion requirements cont
    Has handle enchantments -> showHas handle enchantments


showEachMinion :: [Requirement Minion] -> (Handle Minion -> Aura) -> ShowCard String
showEachMinion requirements cont = do
    requirementsStr <- showRequirements requirements
    handle <- genHandle $ "MINION[" ++ requirementsStr ++ "]"
    auraStr <- showAura $ cont handle
    return $ "Each minion: " ++ auraStr


showHas :: Handle Minion -> Enchantment t a -> ShowCard String
showHas minion enchantment = do
    minionStr <- readHandle minion
    enchantmentStr <- showEnchantment enchantment
    return $ unwords [minionStr, "has", enchantmentStr]


showWhile :: Handle a -> [Requirement a] -> Aura -> ShowCard String
showWhile handle requirements aura = case requirements of
    [] -> showAura aura
    _ -> do
        handleStr <- readHandle handle
        requirementsStr <- showRequirements requirements
        auraStr <- showAura aura
        return $ "While " ++ handleStr ++ "[" ++ requirementsStr ++ "]: " ++ auraStr


showWhenever :: (Handle Minion -> EventListener) -> ShowCard String
showWhenever cont = do
    str <- genHandle this >>= showEventListener . cont
    return $ "Whenever " ++ str


showEventListener :: EventListener -> ShowCard String
showEventListener = \case
    SpellIsCast listener -> showSpellIsCast listener
    DamageIsDealt listener -> showDamageIsDealt listener


showSpellIsCast :: (Handle Spell -> Elect AtRandom) -> ShowCard String
showSpellIsCast listener = do
    spell <- genHandle "CAST_SPELL"
    liftM ("a spell is cast: " ++) $ showElect $ listener spell


showDamageIsDealt :: (Handle Character -> Damage -> DamageSource -> Elect AtRandom) -> ShowCard String
showDamageIsDealt listener = do
    victim <- genHandle "DAMAGED_CHARACTER"
    source <- genDamageSource "DAMAGE_SOURCE"
    damage <- genAlgebraicDamage
    damageStr <- readDamage damage
    let prelude = "a character takes " ++ damageStr ++ " damage: "
    liftM (prelude ++) $ showElect $ listener victim damage source


showEnrage :: [Ability] -> [Enchantment Continuous Minion] -> ShowCard String
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
    Observing effect listener -> showObserving effect listener
    PutInHand player card -> showPutInHand player card


showPutInHand :: Handle Player -> Card -> ShowCard String
showPutInHand player card = do
    playerStr <- readHandle player
    let cardStr = showCardName $ cardName card
    return $ "Put " ++ cardStr ++ " in " ++ playerStr ++ "'s hand"


showObserving :: Effect -> EventListener -> ShowCard String
showObserving effect listener = do
    effectStr <- showEffect effect
    listenerStr <- showEventListener listener
    return $ "Observing (" ++ effectStr ++ ") with (" ++ listenerStr ++ ")"


showIf :: Condition -> Effect -> Effect -> ShowCard String
showIf cond true false = do
    condStr <- showCondition cond
    trueStr <- showEffect true
    falseStr <- showEffect false
    return $ "If " ++ condStr ++ " then " ++ trueStr ++ " else " ++ falseStr


showCondition :: Condition -> ShowCard String
showCondition = \case
    Or x y -> showOr x y
    And x y -> showAnd x y
    Satisfies handle requirements -> showSatisfies handle requirements


showBinaryCondition :: String -> Condition -> Condition -> ShowCard String
showBinaryCondition opStr x y = do
    xStr <- showCondition x
    yStr <- showCondition y
    return $ "(" ++ xStr ++ " " ++ opStr ++ " " ++ yStr ++ ")"


showOr :: Condition -> Condition -> ShowCard String
showOr = showBinaryCondition "or"


showAnd :: Condition -> Condition -> ShowCard String
showAnd = showBinaryCondition "and"


showFreeze :: Handle Character -> ShowCard String
showFreeze handle = do
    str <- readHandle handle
    return $ "Freeze " ++ str


showSatisfies :: Handle a -> [Requirement a] -> ShowCard String
showSatisfies handle requirements = case requirements of
    [] -> return "True"
    _ -> do
        handleStr <- readHandle handle
        requirementsStr <- showRequirements requirements
        return $ handleStr ++ " satisfies " ++ "[" ++ requirementsStr ++ "]"


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
showForEach (HandleList userData handles) cont = case cast userData of
    Just () -> case handles of
        [] -> showEffect DoNothing
        proxy : _ -> do
            handlesStr <- liftM itemize $ mapM readHandle handles
            representative <- proxiedGenHandle proxy =<< readAlgebraicSymbol =<< genAlgebraicSymbol
            representativeStr <- readHandle representative
            effectStr <- showEffect $ cont representative
            return $ "ForEach [" ++ handlesStr ++ "] as " ++ representativeStr ++ ": " ++ effectStr
    Nothing -> case cast userData of
        Just str -> case handles of
            [representative] -> do
                effectStr <- showEffect $ cont representative
                return $ "ForEach " ++ str ++ ": " ++ effectStr
            _ -> $logicError 'showForEach "xxx"
        Nothing -> $logicError 'showForEach "xxx"


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
    Minion requirements cont -> showMinion requirements cont
    Player requirements cont -> showPlayer requirements cont
    Character requirements cont -> showCharacter requirements cont


showPlayer :: forall s. (IsSelection s) => [Requirement Player] -> (Handle Player -> Elect s) -> ShowCard String
showPlayer requirements cont = do
    requirementsStr <- showRequirements requirements
    let sel = showSelection (Proxy :: Proxy s)
    handle <- genNumberedHandle $ sel ++ "PLAYER[" ++ requirementsStr ++ "]"
    showElect $ cont handle


showAll :: (IsSelection s) => All s -> ShowCard String
showAll = \case
    Minions requirements cont -> showMinions requirements cont
    Players requirements cont -> showPlayers requirements cont
    Characters requirements cont -> showCharacters requirements cont


class IsSelection (s :: Selection) where
    showSelection :: Proxy s -> String


instance IsSelection Targeted where
    showSelection _ = "TARGET_"


instance IsSelection AtRandom where
    showSelection _ = "RANDOM_"


showMinions :: (IsSelection s) => [Requirement Minion] -> (HandleList Minion -> Elect s) -> ShowCard String
showMinions requirements cont = do
    requirementsStr <- showRequirements requirements
    let handleStr = "MINION[" ++ requirementsStr ++ "]"
    handle <- genHandle handleStr
    showElect $ cont $ HandleList handleStr [handle]


showPlayers :: (IsSelection s) => [Requirement Player] -> (HandleList Player -> Elect s) -> ShowCard String
showPlayers requirements cont = do
    requirementsStr <- showRequirements requirements
    let handleStr = "PLAYER[" ++ requirementsStr ++ "]"
    handle <- genHandle handleStr
    showElect $ cont $ HandleList handleStr [handle]


showCharacters :: (IsSelection s) => [Requirement Character] -> (HandleList Character -> Elect s) -> ShowCard String
showCharacters requirements cont = do
    requirementsStr <- showRequirements requirements
    let handleStr = "CHARACTER[" ++ requirementsStr ++ "]"
    handle <- genHandle handleStr
    showElect $ cont $ HandleList handleStr [handle]


showMinion :: forall s. (IsSelection s) => [Requirement Minion] -> (Handle Minion -> Elect s) -> ShowCard String
showMinion requirements cont = do
    requirementsStr <- showRequirements requirements
    let sel = showSelection (Proxy :: Proxy s)
    handle <- genNumberedHandle $ sel ++ "MINION[" ++ requirementsStr ++ "]"
    showElect $ cont handle


showCharacter :: forall s. (IsSelection s) => [Requirement Character] -> (Handle Character -> Elect s) -> ShowCard String
showCharacter requirements cont = do
    requirementsStr <- showRequirements requirements
    let sel = showSelection (Proxy :: Proxy s)
    handle <- genNumberedHandle $ sel ++ "CHARACTER[" ++ requirementsStr ++ "]"
    showElect $ cont handle


showRequirements :: [Requirement a] -> ShowCard String
showRequirements = liftM (intercalate "," . filter (not . null)) . showRequirements'


showRequirements' :: [Requirement a] -> ShowCard [String]
showRequirements' = \case
    [] -> return []
    r : rs -> showRequirement r >>= \s -> liftM (s :) $ showRequirements' rs


showRequirement :: Requirement a -> ShowCard String
showRequirement = \case
    RequireMinion r -> showRequirement r
    RequirePlayer r -> showRequirement r
    OwnedBy handle -> readHandle handle >>= return . \case
        (is you -> True) -> "FRIENDLY"
        (is opponent -> True) -> "ENEMY"
        str -> "OWNED_BY[" ++ str ++ "]"
    Is handle -> readHandle handle >>= \str -> return ("IS " ++ str)
    Not handle -> readHandle handle >>= \str -> return ("NOT " ++ str)
    IsDamageSource source -> readDamageSource source >>= \str -> return ("IS " ++ str)
    WithAttack ord (Attack value) -> return $ "WITH_ATTACK_" ++ show ord ++ "_" ++ show value
    WithHealth ord (Health value) -> return $ "WITH_HEALTH_" ++ show ord ++ "_" ++ show value
    Damaged -> return "DAMAGED"
    Undamaged -> return "UNDAMAGED"
    IsMinion -> return "IS_MINION"
    AdjacentTo handle -> readHandle handle >>= \str -> return ("ADJACENT_TO " ++ str)
    HasMaxManaCrystals -> return "HAS_MAX_MANA_CRYSTALS"


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


showDealDamage :: Handle Character -> Damage -> DamageSource -> ShowCard String
showDealDamage character damage source = do
    characterStr <- readHandle character
    damageStr <- readDamage damage
    sourceStr <- readDamageSource source
    return $ unwords ["Deal", damageStr, "damage to", characterStr, "by", sourceStr]


showEnchant :: Handle a -> AnyEnchantment a -> ShowCard String
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


showEnchantment :: Enchantment t a -> ShowCard String
showEnchantment = \case
    MinionEnchantment e -> showEnchantment e
    PlayerEnchantment e -> showEnchantment e
    Until timePoint enchantment -> showUntil timePoint enchantment
    StatsDelta (Attack x) (Health y) -> return $ showWithSign x ++ "/" ++ showWithSign y
    StatsScale (Attack x) (Health y) -> return $ "Scale stats by " ++ show x ++ "/" ++ show y
    ChangeStat e -> case e of
        Left (Attack x) -> return $ "Attack changed to " ++ show x
        Right (Health y) -> return $ "Health changed to " ++ show y
    SwapStats -> return "Swapped attack and health"
    Frozen -> return "Frozen"


showUntil :: TimePoint -> Enchantment Continuous a -> ShowCard String
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





