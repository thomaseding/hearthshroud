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


module Hearth.Reflection.ShowCard (
    showCard,
    GenHandle,
) where


--------------------------------------------------------------------------------


import Control.Error.TH
import Control.Monad.State
import Data.List (intercalate, isPrefixOf)
import Data.Proxy
import Data.Typeable (cast)
import Hearth.Authored.Cards (cardName)
import Hearth.Model.Authoring
import Hearth.Model.Runtime


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


class GenHandle (a :: ObjectType) where
    genHandle :: String -> ShowCard (Handle a)


instance GenHandle 'Spell' where
    genHandle = liftM SpellHandle . rawGenHandle


instance GenHandle 'Weapon' where
    genHandle = liftM WeaponHandle . rawGenHandle


instance GenHandle 'Minion' where
    genHandle = liftM MinionHandle . rawGenHandle


instance GenHandle 'Player' where
    genHandle = liftM PlayerHandle . rawGenHandle


instance GenHandle 'Character' where
    genHandle = liftM PlayerCharacter . genHandle


genNumberedHandle :: (GenHandle a) => String -> ShowCard (Handle a)
genNumberedHandle str = do
    n <- gets handleSeed
    genHandle $ str ++ "_" ++ show n


--------------------------------------------------------------------------------


rawReadHandle :: RawHandle -> ShowCard String
rawReadHandle h = case getUserData h of
    Just str -> return str
    Nothing -> $logicError 'rawReadHandle "xxx"


readHandle :: Handle a -> ShowCard String
readHandle = rawReadHandle . getRawHandle


--------------------------------------------------------------------------------


genAlgebraicSymbol :: ShowCard AlgebraicSymbol
genAlgebraicSymbol = do
    n <- gets algebraicSymbolSeed
    modify $ \st -> st { algebraicSymbolSeed = n + 1 }
    return n


readAlgebraicSymbol :: AlgebraicSymbol -> ShowCard String
readAlgebraicSymbol n = return $ symbols !! n
    where
        symbols = words "X Y Z W" ++ map (\i -> "N" ++ show i) [1 :: Int ..]


--------------------------------------------------------------------------------


genAlgebraicInt :: ShowCard Int
genAlgebraicInt = liftM (subtract 1 . negate) genAlgebraicSymbol


readAlgebraicInt :: Int -> ShowCard String
readAlgebraicInt n = case n < 0 of
    True -> readAlgebraicSymbol $ negate $ n + 1
    False -> return $ show n


genAlgebraicDamage :: ShowCard Damage
genAlgebraicDamage = liftM toDamage genAlgebraicInt


readDamage :: Damage -> ShowCard String
readDamage (Damage n) = readAlgebraicInt $ viewInt n


genAlgebraicHealth :: ShowCard Health
genAlgebraicHealth = liftM toHealth genAlgebraicInt


readHealth :: Health -> ShowCard String
readHealth (Health n) = readAlgebraicInt $ viewInt n


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


replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace old new (items @ (_ : rest)) = case old `isPrefixOf` items of
    False -> replace old new rest
    True -> new ++ replace old new (drop (length old) items)


showCard :: Card -> String
showCard card = let
    name = showCardName $ cardName card
    cost = showCost card
    bt = boxText card
    mStats = case card of
        CardMinion minion -> let
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


showCost :: Card -> String
showCost card = let
    cost = case card of
        CardMinion minion -> _minionCost minion
        CardSpell spell -> _spellCost spell
        CardWeapon weapon -> _weaponCost weapon
    in case cost of
        ManaCost (Mana mana) -> "(" ++ show mana ++ ")"


boxText :: Card -> String
boxText = runShowCard . liftM (unlines . filter (not . null) . lines) . \case
    CardMinion minion -> showAbilities $ _minionAbilities minion
    CardSpell spell -> genHandle this >>= showElect . _spellEffect spell
    CardWeapon weapon -> showAbilities $ _weaponAbilities weapon


--------------------------------------------------------------------------------


showAbilities :: (GenHandle a) => [Ability a] -> ShowCard String
showAbilities = liftM unlines . mapM showAbility


showAbility :: (GenHandle a) => Ability a -> ShowCard String
showAbility = \case
    ObserverMinion cont -> showObserver cont
    AuraMinion aura -> showAuraAbility aura
    Battlecry cont -> showBattlecry cont
    Deathrattle cont -> showDeathrattle cont
    ChooseOne cont -> showChooseOne cont
    Charge -> return "Charge"
    DivineShield -> return "Divine Shield"
    Enrage abilities enchantments -> showEnrage abilities enchantments
    Taunt -> return "Taunt"
    SpellDamage n -> return $ "Spell Damage +" ++ show n
    Windfury -> return "Windfury"
    Can'tAttack -> return "Can't Attack"


showAuraAbility :: (GenHandle a) => (Handle a -> Aura) -> ShowCard String
showAuraAbility cont = genHandle this >>= showAura . cont


showAura :: Aura -> ShowCard String
showAura = \case
    AuraOwnerOf handle cont -> showOwnerOf showAura handle cont
    AuraOpponentOf handle cont -> showOpponentOf showAura handle cont
    AuraSequence auras -> showAuraSequence auras
    While handle requirements cont -> showWhile handle requirements cont
    EachMinion requirements cont -> showEachMinion requirements cont
    Has handle enchantment -> showHas handle enchantment
    HasAbility handle ability -> showHasAbility handle ability


showAuraSequence :: [Aura] -> ShowCard String
showAuraSequence = liftM unlines . mapM showAura


showEachMinion :: [Requirement 'Minion'] -> (Handle 'Minion' -> Aura) -> ShowCard String
showEachMinion requirements cont = do
    requirementsStr <- showRequirements requirements
    handle <- genHandle $ "MINION[" ++ requirementsStr ++ "]"
    auraStr <- showAura $ cont handle
    return $ "Each minion: " ++ auraStr


showHas :: (GenHandle a) => Handle a -> Enchantment t a -> ShowCard String
showHas handle enchantment = do
    handleStr <- readHandle handle
    enchantmentStr <- showEnchantment enchantment
    return $ unwords [handleStr, "has", enchantmentStr]


showHasAbility :: (GenHandle a) => Handle a -> Ability a -> ShowCard String
showHasAbility minion ability = do
    minionStr <- readHandle minion
    abilityStr <- showAbility ability
    return $ unwords [minionStr, "has", abilityStr]


showWhile :: Handle a -> [Requirement a] -> Aura -> ShowCard String
showWhile handle requirements aura = case handle of
    SpellHandle {} -> showWhile' handle requirements aura
    WeaponHandle {} -> showWhile' handle requirements aura
    MinionHandle {} -> showWhile' handle requirements aura
    PlayerHandle {} -> showWhile' handle requirements aura
    MinionCharacter {} -> showWhile' handle requirements aura
    PlayerCharacter {} -> showWhile' handle requirements aura


showWhile' :: (GenHandle a) => Handle a -> [Requirement a] -> Aura -> ShowCard String
showWhile' handle requirements aura = case requirements of
    [] -> showAura aura
    _ -> do
        handleStr <- readHandle handle
        requirementsStr <- showRequirements requirements
        auraStr <- showAura aura
        return $ "While " ++ handleStr ++ "[" ++ requirementsStr ++ "]: " ++ auraStr


showObserver :: (GenHandle a) => (Handle a -> EventListener) -> ShowCard String
showObserver cont = do
    str <- genHandle this >>= showEventListener . cont
    return $ "Observer " ++ str


showEventListener :: EventListener -> ShowCard String
showEventListener = \case
    SpellIsCast listener -> showSpellIsCast listener
    DamageIsDealt listener -> showDamageIsDealt listener
    HealthIsRestored listener -> showHealthIsRestored listener
    AtEndOfTurn listener -> showAtEndOfTurn listener


showAtEndOfTurn :: (Handle 'Player' -> Elect 'AtRandom') -> ShowCard String
showAtEndOfTurn listener = do
    player <- genHandle "ACTIVE_PLAYER"
    liftM ("end of turn: " ++) $ showElect $ listener player


showSpellIsCast :: (Handle 'Spell' -> Elect 'AtRandom') -> ShowCard String
showSpellIsCast listener = do
    spell <- genHandle "CAST_SPELL"
    liftM ("a spell is cast: " ++) $ showElect $ listener spell


showDamageIsDealt :: (Handle 'Character' -> Damage -> DamageSource -> Elect 'AtRandom') -> ShowCard String
showDamageIsDealt listener = do
    victim <- genHandle "DAMAGED_CHARACTER"
    source <- genDamageSource "DAMAGE_SOURCE"
    damage <- genAlgebraicDamage
    damageStr <- readDamage damage
    let prelude = "a character takes " ++ damageStr ++ " damage: "
    liftM (prelude ++) $ showElect $ listener victim damage source


showHealthIsRestored :: (Handle 'Character' -> Health -> Elect 'AtRandom') -> ShowCard String
showHealthIsRestored listener = do
    recipient <- genHandle "RESTORED_CHARACTER"
    health <- genAlgebraicHealth
    healthStr <- readHealth health
    let prelude = "a character restores " ++ healthStr ++ " health: "
    liftM (prelude ++) $ showElect $ listener recipient health


showEnrage :: [Ability 'Minion'] -> [Enchantment 'Continuous' 'Minion'] -> ShowCard String
showEnrage abilities enchantments = do
    asStr <- mapM showAbility abilities
    esStr <- mapM showEnchantment enchantments
    return $ "Enrage: " ++ itemize (asStr ++ esStr)


showDeathrattle :: (GenHandle a) => (Handle a -> Elect 'AtRandom') -> ShowCard String
showDeathrattle cont = do
    effectStr <- genHandle this >>= showElect . cont
    return $ "Deathrattle: " ++ effectStr


showBattlecry :: (GenHandle a) => (Handle a -> Elect 'Targeted') -> ShowCard String
showBattlecry cont = do
    effectStr <- genHandle this >>= showElect . cont
    return $ "Battlecry: " ++ effectStr


showEffect :: Effect -> ShowCard String
showEffect = \case
    Get elect -> showElect elect
    DoNothing -> return "DoNothing"
    Unreferenced handle -> showUnreferenced handle
    ForEachMinion handles cont -> showForEach handles cont
    ForEachPlayer handles cont -> showForEach handles cont
    ForEachCharacter handles cont -> showForEach handles cont
    Sequence effects -> showSequence effects
    If cond true false -> showIf cond true false
    DrawCards handle n -> showDrawCards handle n
    DealDamage victim damage source -> showDealDamage victim damage source
    Enchant handle enchantment -> showEnchant handle enchantment
    GainManaCrystals handle amount crystalState -> showGainManaCrystals handle amount crystalState
    DestroyMinion handle -> showDestroy handle
    DestroyWeapon handle -> showDestroy handle
    EquipWeapon handle weapon -> showEquipWeapon handle weapon
    RestoreHealth handle amount -> showRestoreHealth handle amount
    RestoreToFullHealth handle -> showRestoreToFullHealth handle
    Transform handle minion -> showTransform handle minion
    Silence handle -> showSilence handle
    GainArmor handle amount -> showGainArmor handle amount
    Freeze handle -> showFreeze handle
    Observing effect listener -> showObserving effect listener
    PutInHand player card -> showPutInHand player card
    Summon minion loc -> showSummon minion loc
    RandomMissiles reqs n spell -> showRandomMissiles reqs n spell
    DiscardAtRandom player -> showDiscardAtRandom player
    TakeControl player minion -> showTakeControl player minion


showEquipWeapon :: Handle 'Player' -> WeaponCard -> ShowCard String
showEquipWeapon player weapon = do
    playerStr <- readHandle player
    let weaponName = showCardName $ cardName weapon
    return $ playerStr ++ " equip(s) " ++ weaponName


showTakeControl :: Handle 'Player' -> Handle 'Minion' -> ShowCard String
showTakeControl player minion = do
    playerStr <- readHandle player
    minionStr <- readHandle minion
    return $ playerStr ++ " take(s) control of " ++ minionStr


showDiscardAtRandom :: Handle 'Player' -> ShowCard String
showDiscardAtRandom player = do
    playerStr <- readHandle player
    return $ playerStr ++ " discard(s) a card at random"


showRandomMissiles :: [Requirement 'Character'] -> Int -> Handle 'Spell' -> ShowCard String
showRandomMissiles reqs n spell = do
    reqsStr <- showRequirements reqs
    spellStr <- readHandle spell
    return $ show n ++ " RandomMissiles from " ++ spellStr ++ " targeting " ++ reqsStr ++ " characters"


showSummon :: MinionCard -> BoardLocation -> ShowCard String
showSummon minion loc = do
    let minionName = showCardName $ cardName minion
    locStr <- showBoardLocation loc
    return $ "summon " ++ minionName ++ " " ++ locStr


showBoardLocation :: BoardLocation -> ShowCard String
showBoardLocation = \case
    RightOf minion -> do
        minionStr <- readHandle minion
        return $ "to the right of " ++ minionStr
    Rightmost player -> do
        str <- readHandle player
        return $ "to the rightmost of " ++ str


showPutInHand :: Handle 'Player' -> Card -> ShowCard String
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


showFreeze :: Handle 'Character' -> ShowCard String
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


showTransform :: Handle 'Minion' -> MinionCard -> ShowCard String
showTransform oldMinionHandle newMinion = do
    oldMinionStr <- readHandle oldMinionHandle
    let newCardStr = showCard $ CardMinion newMinion
    return $ "Transform " ++ oldMinionStr ++ " to " ++ newCardStr


showGainArmor :: Handle 'Player' -> Armor -> ShowCard String
showGainArmor player (Armor amount) = do
    playerStr <- readHandle player
    return $ playerStr ++ " gains " ++ show amount ++ " armor"


showRestoreHealth :: Handle 'Character' -> Health -> ShowCard String
showRestoreHealth character health = do
    healthStr <- readHealth health
    characterStr <- readHandle character
    return $ characterStr ++ " restores " ++ healthStr ++ " health"


showRestoreToFullHealth :: Handle 'Character' -> ShowCard String
showRestoreToFullHealth character = do
    characterStr <- readHandle character
    return $ "Restore " ++ characterStr ++ " to full health"


showDestroy :: Handle a -> ShowCard String
showDestroy handle = do
    handleStr <- readHandle handle
    return $ "Destroy " ++ handleStr


showForEach :: (GenHandle a) => HandleList a -> (Handle a -> Effect) -> ShowCard String
showForEach (HandleList userData handles) cont = case cast userData of
    Just () -> case handles of
        [] -> showEffect DoNothing
        _ -> do
            handlesStr <- liftM itemize $ mapM readHandle handles
            representative <- genHandle =<< readAlgebraicSymbol =<< genAlgebraicSymbol
            representativeStr <- readHandle representative
            effectStr <- showEffect $ cont representative
            return $ "ForEach [" ++ handlesStr ++ "] as " ++ representativeStr ++ ": " ++ effectStr
    Nothing -> case cast userData of
        Just str -> case handles of
            [_] -> do
                representative <- genHandle =<< readAlgebraicSymbol =<< genAlgebraicSymbol
                representativeStr <- readHandle representative
                effectStr <- showEffect $ cont representative
                return $ "ForEach " ++ str ++ " as " ++ representativeStr ++ ": " ++ effectStr
            _ -> $logicError 'showForEach "xxx"
        Nothing -> $logicError 'showForEach "xxx"


showElect :: (IsSelection s) => Elect s -> ShowCard String
showElect = \case
    A x -> showA x
    All x -> showAll x
    Effect x -> showEffect x
    OwnerOf handle cont -> showOwnerOf showElect handle cont
    OpponentOf handle cont -> showOpponentOf showElect handle cont
    ChooseOne' choices -> showChooseOne' choices


showChooseOne :: (IsSelection s, GenHandle a) => (Handle a -> [Elect s]) -> ShowCard String
showChooseOne cont = do
    genHandle this >>= showChooseOne' . cont


showChooseOne' :: (IsSelection s) => [Elect s] -> ShowCard String
showChooseOne' choices = do
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
    Weapon requirements cont -> showWeapon requirements cont
    Minion requirements cont -> showMinion requirements cont
    Player requirements cont -> showPlayer requirements cont
    Character requirements cont -> showCharacter requirements cont


showWeapon :: forall s. (IsSelection s) => [Requirement 'Weapon'] -> (Handle 'Weapon' -> Elect s) -> ShowCard String
showWeapon requirements cont = do
    requirementsStr <- showRequirements requirements
    let sel = showSelection (Proxy :: Proxy s)
    handle <- genNumberedHandle $ sel ++ "WEAPON[" ++ requirementsStr ++ "]"
    showElect $ cont handle


showPlayer :: forall s. (IsSelection s) => [Requirement 'Player'] -> (Handle 'Player' -> Elect s) -> ShowCard String
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


instance IsSelection 'Targeted' where
    showSelection _ = "TARGET_"


instance IsSelection 'AtRandom' where
    showSelection _ = "RANDOM_"


showMinions :: (IsSelection s) => [Requirement 'Minion'] -> (HandleList 'Minion' -> Elect s) -> ShowCard String
showMinions requirements cont = do
    requirementsStr <- showRequirements requirements
    let handleStr = "MINION[" ++ requirementsStr ++ "]"
    handle <- genHandle handleStr
    showElect $ cont $ HandleList handleStr [handle]


showPlayers :: (IsSelection s) => [Requirement 'Player'] -> (HandleList 'Player' -> Elect s) -> ShowCard String
showPlayers requirements cont = do
    requirementsStr <- showRequirements requirements
    let handleStr = "PLAYER[" ++ requirementsStr ++ "]"
    handle <- genHandle handleStr
    showElect $ cont $ HandleList handleStr [handle]


showCharacters :: (IsSelection s) => [Requirement 'Character'] -> (HandleList 'Character' -> Elect s) -> ShowCard String
showCharacters requirements cont = do
    requirementsStr <- showRequirements requirements
    let handleStr = "CHARACTER[" ++ requirementsStr ++ "]"
    handle <- genHandle handleStr
    showElect $ cont $ HandleList handleStr [handle]


showMinion :: forall s. (IsSelection s) => [Requirement 'Minion'] -> (Handle 'Minion' -> Elect s) -> ShowCard String
showMinion requirements cont = do
    requirementsStr <- showRequirements requirements
    let sel = showSelection (Proxy :: Proxy s)
    handle <- genNumberedHandle $ sel ++ "MINION[" ++ requirementsStr ++ "]"
    showElect $ cont handle


showCharacter :: forall s. (IsSelection s) => [Requirement 'Character'] -> (Handle 'Character' -> Elect s) -> ShowCard String
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
    OfTribe tribe -> return $ "is " ++ show tribe
    HasCharge -> return "HAS_CHARGE"
    HasMinion reqs -> do
        reqsStr <- showRequirements reqs
        return $ "HAS_MINION" ++ reqsStr


showOwnerOf :: (x -> ShowCard String) -> Handle a -> (Handle 'Player' -> x) -> ShowCard String
showOwnerOf showX handle cont = do
    player <- readHandle handle >>= \case
        (is this -> True) -> genHandle you
        str -> genHandle ("OWNER_OF[" ++ str ++ "]")
    showX $ cont player


showOpponentOf :: (x -> ShowCard String) -> Handle 'Player' -> (Handle 'Player' -> x) -> ShowCard String
showOpponentOf showX minion cont = do
    player <- readHandle minion >>= \case
        (is you -> True) -> genHandle opponent
        str -> genHandle ("OPPONENT_OF[" ++ str ++ "]")
    showX $ cont player


showSequence :: [Effect] -> ShowCard String
showSequence = liftM unlines . mapM showEffect


showSilence :: Handle 'Minion' -> ShowCard String
showSilence minion = do
    minionStr <- readHandle minion
    return $ unwords ["Silence", minionStr]


showDrawCards :: Handle 'Player' -> Int -> ShowCard String
showDrawCards player amount = do
    playerStr <- readHandle player
    let plural = case amount of
            1 -> ""
            _ -> "s"
        drawStr = "draw" ++ plural
        cardStr = "card" ++ plural
    return $ unwords [playerStr, drawStr, show amount, cardStr]


showDealDamage :: Handle 'Character' -> Damage -> DamageSource -> ShowCard String
showDealDamage character damage source = do
    characterStr <- readHandle character
    damageStr <- readDamage damage
    sourceStr <- readDamageSource source
    return $ unwords ["Deal", damageStr, "damage to", characterStr, "by", sourceStr]


showEnchant :: Handle a -> AnyEnchantment a -> ShowCard String
showEnchant handle enchantment = case handle of
    SpellHandle {} -> showEnchant' handle enchantment
    WeaponHandle {} -> showEnchant' handle enchantment
    MinionHandle {} -> showEnchant' handle enchantment
    PlayerHandle {} -> showEnchant' handle enchantment
    MinionCharacter {} -> showEnchant' handle enchantment
    PlayerCharacter {} -> showEnchant' handle enchantment


showEnchant' :: (GenHandle a) => Handle a -> AnyEnchantment a -> ShowCard String
showEnchant' handle enchantment = do
    handleStr <- readHandle handle
    enchantmentStr <- case enchantment of
        ContinuousEnchantment e -> showEnchantment e
        LimitedEnchantment e -> showEnchantment e
    return $ unwords ["Give", handleStr, enchantmentStr]


showGainManaCrystals :: Handle 'Player' -> Int -> CrystalState -> ShowCard String
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


showEnchantment :: (GenHandle a) => Enchantment t a -> ShowCard String
showEnchantment = \case
    MinionEnchantment e -> showEnchantment e
    PlayerEnchantment e -> showEnchantment e
    Until timePoint enchantment -> showUntil timePoint enchantment
    DelayedEffect timePoint effect -> showDelayedEffect timePoint effect
    GainAttack (Attack x) -> return $ showWithSign x ++ " attack"
    GainHealth (Health x) -> return $ showWithSign x ++ " health"
    StatsScale (Attack x) (Health y) -> return $ "Scale stats by " ++ show x ++ "/" ++ show y
    ChangeStat e -> case e of
        Left (Attack x) -> return $ "Attack changed to " ++ show x
        Right (Health y) -> return $ "Health changed to " ++ show y
    SwapStats -> return "Swapped attack and health"
    Grant ability -> showAbility ability >>= \str -> return $ "Grant " ++ str
    Frozen -> return "Frozen"
    AttackDelta (Attack x) -> return $ "+" ++ show x ++ " Attack"


showDelayedEffect :: TimePoint -> Effect -> ShowCard String
showDelayedEffect timePoint effect = do
    timePointStr <- showTimePoint timePoint
    effectStr <- showEffect effect
    return $ effectStr ++ " at " ++ timePointStr


showUntil :: (GenHandle a) => TimePoint -> Enchantment 'Continuous' a -> ShowCard String
showUntil timePoint enchantment = do
    timePointStr <- showTimePoint timePoint
    enchantmentStr <- showEnchantment enchantment
    return $ enchantmentStr ++ " until " ++ timePointStr


showTimePoint :: TimePoint -> ShowCard String
showTimePoint = return . show


showWithSign :: IdInt -> String
showWithSign (IdInt _ n) = case n > 0 of
    True -> '+' : show n
    False -> show n





