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
    HandCardSpell spell -> genHandle this >>= showElectionEffect . _spellEffect spell


--------------------------------------------------------------------------------


showAbilities :: [Ability] -> ShowCard String
showAbilities = liftM unlines . mapM showAbility


showAbility :: Ability -> ShowCard String
showAbility = \case
    KeywordAbility ability -> showKeywordAbility ability


showKeywordAbility :: KeywordAbility -> ShowCard String
showKeywordAbility = \case
    Battlecry effectHole -> showBattlecry effectHole
    Deathrattle effectHole -> showDeathrattle effectHole
    Charge -> return "Charge"
    DivineShield -> return "Divine Shield"
    Enrage abilities enchantments -> showEnrage abilities enchantments
    Taunt -> return "Taunt"


showEnrage :: [Ability] -> [Enchantment] -> ShowCard String
showEnrage abilities enchantments = do
    asStr <- mapM showAbility abilities
    esStr <- mapM showEnchantment enchantments
    return $ "Enrage: " ++ itemize (asStr ++ esStr)


showDeathrattle :: (MinionHandle -> Effect) -> ShowCard String
showDeathrattle effectHole = do
    effectStr <- genHandle this >>= showEffect . effectHole
    return $ "Deathrattle: " ++ effectStr


showBattlecry :: (Elect' a) => (MinionHandle -> ElectionEffect a) -> ShowCard String
showBattlecry effectHole = do
    effectStr <- genHandle this >>= showElectionEffect . effectHole
    return $ "Battlecry: " ++ effectStr


showEffect :: Effect -> ShowCard String
showEffect = \case
    Elect elect -> showElect elect
    With with -> showWith with
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


showRestoreHealth :: CharacterHandle -> Int -> ShowCard String
showRestoreHealth character amount = do
    characterStr <- readHandle character
    return $ "Restore " ++ show amount ++ " health on " ++ characterStr


showDestroyMinion :: MinionHandle -> ShowCard String
showDestroyMinion minion = do
    minionStr <- readHandle minion
    return $ "Destroy " ++ minionStr


showWith :: With -> ShowCard String
showWith = \case
    All x -> showAll x
    Unique x -> showUnique x


showForEach :: [Handle a] -> (Handle a -> Effect) -> ShowCard String
showForEach handles cont = case handles of
    [handle] -> do
        str <- readHandle handle
        effectStr <- showEffect $ cont handle
        return $ "ForEach " ++ str ++ "s: " ++ effectStr
    _ -> $logicError 'showForEach "xxx"


showElect :: (Elect' a) => Elect a -> ShowCard String
showElect = \case
    AnyCharacter effectHole -> showAnyCharacter effectHole
    AnyEnemy effectHole -> showAnyEnemy effectHole
    AnyMinion effectHole -> showAnyMinion effectHole
    AnotherCharacter handle effectHole -> showAnotherCharacter handle effectHole
    AnotherMinion handle effectHole -> showAnotherMinion handle effectHole
    AnotherFriendlyMinion handle effectHole -> showAnotherFriendlyMinion handle effectHole


showUnique :: Unique -> ShowCard String
showUnique = \case
    CasterOf handle effectHole -> showCasterOf handle effectHole
    OpponentOf handle effectHole -> showOpponentOf handle effectHole
    ControllerOf handle effectHole -> showControllerOf handle effectHole


showAll :: All -> ShowCard String
showAll = \case
    OtherCharacters handle effectHole -> showOtherCharacters handle effectHole
    OtherEnemies handle effectHole -> showOtherEnemies handle effectHole
    CharactersOf handle effectHole -> showCharactersOf handle effectHole
    MinionsOf handle effectHole -> showMinionsOf handle effectHole
    Players effectHole -> showPlayers effectHole
    Minions effectHole -> showMinions effectHole


class Elect' a where
    showElectionEffect :: ElectionEffect a -> ShowCard String
    showSelection :: Proxy a -> String


instance Elect' Targeted where
    showElectionEffect = \case
        Targeted elect -> showElect elect
        Effect effect -> showEffect effect
    showSelection _ = "TARGET_"


instance Elect' AtRandom where
    showElectionEffect (FromRandom effect) = showEffect effect
    showSelection _ = "RANDOM_"


showMinions :: ([MinionHandle] -> Effect) -> ShowCard String
showMinions effectHole = do
    other <- genHandle "MINION"
    showEffect $ effectHole [other]


showPlayers :: ([PlayerHandle] -> Effect) -> ShowCard String
showPlayers effectHole = do
    other <- genHandle "PLAYER"
    showEffect $ effectHole [other]


showMinionsOf :: PlayerHandle -> ([MinionHandle] -> Effect) -> ShowCard String
showMinionsOf handle effectHole = do
    other <- readHandle handle >>= \case
        (is you -> True) -> genHandle "FRIENDLY_MINION"
        _ -> genHandle "ENEMY_MINION"
    showEffect $ effectHole [other]


showCharactersOf :: PlayerHandle -> ([CharacterHandle] -> Effect) -> ShowCard String
showCharactersOf handle effectHole = do
    other <- readHandle handle >>= \case
        (is you -> True) -> genHandle "FRIENDLY_CHARACTER"
        _ -> genHandle "ENEMY_CHARACTER"
    showEffect $ effectHole [other]


showOtherEnemies :: CharacterHandle -> ([CharacterHandle] -> Effect) -> ShowCard String
showOtherEnemies character effectHole = do
    other <- readHandle character >>= \case
        (is this -> True) -> genNumberedHandle "OTHER_ENEMY"
        str -> genHandle ("(OTHER_ENEMY " ++ str ++ ")")
    showEffect $ effectHole [other]


showOtherCharacters :: CharacterHandle -> ([CharacterHandle] -> Effect) -> ShowCard String
showOtherCharacters character effectHole = do
    other <- readHandle character >>= \case
        (is this -> True) -> genNumberedHandle "OTHER_CHARACTER"
        str -> genHandle ("(OTHER_CHARACTER " ++ str ++ ")")
    showEffect $ effectHole [other]


showAnotherCharacter :: forall a. (Elect' a) => CharacterHandle -> (CharacterHandle -> ElectionEffect a) -> ShowCard String
showAnotherCharacter character effectHole = do
    let sel = showSelection (Proxy :: Proxy a)
    other <- readHandle character >>= \case
        (is this -> True) -> genNumberedHandle $ sel ++ "CHARACTER"
        str -> genHandle ("(" ++ sel ++ "ANOTHER_CHARACTER " ++ str ++ ")")
    showElectionEffect $ effectHole other


showAnyEnemy :: forall a. (Elect' a) => (CharacterHandle -> ElectionEffect a) -> ShowCard String
showAnyEnemy effectHole = do
    let sel = showSelection (Proxy :: Proxy a)
    character <- genNumberedHandle $ sel ++ "ANY_ENEMY"
    showElectionEffect $ effectHole character


showAnyCharacter :: forall a. (Elect' a) => (CharacterHandle -> ElectionEffect a) -> ShowCard String
showAnyCharacter effectHole = do
    let sel = showSelection (Proxy :: Proxy a)
    character <- genNumberedHandle $ sel ++ "ANY_CHARACTER"
    showElectionEffect $ effectHole character


showAnyMinion :: forall a. (Elect' a) => (MinionHandle -> ElectionEffect a) -> ShowCard String
showAnyMinion effectHole = do
    let sel = showSelection (Proxy :: Proxy a)
    minion <- genNumberedHandle $ sel ++ "ANY_MINION"
    showElectionEffect $ effectHole minion


showAnotherMinion :: forall a. (Elect' a) => MinionHandle -> (MinionHandle -> ElectionEffect a) -> ShowCard String
showAnotherMinion minion effectHole = do
    let sel = showSelection (Proxy :: Proxy a)
    other <- readHandle minion >>= \case
        (is this -> True) -> genNumberedHandle $ sel ++ "TARGET_MINION"
        str -> genHandle ("(" ++ sel ++ "ANOTHER_MINION " ++ str ++ ")")
    showElectionEffect $ effectHole other


showAnotherFriendlyMinion :: forall a. (Elect' a) => MinionHandle -> (MinionHandle -> ElectionEffect a) -> ShowCard String
showAnotherFriendlyMinion minion effectHole = do
    let sel = showSelection (Proxy :: Proxy a)
    other <- readHandle minion >>= \case
        (is this -> True) -> genNumberedHandle $ sel ++ "FRIENDLY_MINION"
        str -> genHandle ("(" ++ sel ++ "ANOTHER_FRIENDLY_MINION " ++ str ++ ")")
    showElectionEffect $ effectHole other


showCasterOf :: SpellHandle -> (PlayerHandle -> Effect) -> ShowCard String
showCasterOf spell effectHole = do
    player <- readHandle spell >>= \case
        (is this -> True) -> genHandle you
        str -> genHandle ("(CASTER_OF " ++ str ++ ")")
    showEffect $ effectHole player


showControllerOf :: MinionHandle -> (PlayerHandle -> Effect) -> ShowCard String
showControllerOf minion effectHole = do
    player <- readHandle minion >>= \case
        (is this -> True) -> genHandle you
        str -> genHandle ("(CONTROLLER_OF " ++ str ++ ")")
    showEffect $ effectHole player


showOpponentOf :: PlayerHandle -> (PlayerHandle -> Effect) -> ShowCard String
showOpponentOf minion effectHole = do
    player <- readHandle minion >>= \case
        (is you -> True) -> genHandle opponent
        str -> genHandle ("(OPPONENT_OF " ++ str ++ ")")
    showEffect $ effectHole player


showSequence :: [Effect] -> ShowCard String
showSequence = liftM itemize . mapM showEffect


showKeywordEffect :: KeywordEffect -> ShowCard String
showKeywordEffect = \case
    Silence handle -> showSilence handle


showSilence :: MinionHandle -> ShowCard String
showSilence minion = do
    minionStr <- readHandle minion
    return $ unwords ["Silence", minionStr]


showDrawCards :: PlayerHandle -> Int -> ShowCard String
showDrawCards player amount = do
    playerStr <- readHandle player
    let plural = case amount of
            1 -> ""
            _ -> "s"
        drawStr = "draw" ++ plural
        cardStr = "card" ++ plural
    return $ unwords [playerStr, drawStr, show amount, cardStr]


showDealDamage :: CharacterHandle -> Damage -> ShowCard String
showDealDamage character (Damage amount) = do
    characterStr <- readHandle character
    return $ unwords ["Deal", show amount, "damage to", characterStr]


showEnchant :: MinionHandle -> [Enchantment] -> ShowCard String
showEnchant minion enchantments = do
    minionStr <- readHandle minion
    enchantmentsStr <- showEnchantments enchantments
    return $ unwords ["Give", minionStr, enchantmentsStr]


showGiveAbility :: MinionHandle -> [Ability] -> ShowCard String
showGiveAbility minion abilities = do
    minionStr <- readHandle minion
    abilitiesStr <- showAbilities abilities
    return $ unwords ["Give", minionStr, abilitiesStr]


showGainManaCrystal :: CrystalState -> PlayerHandle -> ShowCard String
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





