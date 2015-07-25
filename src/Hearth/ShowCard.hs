{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}


module Hearth.ShowCard (
    showCard,
) where


--------------------------------------------------------------------------------


import Control.Applicative
import Control.Error
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
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


class GenHandle handle where
    genHandle :: String -> ShowCard handle


instance GenHandle PlayerHandle where
    genHandle = liftM PlayerHandle . rawGenHandle


instance GenHandle MinionHandle where
    genHandle = liftM MinionHandle . rawGenHandle


instance GenHandle SpellHandle where
    genHandle = liftM SpellHandle . rawGenHandle


instance GenHandle CharacterHandle where
    genHandle = liftM Left . genHandle


genNumberedHandle :: (GenHandle handle) => String -> ShowCard handle
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


class ReadHandle handle where
    readHandle :: handle -> ShowCard String


instance ReadHandle PlayerHandle where
    readHandle (PlayerHandle handle) = rawReadHandle handle


instance ReadHandle MinionHandle where
    readHandle (MinionHandle handle) = rawReadHandle handle


instance ReadHandle SpellHandle where
    readHandle (SpellHandle handle) = rawReadHandle handle


instance ReadHandle CharacterHandle where
    readHandle = \case
        Left handle -> readHandle handle
        Right handle -> readHandle handle


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
    HandCardSpell spell -> genHandle this >>= showEffect . _spellEffect spell


--------------------------------------------------------------------------------


showAbilities :: [Ability] -> ShowCard String
showAbilities = liftM unlines . mapM showAbility


showAbility :: Ability -> ShowCard String
showAbility = \case
    KeywordAbility ability -> showKeywordAbility ability


showKeywordAbility :: KeywordAbility -> ShowCard String
showKeywordAbility = \case
    Battlecry effectHole -> showBattlecry effectHole
    Charge -> return "Charge"
    DivineShield -> return "Divine Shield"
    Enrage enchantments -> liftM ("Enrage: " ++) $ showEnchantments enchantments
    Taunt -> return "Taunt"


showBattlecry :: (MinionHandle -> Effect) -> ShowCard String
showBattlecry effectHole = do
    effectStr <- genHandle this >>= showEffect . effectHole
    return $ "Battlecry: " ++ effectStr


showEffect :: Effect -> ShowCard String
showEffect = \case
    Elect elect -> showElect elect
    Sequence effects -> showSequence effects
    DrawCards handle n -> showDrawCards handle n
    KeywordEffect effect -> showKeywordEffect effect
    DealDamage handle damage -> showDealDamage handle damage
    Enchant handle enchantments -> showEnchant handle enchantments
    Give handle abilities -> showGive handle abilities
    GainManaCrystal crystalState handle -> showGainManaCrystal crystalState handle


showElect :: Elect -> ShowCard String
showElect = \case
    CasterOf handle effectHole -> showCasterOf handle effectHole
    OpponentOf handle effectHole -> showOpponentOf handle effectHole
    ControllerOf handle effectHole -> showControllerOf handle effectHole
    AnyCharacter selection effectHole -> showAnyCharacter selection effectHole
    AnyEnemy selection effectHole -> showAnyEnemy selection effectHole
    AnotherCharacter selection handle effectHole -> showAnotherCharacter selection handle effectHole
    AnotherMinion selection handle effectHole -> showAnotherMinion selection handle effectHole
    AnotherFriendlyMinion selection handle effectHole -> showAnotherFriendlyMinion selection handle effectHole
    OtherCharacters handle effectHole -> showOtherCharacters handle effectHole
    OtherEnemies handle effectHole -> showOtherEnemies handle effectHole


showSelection :: Selection -> String
showSelection = \case
    Targeted -> "TARGET_"
    AtRandom -> "RANDOM_"


showOtherEnemies :: CharacterHandle -> (CharacterHandle -> Effect) -> ShowCard String
showOtherEnemies character effectHole = do
    others <- readHandle character >>= \case
        (is this -> True) -> genNumberedHandle "OTHER_ENEMIES"
        str -> genHandle ("(OTHER_ENEMIES " ++ str ++ ")")
    showEffect $ effectHole others


showOtherCharacters :: CharacterHandle -> (CharacterHandle -> Effect) -> ShowCard String
showOtherCharacters character effectHole = do
    others <- readHandle character >>= \case
        (is this -> True) -> genNumberedHandle "OTHER_CHARACTERS"
        str -> genHandle ("(OTHER_CHARACTERS " ++ str ++ ")")
    showEffect $ effectHole others


showAnotherCharacter :: Selection -> CharacterHandle -> (CharacterHandle -> Effect) -> ShowCard String
showAnotherCharacter selection character effectHole = do
    let sel = showSelection selection
    other <- readHandle character >>= \case
        (is this -> True) -> genNumberedHandle $ sel ++ "CHARACTER"
        str -> genHandle ("(" ++ sel ++ "ANOTHER_CHARACTER " ++ str ++ ")")
    showEffect $ effectHole other


showAnyEnemy :: Selection -> (CharacterHandle -> Effect) -> ShowCard String
showAnyEnemy selection effectHole = do
    let sel = showSelection selection
    character <- genNumberedHandle $ sel ++ "ANY_ENEMY"
    showEffect $ effectHole character


showAnyCharacter :: Selection -> (CharacterHandle -> Effect) -> ShowCard String
showAnyCharacter selection effectHole = do
    let sel = showSelection selection
    character <- genNumberedHandle $ sel ++ "ANY_CHARACTER"
    showEffect $ effectHole character


showAnotherMinion :: Selection -> MinionHandle -> (MinionHandle -> Effect) -> ShowCard String
showAnotherMinion selection minion effectHole = do
    let sel = showSelection selection
    other <- readHandle minion >>= \case
        (is this -> True) -> genNumberedHandle $ sel ++ "TARGET_MINION"
        str -> genHandle ("(" ++ sel ++ "ANOTHER_MINION " ++ str ++ ")")
    showEffect $ effectHole other


showAnotherFriendlyMinion :: Selection -> MinionHandle -> (MinionHandle -> Effect) -> ShowCard String
showAnotherFriendlyMinion selection minion effectHole = do
    let sel = showSelection selection
    other <- readHandle minion >>= \case
        (is this -> True) -> genNumberedHandle $ sel ++ "FRIENDLY_MINION"
        str -> genHandle ("(" ++ sel ++ "ANOTHER_FRIENDLY_MINION " ++ str ++ ")")
    showEffect $ effectHole other


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


showGive :: MinionHandle -> [Ability] -> ShowCard String
showGive minion abilities = do
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





