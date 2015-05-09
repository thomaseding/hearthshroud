{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}


module Hearth.ShowCard where


--------------------------------------------------------------------------------


import Control.Applicative
import Control.Error
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
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


class GenHandle handle where
    genHandle :: String -> ShowCard handle


instance GenHandle PlayerHandle where
    genHandle = liftM PlayerHandle . rawGenHandle


instance GenHandle MinionHandle where
    genHandle = liftM MinionHandle . rawGenHandle


instance GenHandle SpellHandle where
    genHandle = liftM SpellHandle . rawGenHandle


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


showCardAbilities :: DeckCard -> ShowCard String
showCardAbilities = liftM (unlines . filter (not . null) . lines) . \case
    DeckCardMinion minion -> showAbilities $ _minionAbilities minion
    DeckCardSpell spell -> genHandle this >>= showEffect . _spellEffect spell


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
showBattlecry effectHole = genHandle this >>= showEffect . effectHole


showEffect :: Effect -> ShowCard String
showEffect = \case
    With elect -> showElect elect
    Sequence effects -> showSequence effects
    DrawCards handle n -> showDrawCards handle n
    KeywordEffect effect -> showKeywordEffect effect
    DealDamage handle damage -> showDealDamage handle damage
    Enchant handle enchantments -> showEnchant handle enchantments
    Give handle abilities -> showGive handle abilities
    GainManaCrystal handle crystalState -> showGainManaCrystal handle crystalState


showElect :: Elect -> ShowCard String
showElect = \case
    CasterOf handle effectHole -> showCasterOf handle effectHole
    OpponentOf handle effectHole -> showOpponentOf handle effectHole
    ControllerOf handle effectHole -> showControllerOf handle effectHole
    AnyCharacter effectHole -> showAnyCharacter effectHole
    AnyEnemy effectHole -> showAnyEnemy effectHole
    AnotherCharacter handle effectHole -> showAnotherCharacter handle effectHole
    AnotherMinion handle effectHole -> showAnotherMinion handle effectHole
    AnotherFriendlyMinion handle effectHole -> showAnotherFriendlyMinion handle effectHole
    OtherCharacters handle effectHole -> showOtherCharacters handle effectHole
    OtherEnemies handle effectHole -> showOtherEnemies handle effectHole


showOtherEnemies :: MinionHandle -> (MinionHandle -> Effect) -> ShowCard String
showOtherEnemies minion effectHole = do
    others <- readHandle minion >>= \case
        (is this -> True) -> genNumberedHandle "OTHER_ENEMIES"
        str -> genHandle ("OtherEnemies " ++ str)
    showEffect $ effectHole others


showOtherCharacters :: MinionHandle -> (MinionHandle -> Effect) -> ShowCard String
showOtherCharacters minion effectHole = do
    others <- readHandle minion >>= \case
        (is this -> True) -> genNumberedHandle "OTHER_CHARACTERS"
        str -> genHandle ("OtherChracters " ++ str)
    showEffect $ effectHole others


showAnotherCharacter :: MinionHandle -> (MinionHandle -> Effect) -> ShowCard String
showAnotherCharacter minion effectHole = do
    other <- readHandle minion >>= \case
        (is this -> True) -> genNumberedHandle "TARGET_CHARACTER"
        str -> genHandle ("AnotherCharacter " ++ str)
    showEffect $ effectHole other


showAnyEnemy :: (MinionHandle -> Effect) -> ShowCard String
showAnyEnemy effectHole = do
    character <- genNumberedHandle "ANY_ENEMY"
    showEffect $ effectHole character


showAnyCharacter :: (MinionHandle -> Effect) -> ShowCard String
showAnyCharacter effectHole = do
    character <- genNumberedHandle "ANY_CHARACTER"
    showEffect $ effectHole character


showAnotherMinion :: MinionHandle -> (MinionHandle -> Effect) -> ShowCard String
showAnotherMinion minion effectHole = do
    other <- readHandle minion >>= \case
        (is this -> True) -> genNumberedHandle "TARGET_MINION"
        str -> genHandle ("AnotherMinion " ++ str)
    showEffect $ effectHole other


showAnotherFriendlyMinion :: MinionHandle -> (MinionHandle -> Effect) -> ShowCard String
showAnotherFriendlyMinion minion effectHole = do
    other <- readHandle minion >>= \case
        (is this -> True) -> genNumberedHandle "FRIENDLY_MINION"
        str -> genHandle ("AnotherFriendlyMinion " ++ str)
    showEffect $ effectHole other


showCasterOf :: SpellHandle -> (PlayerHandle -> Effect) -> ShowCard String
showCasterOf spell effectHole = do
    player <- readHandle spell >>= \case
        (is this -> True) -> genHandle you
        str -> genHandle ("CasterOf " ++ str)
    showEffect $ effectHole player


showControllerOf :: MinionHandle -> (PlayerHandle -> Effect) -> ShowCard String
showControllerOf minion effectHole = do
    player <- readHandle minion >>= \case
        (is this -> True) -> genHandle you
        str -> genHandle ("ControllerOf " ++ str)
    showEffect $ effectHole player


showOpponentOf :: PlayerHandle -> (PlayerHandle -> Effect) -> ShowCard String
showOpponentOf minion effectHole = do
    player <- readHandle minion >>= \case
        (is you -> True) -> genHandle opponent
        str -> genHandle ("OpponentOf " ++ str)
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


showDealDamage :: MinionHandle -> Damage -> ShowCard String
showDealDamage minion (Damage amount) = do
    minionStr <- readHandle minion
    return $ unwords ["Deal", show amount, "damage to", minionStr]


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


showGainManaCrystal :: PlayerHandle -> CrystalState -> ShowCard String
showGainManaCrystal player crystalState = do
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





