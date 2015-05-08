{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}


module Hearth.Engine where


--------------------------------------------------------------------------------


import Control.Applicative
import Control.Error
import Control.Lens
import Control.Lens.Helper
import Control.Lens.Internal.Zoom (Zoomed, Focusing)
import Control.Monad.Loops
import Control.Monad.Prompt
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.State.Local
import Data.Data
import Data.Either.Combinators
import Data.Function
import Data.List
import Data.List.Ordered
import Data.Maybe
import qualified Data.NonEmpty as NonEmpty
import Hearth.Action
import Hearth.Cards (cardByName)
import Hearth.DebugEvent
import Hearth.DeckToHand
import Hearth.GameEvent
import Hearth.HandToDeck
import Hearth.Model
import Hearth.Names (CardName(BasicCardName))
import Hearth.Names.Basic (BasicCardName(TheCoin))
import Hearth.Prompt
import Language.Haskell.TH.Syntax (Name)


--------------------------------------------------------------------------------


type Pair a = (a, a)


newtype Hearth' st m a = Hearth {
    unHearth :: StateT st m a
} deriving (Functor, Applicative, Monad, MonadState st, MonadIO, MonadTrans)


instance (Monad m) => MonadReader st (Hearth' st m) where
    ask = get
    local = stateLocal


type Hearth = Hearth' GameState
type HearthMonad m = MonadPrompt HearthPrompt m


type instance Zoomed (Hearth' st m) = Focusing m


instance Monad m => Zoom (Hearth' st m) (Hearth' st' m) st st' where
    zoom l = Hearth . zoom l . unHearth


instance (HearthMonad m) => MonadPrompt HearthPrompt (Hearth' st m) where
    prompt = lift . prompt


class LogCall a where
    logCall :: Name -> a -> a


instance (HearthMonad m) => LogCall (Hearth' st m a) where
    logCall funcName m = do
        prompt $ PromptDebugEvent $ FunctionEntered funcName
        x <- m
        prompt $ PromptDebugEvent $ FunctionExited funcName
        return x


instance (HearthMonad m) => LogCall (a -> Hearth' st m b) where
    logCall msg f = logCall msg . f


instance (HearthMonad m) => LogCall (a -> b -> Hearth' st m c) where
    logCall msg f = logCall msg . f


data PlayerData = PlayerData Hero Deck
    deriving (Show, Typeable)


guardedPrompt :: (MonadPrompt p m) => p a -> (a -> Bool) -> m a
guardedPrompt p f = prompt p >>= \x -> case f x of
    True -> return x
    False -> guardedPrompt p f


isSubsetOf :: (Ord a) => [a] -> [a] -> Bool
isSubsetOf = subset `on` sort


runQuery :: (HearthMonad m) => GameSnapshot -> Hearth m a -> m a
runQuery snapshot query = evalStateT (unHearth query') $ snapshot^.snapshotGameState
    where
        query' = logCall 'runQuery query


runHearth :: (HearthMonad m) => Pair PlayerData -> m GameResult
runHearth = evalStateT (unHearth runGame) . mkGameState


newHandle :: (HearthMonad m) => Hearth m RawHandle
newHandle = do
    gameHandleSeed += 1
    view gameHandleSeed


mkGameState :: Pair PlayerData -> GameState
mkGameState (p1, p2) = let
    ps = [p1, p2]
    in GameState {
        _gameTurn = Turn 1,
        _gameHandleSeed = RawHandle $ length ps,
        _gamePlayerTurnOrder = [],
        _gamePlayers = zipWith mkPlayer (map PlayerHandle [0..]) ps }


mkPlayer :: PlayerHandle -> PlayerData -> Player
mkPlayer handle (PlayerData hero deck) = Player {
    _playerHandle = handle,
    _playerDeck = deck,
    _playerExcessDrawCount = 0,
    _playerHand = Hand [],
    _playerMinions = [],
    _playerTotalManaCrystals = 0,
    _playerEmptyManaCrystals = 0,
    _playerTemporaryManaCrystals = 0,
    _playerHero = mkBoardHero hero }


mkBoardHero :: Hero -> BoardHero
mkBoardHero hero = BoardHero {
    _boardHeroCurrHealth = _heroHealth hero,
    _boardHeroArmor = 0,
    _boardHero = hero }


getPlayer :: PlayerHandle -> Lens' GameState Player
getPlayer handle f st = fmap put' get'
    where
        players = st^.gamePlayers
        put' player = let
            g p = case p^.playerHandle == handle of
                True -> player
                False -> p
            in set gamePlayers (map g players) st
        get' = f $ fromJust $ find (\p -> p^.playerHandle == handle) players


getActivePlayerHandle :: (HearthMonad m) => Hearth m PlayerHandle
getActivePlayerHandle = logCall 'getActivePlayerHandle $ do
    (h : _) <- view gamePlayerTurnOrder
    return h


getNonActivePlayerHandle :: (HearthMonad m) => Hearth m PlayerHandle
getNonActivePlayerHandle = logCall 'getNonActivePlayerHandle $ do
    (_ : h : _) <- view gamePlayerTurnOrder
    return h


isActivePlayer :: (HearthMonad m) => PlayerHandle -> Hearth m Bool
isActivePlayer h = liftM (h ==) getActivePlayerHandle


getControllerOf :: (HearthMonad m) => MinionHandle -> Hearth m PlayerHandle
getControllerOf handle = logCall 'getControllerOf $ do
    players <- view gamePlayers
    let f minion = minion^.boardMinionHandle == handle
        players' = flip filter players $ \player -> any f $ player^.playerMinions
    case players' of
        [player] -> return $ player^.playerHandle
        _ -> $logicError 'getControllerOf "Inconsistent handles."


zoomPlayer :: (Zoom m n Player GameState, Functor (Zoomed m c), Zoomed n ~ Zoomed m) => PlayerHandle -> m c -> n c
zoomPlayer = zoom . getPlayer


zoomHero :: (Zoom m n BoardHero GameState, Functor (Zoomed m c), Zoomed n ~ Zoomed m) => PlayerHandle -> m c -> n c
zoomHero handle = zoom (getPlayer handle.playerHero)


getPlayerHandles :: (HearthMonad m) => Hearth m [PlayerHandle]
getPlayerHandles = viewListOf $ gamePlayers.traversed.playerHandle


genRawHandle :: (HearthMonad m) => Hearth m RawHandle
genRawHandle = do
    handle <- view $ gameHandleSeed
    gameHandleSeed += 1
    return handle


class GenHandle handle where
    genHandle :: (HearthMonad m) => Hearth m handle


instance GenHandle MinionHandle where
    genHandle = liftM MinionHandle genRawHandle


instance GenHandle SpellHandle where
    genHandle = liftM SpellHandle genRawHandle


runGame :: (HearthMonad m) => Hearth m GameResult
runGame = logCall 'runGame $ do
    prompt $ PromptGameEvent GameBegins
    initGame
    tickTurn
    let gameResult = GameResult
    prompt $ PromptGameEvent $ GameEnds gameResult
    return gameResult


initGame :: (HearthMonad m) => Hearth m ()
initGame = logCall 'initGame $ do
    flipCoin
    handles <- getPlayerHandles
    mapM_ initPlayer handles


flipCoin :: (HearthMonad m) => Hearth m ()
flipCoin = logCall 'flipCoin $ getPlayerHandles >>= \handles -> do
    handle <- prompt $ PromptPickRandom $ NonEmpty.fromList handles
    let handles' = dropWhile (/= handle) $ cycle handles
    gamePlayerTurnOrder .= handles'


initPlayer :: (HearthMonad m) => PlayerHandle -> Hearth m ()
initPlayer = initHand


initHand :: (HearthMonad m) => PlayerHandle -> Hearth m ()
initHand handle = logCall 'initHand $ do
    shuffleDeck handle
    isFirst <- isActivePlayer handle
    let numCards = case isFirst of
            True -> 3
            False -> 4
    drawnCards <- drawCards handle numCards
    keptCards <- guardedPrompt (PromptMulligan handle drawnCards) $ let
        in flip (on isSubsetOf $ map handCardName) drawnCards
    let tossedCards = foldr (deleteBy $ on (==) handCardName) keptCards drawnCards
        tossedCards' = map handToDeck tossedCards
    drawCards handle (length tossedCards) >>= \case
        [] -> return ()
        _ -> do
            getPlayer handle.playerDeck.deckCards %= (tossedCards' ++)
            shuffleDeck handle
    case isFirst of
        True -> return ()
        False -> let
            theCoin = deckToHand $ cardByName $ BasicCardName TheCoin
            in getPlayer handle.playerHand.handCards %= (theCoin :)


drawCards :: (HearthMonad m) => PlayerHandle -> Int -> Hearth m [HandCard]
drawCards handle = logCall 'drawCards $ liftM catMaybes . flip replicateM (drawCard handle)


putInHand :: (HearthMonad m) => PlayerHandle -> HandCard -> Hearth m Bool
putInHand handle card = logCall 'putInHand $ zoom (getPlayer handle.playerHand.handCards) $ do
    to length >>=. \case
        10 -> return False
        _ -> do
            id %= (card :)
            return True


removeFromHand :: (HearthMonad m) => PlayerHandle -> HandCard -> Hearth m Bool
removeFromHand handle card = logCall 'removeFromHand $ zoom (getPlayer handle.playerHand.handCards) $ do
    hand <- view id
    id %= deleteBy (on (==) handCardName) card
    hand' <- view id
    return $ length hand /= length hand'


drawCard :: (HearthMonad m) => PlayerHandle -> Hearth m (Maybe HandCard)
drawCard handle = logCall 'drawCard $ getPlayer handle.playerDeck >>=. \case
    Deck [] -> do
        getPlayer handle.playerExcessDrawCount += 1
        excess <- view $ getPlayer handle.playerExcessDrawCount
        damagePlayerHero handle $ Damage excess
        return Nothing
    Deck (c:cs) -> do
        let c' = deckToHand c
            deck = Deck cs
            promptDraw eCard = do
                prompt $ PromptGameEvent $ CardDrawn handle eCard deck
                return $ either (const Nothing) Just eCard
        getPlayer handle.playerDeck .= deck
        putInHand handle c' >>= \case
            False -> promptDraw $ Left c
            True -> promptDraw $ Right c'


damagePlayerHero :: (HearthMonad m) => PlayerHandle -> Damage -> Hearth m ()
damagePlayerHero handle damage = logCall 'damagePlayerHero $ zoomHero handle $ do
    let Damage amount = damage
        delta = Health amount
    oldHealth <- view boardHeroCurrHealth
    boardHeroCurrHealth -= delta
    prompt $ PromptGameEvent $ HeroTakesDamage handle oldHealth damage


isPlayerHeroDead :: (HearthMonad m) => PlayerHandle -> Hearth m Bool
isPlayerHeroDead handle = logCall 'isPlayerHeroDead $ do
    health <- view $ getPlayer handle.playerHero.boardHeroCurrHealth
    return $ health <= 0


shuffleDeck :: (HearthMonad m) => PlayerHandle -> Hearth m ()
shuffleDeck handle = logCall 'shuffleDeck $ zoomPlayer handle $ do
    Deck deck <- view playerDeck
    deck' <- let
        f = sort . map deckCardName
        in liftM Deck $ guardedPrompt (PromptShuffle deck) $ on (==) f deck
    prompt $ PromptGameEvent $ DeckShuffled handle deck'
    playerDeck .= deck'


isGameOver :: (HearthMonad m) => Hearth m Bool
isGameOver = logCall 'isGameOver $ do
    handles <- getPlayerHandles
    anyM isPlayerHeroDead handles


tickTurn :: (HearthMonad m) => Hearth m ()
tickTurn = do
    runTurn
    isGameOver >>= \case
        False -> tickTurn
        True -> return ()


runTurn :: (HearthMonad m) => Hearth m ()
runTurn = logCall 'runTurn $ do
    beginTurn
    pumpTurn
    endTurn


gainManaCrystal :: (HearthMonad m) => PlayerHandle -> CrystalState -> Hearth m ()
gainManaCrystal handle crystalState = logCall 'gainManaCrystal $ zoomPlayer handle $ do
    totalCount <- view playerTotalManaCrystals
    case totalCount of
        10 -> do
            let refillCount = case crystalState of
                    CrystalFull -> 1
                    CrystalEmpty -> 0
                    CrystalTemporary -> 1
            let realCount = case crystalState of
                    CrystalFull -> 1
                    CrystalEmpty -> 1
                    CrystalTemporary -> 0
            playerTemporaryManaCrystals %= max 0 . subtract realCount
            prompt $ PromptGameEvent $ GainsManaCrystal handle Nothing
            playerEmptyManaCrystals %= max 0 . subtract refillCount
            prompt $ PromptGameEvent $ ManaCrystalsRefill handle refillCount
        _ -> do
            playerTotalManaCrystals += 1
            case crystalState of
                CrystalFull -> return ()
                CrystalEmpty -> playerEmptyManaCrystals += 1
                CrystalTemporary -> playerTemporaryManaCrystals += 1
            prompt $ PromptGameEvent $ GainsManaCrystal handle $ Just crystalState


beginTurn :: (HearthMonad m) => Hearth m ()
beginTurn = logCall 'beginTurn $ do
    handle <- getActivePlayerHandle
    gainManaCrystal handle CrystalFull
    getPlayer handle.playerEmptyManaCrystals .= 0
    getPlayer handle.playerMinions.traversed.boardMinionAttackCount .= Right 0
    _ <- drawCard handle
    return ()


endTurn :: (HearthMonad m) => Hearth m ()
endTurn = logCall 'endTurn $ do
    handle <- getActivePlayerHandle
    zoomPlayer handle $ do
        tempCount <- view playerTemporaryManaCrystals
        playerTotalManaCrystals %= max 0 . subtract tempCount
        playerEmptyManaCrystals %= max 0 . subtract tempCount
        playerTemporaryManaCrystals .= 0
    gamePlayerTurnOrder %= tail


data TurnEvolution = ContinueTurn | EndTurn
    deriving (Eq)


pumpTurn :: (HearthMonad m) => Hearth m ()
pumpTurn = logCall 'pumpTurn $ do
    let cond = \case
            Nothing -> True
            Just EndTurn -> True
            Just ContinueTurn -> False
    _ <- iterateUntil cond pumpTurn'
    return ()


pumpTurn' :: (HearthMonad m) => Hearth m (Maybe TurnEvolution)
pumpTurn' = logCall 'pumpTurn' $ isGameOver >>= \case
    True -> return Nothing
    False -> performAction


performAction :: (HearthMonad m) => Hearth m (Maybe TurnEvolution)
performAction = logCall 'performAction $ do
    snapshot <- gets GameSnapshot
    prompt (PromptAction snapshot) >>= liftM Just . enactAction
    
    
enactAction :: (HearthMonad m) => Action -> Hearth m TurnEvolution
enactAction = logCall 'enactAction . \case
    ActionPlayerConceded _ -> $todo 'pumpTurn' "concede"
    ActionPlayMinion card pos -> actionPlayMinion card pos
    ActionPlaySpell card -> actionPlaySpell card
    ActionAttackMinion attacker defender -> actionAttackMinion attacker defender
    ActionEndTurn -> return EndTurn


isCardInHand :: (HearthMonad m) => PlayerHandle -> HandCard -> Hearth m Bool
isCardInHand handle card = logCall 'isCardInHand $ local id $ removeFromHand handle card


insertAt :: Int -> a -> [a] -> [a]
insertAt n x xs = let
    (left, right) = splitAt n xs
    in left ++ [x] ++ right


placeOnBoard :: (HearthMonad m) => PlayerHandle -> BoardPos -> Minion -> Hearth m (Maybe BoardMinion)
placeOnBoard handle (BoardPos pos) minion = logCall 'placeOnBoard $ do
    minionHandle <- genHandle
    let minion' = BoardMinion {
            _boardMinionDamage = 0,
            _boardMinionEnchantments = [],
            _boardMinionAbilities = minion^.minionAbilities,
            _boardMinionEnrageEnchantments = [],
            _boardMinionAttackCount = Left 0,
            _boardMinionHandle = minionHandle,
            _boardMinion = minion }
    zoom (getPlayer handle.playerMinions) $ do
        to length >>=. \case
            7 -> return Nothing
            len -> case 0 <= pos && pos <= len of
                False -> return Nothing
                True -> do
                    id %= insertAt pos minion'
                    return $ Just minion'


actionAttackMinion :: (HearthMonad m) => BoardMinion -> BoardMinion -> Hearth m TurnEvolution
actionAttackMinion attacker defender = logCall 'actionAttackMinion $ do
    _ <- attackMinion attacker defender
    return ContinueTurn


actionPlayMinion :: (HearthMonad m) => HandCard -> BoardPos -> Hearth m TurnEvolution
actionPlayMinion card pos = logCall 'actionPlayMinion $ do
    handle <- getActivePlayerHandle
    _ <- playMinion handle card pos
    return ContinueTurn


actionPlaySpell :: (HearthMonad m) => HandCard -> Hearth m TurnEvolution
actionPlaySpell card = logCall 'actionPlaySpell $ do
    handle <- getActivePlayerHandle
    _ <- playSpell handle card
    return ContinueTurn


playMinion :: (HearthMonad m) => PlayerHandle -> HandCard -> BoardPos -> Hearth m Result
playMinion handle card pos = logCall 'playMinion $ do
    st <- get
    mBoardMinion <- playMinion' handle card pos
    when (isNothing mBoardMinion) $ put st
    let result = maybe Failure (const Success) mBoardMinion
    prompt $ PromptGameEvent $ PlayedCard handle card result
    case mBoardMinion of
        Nothing -> return ()
        Just bm -> enactAnyBattleCries bm
    return result


playSpell :: (HearthMonad m) => PlayerHandle -> HandCard -> Hearth m Result
playSpell handle card = logCall 'playSpell $ do
    st <- get
    mSpell <- playSpell' handle card
    when (isNothing mSpell) $ put st
    let result = maybe Failure (const Success) mSpell
    prompt $ PromptGameEvent $ PlayedCard handle card result
    case mSpell of
        Nothing -> return ()
        Just spell -> enactSpell spell
    return result


enactSpell :: (HearthMonad m) => Spell -> Hearth m ()
enactSpell spell = let
    f = spell^.spellEffect
    in genHandle >>= enactEffect . f


enactAnyBattleCries :: (HearthMonad m) => BoardMinion -> Hearth m ()
enactAnyBattleCries minion = logCall 'enactAnyBattleCries $ do
    let minionHandle = minion^.boardMinionHandle
    forM_ (minion^.boardMinionAbilities) $ \case
        KeywordAbility (Battlecry effect) -> enactBattlecry minionHandle effect
        _ -> return ()


enactBattlecry :: (HearthMonad m) => MinionHandle -> (MinionHandle -> Effect) -> Hearth m ()
enactBattlecry handle = logCall 'enactBattlecry . enactEffect . ($ handle)


enactEffect :: (HearthMonad m) => Effect -> Hearth m ()
enactEffect = logCall 'enactEffect . \case
    With elect -> enactElect elect
    Sequence effects -> mapM_ enactEffect effects
    DrawCards handle n -> drawCards handle n >> return ()
    KeywordEffect effect -> enactKeywordEffect effect
    DealDamage handle damage -> dealDamage handle damage
    Enchant handle enchantments -> enchant handle enchantments
    Give handle abilities -> giveAbilities handle abilities
    GainManaCrystal handle crystalState -> gainManaCrystal handle crystalState


giveAbilities :: (HearthMonad m) => MinionHandle -> [Ability] -> Hearth m ()
giveAbilities handle abilities = logCall 'giveAbilities $ withMinions $ \bm -> let
    in return $ Just $ case bm^.boardMinionHandle == handle of
        False -> bm
        True -> bm & boardMinionAbilities %~ (abilities ++)


enchant :: (HearthMonad m) => MinionHandle -> [Enchantment] -> Hearth m ()
enchant handle enchantments = logCall 'enchant $ withMinions $ \bm -> let
    in return $ Just $ case bm^.boardMinionHandle == handle of
        False -> bm
        True -> bm & boardMinionEnchantments %~ (enchantments ++)


activateEnrage :: (HearthMonad m) => MinionHandle -> Hearth m ()
activateEnrage handle = logCall 'activateEnrage $ do
    withMinions $ liftM Just . \bm -> case bm^.boardMinionHandle == handle of
        False -> return bm
        True -> case bm^.boardMinionDamage == 0 of
            True -> return bm
            False -> case bm^.boardMinionEnrageEnchantments of
                [] -> let
                    enchantments = concat $ flip mapMaybe (bm^.boardMinionAbilities) $ \case
                        KeywordAbility (Enrage es) -> Just es
                        _ -> Nothing
                    bm' = bm & boardMinionEnrageEnchantments .~ enchantments
                    in case enchantments of
                         [] -> return bm
                         _ -> do
                            prompt $ PromptGameEvent $ EnrageActivated bm'
                            return bm'
                _ -> return bm


deactivateEnrage :: (HearthMonad m) => BoardMinion -> Hearth m ()
deactivateEnrage _ = logCall 'deactivateEnrage $ $todo 'deactivateEnrage "xxx"


allEnchantments :: BoardMinion -> [Enchantment]
allEnchantments bm = bm^.boardMinionEnrageEnchantments ++ bm^.boardMinionEnchantments


dynamicAttack :: (HearthMonad m) => BoardMinion -> Hearth m Attack
dynamicAttack bm = logCall 'dynamicAttack $ let
    delta = sum $ flip mapMaybe (allEnchantments bm) $ \case
        StatsDelta a _ -> Just a
    in return $ bm^.boardMinion.minionAttack + delta


dynamicHealth :: (HearthMonad m) => BoardMinion -> Hearth m Health
dynamicHealth bm = logCall 'dynamicHealth $ let
    delta = sum $ flip mapMaybe (allEnchantments bm) $ \case
        StatsDelta _ h -> Just h
    in return $ bm^.boardMinion.minionHealth + delta


dealDamage :: (HearthMonad m) => MinionHandle -> Damage -> Hearth m ()
dealDamage bmHandle damage = logCall 'dealDamage $ case damage <= 0 of
    True -> return ()
    False -> do
        withMinions $ \bm -> do
            liftM Just $ case bm^.boardMinionHandle == bmHandle of
                False -> return bm
                True -> case loseDivineShield bm of
                    Just bm' -> do
                        prompt $ PromptGameEvent $ LostDivineShield bm'
                        return bm'
                    Nothing -> do
                        let bm' = bm & boardMinionDamage +~ damage
                        prompt $ PromptGameEvent $ MinionTakesDamage bm damage
                        return bm'
        activateEnrage bmHandle
        clearDeadMinions


enactKeywordEffect :: (HearthMonad m) => KeywordEffect -> Hearth m ()
enactKeywordEffect = logCall 'enactKeywordEffect . \case
    Silence handle -> silence handle


silence :: (HearthMonad m) => MinionHandle -> Hearth m ()
silence victim = logCall 'silence $ do
    withMinions $ \bm -> case bm^.boardMinionHandle == victim of
        False -> return $ Just bm
        True -> do
            health <- dynamicHealth bm
            let bm' = bm & boardMinionAbilities .~ [] & boardMinionEnchantments .~ []
            health' <- dynamicHealth bm'
            let delta = Damage $ unHealth $ health' - health
            let bm'' = case delta < 0 of
                    True -> bm' & boardMinionDamage %~ max 0 . (+ delta)
                    False -> bm'
            prompt $ PromptGameEvent $ Silenced bm''
            return $ Just bm''


enactElect :: (HearthMonad m) => Elect -> Hearth m ()
enactElect = logCall 'enactElect . \case
    CasterOf _ f -> getActivePlayerHandle >>= enactEffect . f
    OpponentOf _ f -> getNonActivePlayerHandle >>= enactEffect . f
    ControllerOf minionHandle f -> getControllerOf minionHandle >>= enactEffect . f
    AnyCharacter f -> anyCharacter f
    AnotherCharacter bannedMinion f -> anotherCharacter bannedMinion f
    AnotherMinion bannedMinion f -> anotherMinion bannedMinion f
    AnotherFriendlyMinion bannedMinion f -> anotherFriendlyMinion bannedMinion f
    OtherCharacters bannedMinion f -> otherCharacters bannedMinion f


otherCharacters :: (HearthMonad m) => MinionHandle -> (MinionHandle -> Effect) -> Hearth m ()
otherCharacters bannedHandle f = logCall 'otherCharacters $ do
    handles <- getPlayerHandles
    targets <- liftM concat $ forM handles $ \handle -> do
        bms <- view $ getPlayer handle.playerMinions
        let bmHandles = map (\bm -> bm^.boardMinionHandle) bms
        return $ filter (/= bannedHandle) bmHandles
    forM_ targets $ enactEffect . f


anotherCharacter :: (HearthMonad m) => MinionHandle -> (MinionHandle -> Effect) -> Hearth m ()
anotherCharacter = logCall 'anotherCharacter anotherMinion


anyCharacter :: (HearthMonad m) => (MinionHandle -> Effect) -> Hearth m ()
anyCharacter f = logCall 'anotherMinion $ do
    handles <- getPlayerHandles
    targets <- liftM concat $ forM handles $ \handle -> do
        bms <- view $ getPlayer handle.playerMinions
        return $ map (\bm -> bm^.boardMinionHandle) bms
    pickMinionFrom targets >>= \case
        Nothing -> return ()
        Just target -> enactEffect $ f target


anotherMinion :: (HearthMonad m) => MinionHandle -> (MinionHandle -> Effect) -> Hearth m ()
anotherMinion bannedHandle f = logCall 'anotherMinion $ do
    handles <- getPlayerHandles
    targets <- liftM concat $ forM handles $ \handle -> do
        bms <- view $ getPlayer handle.playerMinions
        let bmHandles = map (\bm -> bm^.boardMinionHandle) bms
        return $ filter (/= bannedHandle) bmHandles
    pickMinionFrom targets >>= \case
        Nothing -> return ()
        Just target -> enactEffect $ f target


anotherFriendlyMinion :: (HearthMonad m) => MinionHandle -> (MinionHandle -> Effect) -> Hearth m ()
anotherFriendlyMinion bannedHandle f = logCall 'anotherFriendlyMinion $ do
    controller <- getControllerOf bannedHandle
    targets <- do
        bms <- view $ getPlayer controller.playerMinions
        let bmHandles = map (\bm -> bm^.boardMinionHandle) bms
        return $ filter (/= bannedHandle) bmHandles
    pickMinionFrom targets >>= \case
        Nothing -> return ()
        Just target -> enactEffect $ f target


pickMinionFrom :: (HearthMonad m) => [MinionHandle] -> Hearth m (Maybe MinionHandle)
pickMinionFrom = logCall 'pickMinionFrom . \case
    [] -> return Nothing
    handles -> do
        handle <- guardedPrompt (PromptPickRandom $ NonEmpty.fromList handles) (`elem` handles)
        return $ Just handle


playCommon :: (HearthMonad m) => PlayerHandle -> HandCard -> Hearth m Result
playCommon handle card = logCall 'playCommon $ removeFromHand handle card >>= \case
    False -> return Failure
    True -> payCost handle $ costOf card


playMinion' :: (HearthMonad m) => PlayerHandle -> HandCard -> BoardPos -> Hearth m (Maybe BoardMinion)
playMinion' handle card pos = logCall 'playMinion' $ playCommon handle card >>= \case
    Failure -> return Nothing
    Success -> case card of
        HandCardMinion minion -> placeOnBoard handle pos minion
        _ -> return Nothing


playSpell' :: (HearthMonad m) => PlayerHandle -> HandCard -> Hearth m (Maybe Spell)
playSpell' handle card = logCall 'playSpell' $ playCommon handle card >>= \case
    Failure -> return Nothing
    Success -> return $ case card of
        HandCardSpell spell -> Just spell
        _ -> Nothing


costOf :: HandCard -> Cost
costOf = \case
    HandCardMinion minion -> minion^.minionCost
    HandCardSpell spell -> spell^.spellCost


payCost :: (HearthMonad m) => PlayerHandle -> Cost -> Hearth m Result
payCost who = logCall 'payCost $  \case
    ManaCost mana -> payManaCost who mana


payManaCost :: (HearthMonad m) => PlayerHandle -> Mana -> Hearth m Result
payManaCost who (Mana cost) = logCall 'payManaCost $ zoomPlayer who $ do
    totalMana <- view playerTotalManaCrystals
    emptyMana <- view playerEmptyManaCrystals
    let availableMana = totalMana - emptyMana
    case cost <= availableMana of
        False -> return Failure
        True -> do
            playerEmptyManaCrystals += cost
            return Success


withMinions :: (HearthMonad m) => (BoardMinion -> Hearth m (Maybe BoardMinion)) -> Hearth m ()
withMinions f = logCall 'withMinions $ do
    ps <- view gamePlayers
    ps' <- forM ps $ \p -> do
        let bms = p^.playerMinions
        bms' <- liftM catMaybes $ mapM f bms
        return $ p & playerMinions .~ bms'
    gamePlayers .= ps'


clearDeadMinions :: (HearthMonad m) => Hearth m ()
clearDeadMinions = logCall 'clearDeadMinions $ withMinions $ \bm -> do
    threshold <- liftM (Damage . unHealth) $ dynamicHealth bm
    let alive = bm^.boardMinionDamage < threshold
    case alive of
        True -> return $ Just bm
        False -> do
            prompt $ PromptGameEvent $ MinionDied bm
            return Nothing


attackMinion :: (HearthMonad m) => BoardMinion -> BoardMinion -> Hearth m Result
attackMinion attacker defender = logCall 'attackMinion $ case attacker^.boardMinionAttackCount of
    Right 0 -> go
    Left 0 -> let
        hasCharge = flip any (attacker^.boardMinionAbilities) $ \case
            KeywordAbility Charge -> True
            _ -> False
        in case hasCharge of
            False -> return Failure
            True -> go
    _ -> return Failure
    where
        go = attackMinion' attacker defender


attackMinion' :: (HearthMonad m) => BoardMinion -> BoardMinion -> Hearth m Result
attackMinion' attacker defender = logCall 'attackMinion' $ do
    prompt $ PromptGameEvent $ AttackMinion attacker defender
    active <- getActivePlayerHandle
    attackerController <- getControllerOf $ attacker^.boardMinionHandle
    defenderController <- getControllerOf $ defender^.boardMinionHandle
    isLegal <- case attackerController == active of
        False -> return False
        True -> case defenderController == active of
            True -> return False
            False -> case hasTaunt defender of
                True -> return True
                False -> do
                    hasTaunts <- view $ getPlayer defenderController.playerMinions.to (any hasTaunt)
                    case hasTaunts of
                        True -> return False
                        False -> return True
    case isLegal of
        False -> return Failure
        True -> do
            let x `harms` y = do
                    dmg <- liftM (Damage . unAttack) $ dynamicAttack x
                    dealDamage (y^.boardMinionHandle) dmg
            attacker `harms` defender
            defender `harms` attacker
            withMinions $ \bm -> return $ Just $ case bm^.boardMinionHandle == attacker^.boardMinionHandle of
                True -> bm & boardMinionAttackCount %~ mapBoth succ succ
                False -> bm
            return Success
    where
        hasTaunt bm = flip any (bm^.boardMinionAbilities) $ \case
            KeywordAbility Taunt -> True
            _ -> False


replaceMinionByHandle :: (HearthMonad m) => BoardMinion -> Hearth m ()
replaceMinionByHandle bm' = logCall 'replaceMinionByHandle $ do
    controller <- getControllerOf $ bm'^.boardMinionHandle
    getPlayer controller.playerMinions %= \bms -> let
        (front, _ : end) = span (\bm -> bm^.boardMinionHandle /= bm'^.boardMinionHandle) bms
        in front ++ [bm'] ++ end
    

loseDivineShield :: BoardMinion -> Maybe BoardMinion
loseDivineShield bm = let
    abilities = bm^.boardMinionAbilities
    abilities' = flip filter abilities $ \case
        KeywordAbility DivineShield -> False
        _ -> True
    in case on (==) length abilities abilities' of
        True -> Nothing
        False -> Just $ bm & boardMinionAbilities .~ abilities'





























































