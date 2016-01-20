{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Hearth.Engine (
    module Hearth.Engine,
    module Hearth.Engine.Data,
) where


--------------------------------------------------------------------------------


import Control.Error.TH
import Control.Lens hiding (Each, transform)
import Control.Lens.Helper
import Control.Monad.Loops
import Control.Monad.Prompt hiding (Effect)
import Control.Monad.Reader
import Control.Monad.State
import Data.Function
import Data.List
import Data.List.Ordered
import Data.Maybe
import Data.NonEmpty (NonEmpty)
import qualified Data.NonEmpty as NonEmpty
import Data.Proxy
import qualified Data.Set as Set
import Hearth.Action
import Hearth.CardName
import Hearth.Cards (cardByName, cardName)
import Hearth.CardSet.Basic.Names (BasicCardName(TheCoin))
import Hearth.Engine.Data
import Hearth.GameEvent
import Hearth.Model
import Hearth.Prompt


--------------------------------------------------------------------------------


instance (HearthMonad k m) => MonadPrompt (HearthPrompt k) (Hearth k m) where
    prompt p = do
        result <- lift (prompt p)
        case p of
            PromptGameEvent _ e -> handleGameEvent e
            _ -> return ()
        return result


promptGameEvent :: (HearthMonad k m) => GameEvent k -> Hearth k m ()
promptGameEvent event = do
    snap <- gets GameSnapshot
    prompt $ PromptGameEvent snap $ event


guardedPrompt :: (MonadPrompt p m) => p a -> (a -> m Bool) -> m a
guardedPrompt p predicate = prompt p >>= \x -> predicate x >>= \case
    True -> return x
    False -> guardedPrompt p predicate


isSubsetOf :: (Ord a) => [a] -> [a] -> Bool
isSubsetOf = subset `on` sort


runQuery :: (HearthMonad k m) => GameSnapshot k -> Hearth k m a -> m a
runQuery snapshot query = evalStateT (unHearth query') $ snapshot^.snapshotGameState
    where
        query' = logCall 'runQuery query


runHearth :: (HearthMonad k m) => Pair (PlayerData k) -> m GameResult
runHearth = evalStateT (unHearth runHearth') . mkGameState


mkGameState :: Pair (PlayerData k) -> GameState k
mkGameState (p1, p2) = let
    ps = [p1, p2]
    in GameState {
        _gameTurn = Turn 1,
        _gameHandleSeed = length ps,
        _gamePlayerTurnOrder = [],
        _gameEffectObservers = [],
        _gameBattlecryMinion = Nothing,
        _gamePlayers = zipWith mkPlayer (map (PlayerHandle . RawHandle ()) [0..]) ps }


mkPlayer :: Handle Player -> PlayerData k -> PlayerObject k
mkPlayer handle (PlayerData hero deck) = PlayerObject {
    _playerHandle = handle,
    _playerDeck = deck,
    _playerExcessDrawCount = 0,
    _playerHand = Hand [],
    _playerWeapon = Nothing,
    _playerMinions = [],
    _playerSpells = [],
    _playerEnchantments = [],
    _playerTotalManaCrystals = 0,
    _playerEmptyManaCrystals = 0,
    _playerTemporaryManaCrystals = 0,
    _playerHero = mkBoardHero hero }


mkBoardHero :: Hero k -> BoardHero k
mkBoardHero hero = BoardHero {
    _boardHeroDamage = 0,
    _boardHeroArmor = 0,
    _boardHeroAttackCount = 0,
    _boardHeroPower = hero^.heroPower,
    _boardHeroPowerCount = 0,
    _boardHero = hero }


getPlayer :: Handle Player -> Lens' (GameState k) (PlayerObject k)
getPlayer pHandle = lens getter setter
    where
        getter st = case find (\p -> p^.playerHandle == pHandle) $ st^.gamePlayers of
            Just p -> p
            Nothing -> $logicError 'getPlayer "Non-existent handle."
        setter st p' = st & gamePlayers.traversed %~ \p ->
            case p^.playerHandle == pHandle of
                True -> p'
                False -> p


getMinion :: Handle Minion -> Lens' (GameState k) (BoardMinion k)
getMinion bmHandle = lens getter setter
    where
        getter st = let
            players = st^.gamePlayers
            minions = players >>= \p -> p^.playerMinions
            in case find (\bm -> bm^.boardMinionHandle == bmHandle) minions of
                Just bm -> bm
                Nothing -> $logicError 'getMinion "Non-existent handle."
        setter st bm' = st & gamePlayers.traversed.playerMinions.traversed %~ \bm ->
            case bm^.boardMinionHandle == bmHandle of
                True -> bm'
                False -> bm


getWeapon :: Handle Weapon -> Lens' (GameState k) (BoardWeapon k)
getWeapon wHandle = lens getter setter
    where
        getter st = let
            players = st^.gamePlayers
            weapons = players >>= \p -> maybeToList $ p^.playerWeapon
            in case find (\weapon -> weapon^.boardWeaponHandle == wHandle) weapons of
                Just bm -> bm
                Nothing -> $logicError 'getWeapon "Non-existent handle."
        setter st weapon' = st & gamePlayers.traversed.playerWeapon.traversed %~ \weapon ->
            case weapon^.boardWeaponHandle == wHandle of
                True -> weapon'
                False -> weapon


getSpell :: Handle Spell -> Lens' (GameState k) (CastSpell k)
getSpell sHandle = lens getter setter
    where
        getter st = let
            players = st^.gamePlayers
            spells = players >>= \p -> p^.playerSpells
            in case find (\s -> s^.castSpellHandle == sHandle) spells of
                Just s -> s
                Nothing -> $logicError 'getMinion "Non-existent handle."
        setter st s' = st & gamePlayers.traversed.playerSpells.traversed %~ \s ->
            case s^.castSpellHandle == sHandle of
                True -> s'
                False -> s


getActivePlayerHandle :: (HearthMonad k m) => Hearth k m (Handle Player)
getActivePlayerHandle = logCall 'getActivePlayerHandle $ do
    (h : _) <- view gamePlayerTurnOrder
    return h


getNonActivePlayerHandle :: (HearthMonad k m) => Hearth k m (Handle Player)
getNonActivePlayerHandle = logCall 'getNonActivePlayerHandle $ do
    (_ : h : _) <- view gamePlayerTurnOrder
    return h


isActivePlayer :: (HearthMonad k m) => Handle Player -> Hearth k m Bool
isActivePlayer h = liftM (h ==) getActivePlayerHandle


class Ownable a where
    ownerOf :: (HearthMonad k m) => a -> Hearth k m (Handle Player)


instance Ownable (Handle a) where
    ownerOf = logCall 'ownerOf $ \case
        h @ SpellHandle {} -> do
            players <- view gamePlayers
            let isEq spell = spell^.castSpellHandle == h
                players' = flip filter players $ \player -> any isEq $ player^.playerSpells
            case players' of
                [player] -> return $ player^.playerHandle
                _ -> $logicError 'ownerOf $ "Invalid handle: " ++ show h
        h @ WeaponHandle {} -> do
            players <- view gamePlayers
            let isEq = \case
                    Nothing -> False
                    Just weapon -> weapon^.boardWeaponHandle == h
                players' = flip filter players $ \player -> isEq $ player^.playerWeapon
            case players' of
                [player] -> return $ player^.playerHandle
                _ -> $logicError 'ownerOf $ "Invalid handle: " ++ show h
        h @ MinionHandle {} -> do
            players <- view gamePlayers
            let isEq minion = minion^.boardMinionHandle == h
                players' = flip filter players $ \player -> any isEq $ player^.playerMinions
            case players' of
                [player] -> return $ player^.playerHandle
                _ -> $logicError 'ownerOf $ "Invalid handle: " ++ show h
        h @ PlayerHandle {} -> return h
        MinionCharacter h -> ownerOf h
        PlayerCharacter h -> ownerOf h


opponentOf :: (HearthMonad k m) => Handle Player -> Hearth k m (Handle Player)
opponentOf handle = logCall 'opponentOf $ do
    handles <- getPlayerHandles
    case filter (/= handle) handles of
        [opponent] -> return opponent
        _ -> $logicError 'enactOpponentOf "Opponent should exist and be unique."


getPlayerHandles :: (HearthMonad k m) => Hearth k m [Handle Player]
getPlayerHandles = viewListOf $ gamePlayers.traversed.playerHandle


genRawHandle :: (HearthMonad k m) => Hearth k m RawHandle
genRawHandle = do
    handle <- liftM (RawHandle ()) $ view gameHandleSeed
    gameHandleSeed += 1
    return handle


class GenHandle handle where
    genHandle :: (HearthMonad k m) => Hearth k m handle


instance GenHandle (Handle Minion) where
    genHandle = liftM MinionHandle genRawHandle


instance GenHandle (Handle Spell) where
    genHandle = liftM SpellHandle genRawHandle


instance GenHandle (Handle Weapon) where
    genHandle = liftM WeaponHandle genRawHandle


runHearth' :: (HearthMonad k m) => Hearth k m GameResult
runHearth' = logCall 'runHearth' $ do
    promptGameEvent GameBegins
    initGame
    tickTurn
    let gameResult = GameResult {}
    promptGameEvent $ GameEnds gameResult
    return gameResult


initGame :: (HearthMonad k m) => Hearth k m ()
initGame = logCall 'initGame $ do
    flipCoin
    handles <- getPlayerHandles
    mapM_ initPlayer handles


flipCoin :: (HearthMonad k m) => Hearth k m ()
flipCoin = logCall 'flipCoin $ getPlayerHandles >>= \handles -> do
    snap <- gets GameSnapshot
    AtRandomPick handle <- prompt $ PromptPickAtRandom snap $ PickPlayer $ NonEmpty.fromList handles
    let handles' = dropWhile (/= handle) $ cycle handles
    gamePlayerTurnOrder .= handles'


initPlayer :: (HearthMonad k m) => Handle Player -> Hearth k m ()
initPlayer = initHand


initHand :: (HearthMonad k m) => Handle Player -> Hearth k m ()
initHand handle = logCall 'initHand $ do
    shuffleDeck handle
    isFirst <- isActivePlayer handle
    let numCards = case isFirst of
            True -> 3
            False -> 4
    drawnCards <- drawCards handle numCards
    keptCards <- guardedPrompt (PromptMulligan handle drawnCards) $ \keptCards -> let
        in case on isSubsetOf (map cardName) keptCards drawnCards of
            True -> return True
            False -> do
                prompt $ PromptError InvalidMulligan
                return False
    let tossedCards = foldr (deleteBy $ on (==) cardName) keptCards drawnCards
        tossedCards' = map toDeckCard tossedCards
    drawCards handle (length tossedCards) >>= \case
        [] -> return ()
        _ -> do
            getPlayer handle.playerDeck.deckCards %= (tossedCards' ++)
            shuffleDeck handle
    case isFirst of
        True -> return ()
        False -> let
            theCoin = toHandCard $ cardByName $ BasicCardName TheCoin
            in getPlayer handle.playerHand.handCards %= (theCoin :)


drawCards :: (HearthMonad k m) => Handle Player -> Int -> Hearth k m [HandCard k]
drawCards handle = logCall 'drawCards $ liftM catMaybes . flip replicateM (drawCard handle)


putInHand :: (HearthMonad k m) => Handle Player -> HandCard k -> Hearth k m Bool
putInHand handle card = logCall 'putInHand $ zoom (getPlayer handle.playerHand.handCards) $ do
    to length >>=. \case
        MaxHandSize -> return False
        _ -> do
            id %= (card :)
            return True


removeFromHand :: (HearthMonad k m) => Handle Player -> HandCard k -> Hearth k m Bool
removeFromHand handle card = logCall 'removeFromHand $ zoom (getPlayer handle.playerHand.handCards) $ do
    hand <- view id
    id %= deleteBy (on (==) cardName) card
    hand' <- view id
    return $ length hand /= length hand'


drawCard :: (HearthMonad k m) => Handle Player -> Hearth k m (Maybe (HandCard k))
drawCard handle = logCall 'drawCard $ getPlayer handle.playerDeck >>=. \case
    Deck [] -> do
        getPlayer handle.playerExcessDrawCount += 1
        excess <- view $ getPlayer handle.playerExcessDrawCount
        enactDealDamage (PlayerCharacter handle) (Damage excess) Fatigue
        return Nothing
    Deck (c:cs) -> do
        let c' = toHandCard c
            deck = Deck cs
            promptDraw eCard = do
                promptGameEvent $ CardDrawn handle eCard deck
                return $ either (const Nothing) Just eCard
        getPlayer handle.playerDeck .= deck
        putInHand handle c' >>= \case
            False -> promptDraw $ Left c
            True -> promptDraw $ Right c'


isDead :: (HearthMonad k m) => Handle Character -> Hearth k m Bool
isDead character = logCall 'isDead $ isMortallyWounded character >>= \case
    True -> return True
    False -> case character of
        PlayerCharacter {} -> return False
        MinionCharacter minion -> view $ getMinion minion.boardMinionPendingDestroy


isMortallyWounded :: (HearthMonad k m) => Handle Character -> Hearth k m Bool
isMortallyWounded = logCall 'isMortallyWounded $ liftM (<= 0) . dynamic . viewRemainingHealth


shuffleDeck :: (HearthMonad k m) => Handle Player -> Hearth k m ()
shuffleDeck handle = logCall 'shuffleDeck $ do
    Deck deck <- view $ getPlayer handle.playerDeck
    deck' <- liftM Deck $ guardedPrompt (PromptShuffle deck) $ \deck' -> let
        f = sort . map cardName
        in case on (==) f deck deck' of
            True -> return True
            False -> do
                prompt $ PromptError InvalidShuffle
                return False
    getPlayer handle.playerDeck .= deck'
    promptGameEvent $ DeckShuffled handle deck'


isGameOver :: (HearthMonad k m) => Hearth k m Bool
isGameOver = logCall 'isGameOver $ do
    handles <- getPlayerHandles
    anyM (isDead . PlayerCharacter) handles


tickTurn :: (HearthMonad k m) => Hearth k m ()
tickTurn = do
    runTurn
    isGameOver >>= \case
        False -> tickTurn
        True -> return ()


runTurn :: (HearthMonad k m) => Hearth k m ()
runTurn = logCall 'runTurn $ do
    beginTurn
    pumpTurn
    endTurn


gainManaCrystals :: (HearthMonad k m) => Handle Player -> Int -> CrystalState -> Hearth k m ()
gainManaCrystals handle amount crystalState = logCall 'gainManaCrystals $ do
    replicateM_ amount $ gainManaCrystal handle crystalState


gainManaCrystal :: (HearthMonad k m) => Handle Player -> CrystalState -> Hearth k m ()
gainManaCrystal handle crystalState = logCall 'gainManaCrystal $ do
    totalCount <- view $ getPlayer handle.playerTotalManaCrystals
    case totalCount of
        MaxManaCrystals -> do
            let refillCount = case crystalState of
                    CrystalFull -> 1
                    CrystalEmpty -> 0
                    CrystalTemporary -> 1
            let realCount = case crystalState of
                    CrystalFull -> 1
                    CrystalEmpty -> 1
                    CrystalTemporary -> 0
            getPlayer handle.playerTemporaryManaCrystals %= max 0 . subtract realCount
            promptGameEvent $ GainsManaCrystal handle Nothing
            getPlayer handle.playerEmptyManaCrystals %= max 0 . subtract refillCount
            promptGameEvent $ ManaCrystalsRefill handle refillCount
        _ -> do
            getPlayer handle.playerTotalManaCrystals += 1
            zoom (getPlayer handle) $ case crystalState of
                CrystalFull -> return ()
                CrystalEmpty -> playerEmptyManaCrystals += 1
                CrystalTemporary -> playerTemporaryManaCrystals += 1
            promptGameEvent $ GainsManaCrystal handle $ Just crystalState


scopedPhase :: (HearthMonad k m) => Phase -> Hearth k m a -> Hearth k m a
scopedPhase phase action = logCall 'scopedPhase $ do
    promptGameEvent $ PhaseEvent $ Begin phase
    x <- action
    promptGameEvent $ PhaseEvent $ End phase
    return x


tickTimePointEnchantments :: (HearthMonad k m) => TimePoint -> Hearth k m ()
tickTimePointEnchantments timeEvent = logCall 'tickTimePointEnchantments $ do
    players <- viewListOf $ gamePlayers.traversed.playerHandle
    minions <- viewListOf $ gamePlayers.traversed.playerMinions.traversed.boardMinionHandle
    forM_ players $ \player -> do
        es <- view $ getPlayer player.playerEnchantments
        es' <- liftM catMaybes $ mapM generalConsume es
        getPlayer player.playerEnchantments .= es'
    forM_ minions $ \minion -> do
        es <- view $ getMinion minion.boardMinionEnchantments
        es' <- liftM catMaybes $ mapM generalConsume es
        getMinion minion.boardMinionEnchantments .= es'
    clearDeadMinions
    where
        tickTimePoint :: TimePoint -> Maybe TimePoint
        tickTimePoint = \case
            Delay n timePoint -> case timeEvent == timePoint of
                True -> case n of
                    0 -> tickTimePoint timePoint
                    _ -> Just $ Delay (n - 1) timePoint
                False -> Just $ Delay n timePoint
            timePoint -> case timeEvent == timePoint of
                True -> Nothing
                False -> Just timePoint

        generalConsume :: (HearthMonad k m) => AnyEnchantment k a -> Hearth k m (Maybe (AnyEnchantment k a))
        generalConsume = \case
            Continuous e -> liftM (liftM Continuous) $ continuousConsume e
            Limited e -> liftM (liftM Limited) $ limitedConsume e

        continuousConsume :: (HearthMonad k m) => Enchantment k Continuous a -> Hearth k m (Maybe (Enchantment k Continuous a))
        continuousConsume = return . Just

        limitedConsume :: (HearthMonad k m) => Enchantment k Limited a -> Hearth k m (Maybe (Enchantment k Limited a))
        limitedConsume = \case
            MinionEnchantment e -> liftM (liftM MinionEnchantment) $ limitedConsume e
            PlayerEnchantment e -> liftM (liftM PlayerEnchantment) $ limitedConsume e

            Until timePoint e -> return $ case tickTimePoint timePoint of
                Just timePoint' -> Just $ Until timePoint' e
                Nothing -> Nothing

            DelayedEffect timePoint e -> case tickTimePoint timePoint of
                Just timePoint' -> return $ Just $ DelayedEffect timePoint' e
                Nothing -> do
                    _ <- enactEffect e
                    return Nothing


beginTurn :: (HearthMonad k m) => Hearth k m ()
beginTurn = logCall 'beginTurn $ scopedPhase BeginTurnPhase $ do
    tickTimePointEnchantments BeginOfTurn
    handle <- getActivePlayerHandle
    gainManaCrystal handle CrystalFull
    zoom (getPlayer handle) $ do
        playerEmptyManaCrystals .= 0
        playerHero.boardHeroAttackCount .= 0
        playerHero.boardHeroPowerCount .= 0
        playerMinions.traversed.boardMinionAttackCount .= 0
        playerMinions.traversed.boardMinionNewlySummoned .= False
    _ <- drawCard handle
    return ()


endTurn :: (HearthMonad k m) => Hearth k m ()
endTurn = logCall 'endTurn $ scopedPhase EndTurnPhase $ do
    tickTimePointEnchantments EndOfTurn
    handle <- getActivePlayerHandle
    zoom (getPlayer handle) $ do
        tempCount <- view playerTemporaryManaCrystals
        playerTotalManaCrystals %= max 0 . subtract tempCount
        playerEmptyManaCrystals %= max 0 . subtract tempCount
        playerTemporaryManaCrystals .= 0
    gamePlayerTurnOrder %= tail


data EndTurn = EndTurn
    deriving (Eq)


pumpTurn :: (HearthMonad k m) => Hearth k m ()
pumpTurn = logCall 'pumpTurn $ do
    let cond = \case
            Nothing -> True
            Just evo -> case evo of
                Right EndTurn -> True
                Left () -> False
    _ <- iterateUntil cond pumpTurn'
    return ()


pumpTurn' :: (HearthMonad k m) => Hearth k m (Maybe (Either () EndTurn))
pumpTurn' = logCall 'pumpTurn' $ isGameOver >>= \case
    True -> return Nothing
    False -> liftM Just performAction


performAction :: (HearthMonad k m) => Hearth k m (Either () EndTurn)
performAction = logCall 'performAction $ do
    snapshot <- gets GameSnapshot
    evolution <- prompt (PromptAction snapshot) >>= enactAction
    clearDeadMinions
    return $ case evolution of
        Left _ -> Left ()
        Right EndTurn -> Right EndTurn


enactAction :: (HearthMonad k m) => Action k -> Hearth k m (Either Result EndTurn)
enactAction = logCall 'enactAction . \case
    ActionPlayerConceded p -> concede p >> return (Left Success)
    ActionPlayMinion card pos -> liftM Left $ actionPlayMinion card pos
    ActionPlaySpell card -> liftM Left $ actionPlaySpell card
    ActionPlayWeapon card -> liftM Left $ actionPlayWeapon card
    ActionAttack attacker defender -> liftM Left $ actionAttack attacker defender
    ActionEndTurn -> return $ Right EndTurn
    ActionHeroPower -> liftM Left actionHeroPower


actionHeroPower :: (HearthMonad k m) => Hearth k m Result
actionHeroPower = logCall 'actionHeroPower $ do
    st <- get
    handle <- getActivePlayerHandle
    hero <- view $ getPlayer handle.playerHero
    let power = hero^.boardHeroPower
        timesUsed = hero^.boardHeroPowerCount
        cost = power^.heroPowerCost
        cont = power^.heroPowerEffect
    case timesUsed == 0 of
        False -> return $ Failure "Hero power limit has been reached."
        True -> payCost handle cost >>= \case
            Failure msg -> return $ Failure msg
            Success -> do
                promptGameEvent $ UsedHeroPower handle power
                scopedPhase HeroPowerPhase $ enactElect (cont handle) >>= \case
                    NotAvailable -> do
                        put st
                        return $ Failure "No targets available."
                    Available result' -> case result' of
                        TargetedPick _ -> do
                            getPlayer handle.playerHero.boardHeroPowerCount += 1
                            return Success
                        AbortTargetedPick -> do
                            put st
                            return $ Failure "Targeted pick aborted."


concede :: (HearthMonad k m) => Handle Player -> Hearth k m ()
concede p = logCall 'concede $ do
    health <- dynamic $ viewMaxHealth p
    getPlayer p.playerHero.boardHeroDamage .= Damage (unHealth health)


isCardInHand :: (HearthMonad k m) => Handle Player -> HandCard k -> Hearth k m Bool
isCardInHand handle card = logCall 'isCardInHand $ local id $ removeFromHand handle card


insertAt :: Int -> a -> [a] -> [a]
insertAt n x xs = let
    (left, right) = splitAt n xs
    in left ++ [x] ++ right


toBoardMinion :: Handle Minion -> MinionCard k -> BoardMinion k
toBoardMinion handle minion = BoardMinion {
    _boardMinionDamage = 0,
    _boardMinionEnchantments = [],
    _boardMinionAbilities = minion^.minionAbilities,
    _boardMinionAttackCount = 0,
    _boardMinionNewlySummoned = True,
    _boardMinionPendingDestroy = False,
    _boardMinionHandle = handle,
    _boardMinion = minion }


summon :: (HearthMonad k m) => Handle Player -> MinionCard k -> BoardIndex -> Hearth k m (Either String (Handle Minion))
summon handle minion (BoardIndex idx) = logCall 'summon $ do
    minionHandle <- genHandle
    zoom (getPlayer handle.playerMinions) $ do
        to length >>=. \case
            MaxBoardMinionsPerPlayer -> return $ Left "Board is too full."
            len -> case 0 <= idx && idx <= len of
                False -> return $ Left "Invalid board index."
                True -> do
                    id %= insertAt idx (toBoardMinion minionHandle minion)
                    return $ Right minionHandle


actionAttack :: (HearthMonad k m) => Handle Character -> Handle Character -> Hearth k m Result
actionAttack attacker defender = logCall 'actionAttack $ do
    enactAttack attacker defender


actionPlayMinion :: (HearthMonad k m) => HandCard k -> BoardIndex -> Hearth k m Result
actionPlayMinion card idx = logCall 'actionPlayMinion $ do
    pHandle <- getActivePlayerHandle
    playMinion pHandle card idx


actionPlaySpell :: (HearthMonad k m) => HandCard k -> Hearth k m Result
actionPlaySpell card = logCall 'actionPlaySpell $ do
    handle <- getActivePlayerHandle
    playSpell handle card


actionPlayWeapon :: (HearthMonad k m) => HandCard k -> Hearth k m Result
actionPlayWeapon card = logCall 'actionPlayWeapon $ do
    handle <- getActivePlayerHandle
    playWeapon handle card


playMinion :: (HearthMonad k m) => Handle Player -> HandCard k -> BoardIndex -> Hearth k m Result
playMinion pHandle card idx = logCall 'playMinion $ do
    st <- get
    playMinion' pHandle card idx >>= \case
        Left msg -> do
            put st
            return $ Failure msg
        Right bmHandle -> do
            promptGameEvent $ PlayedMinion pHandle bmHandle
            result <- enactAnyBattlecries bmHandle
            when (result /= Success) $ put st
            return result


playMinion' :: (HearthMonad k m) => Handle Player -> HandCard k -> BoardIndex -> Hearth k m (Either String (Handle Minion))
playMinion' player card idx = logCall 'playMinion' $ playCommon player card >>= \case
    Failure msg -> return $ Left msg
    Success -> case card of
        HandCardMinion minion -> summon player minion idx
        _ -> return $ Left "Must pick a minion to play a minion."


playWeapon :: (HearthMonad k m) => Handle Player -> HandCard k -> Hearth k m Result
playWeapon pHandle card = logCall 'playWeapon $ do
    st <- get
    playWeapon' pHandle card >>= \case
        Left msg -> return $ Failure msg
        Right wHandle -> do
            promptGameEvent $ PlayedWeapon pHandle wHandle
            result <- enactAnyBattlecries wHandle
            when (result /= Success) $ put st
            return result


playWeapon' :: (HearthMonad k m) => Handle Player -> HandCard k -> Hearth k m (Either String (Handle Weapon))
playWeapon' player card = logCall 'playWeapon' $ playCommon player card >>= \case
    Failure msg -> return $ Left msg
    Success -> case card of
        HandCardWeapon weapon -> liftM Right $ equipWeapon player weapon
        _ -> return $ Left "Must pick a weapon to play a weapon."


playSpell :: (HearthMonad k m) => Handle Player -> HandCard k -> Hearth k m Result
playSpell pHandle card = logCall 'playSpell $ do
    st <- get
    result <- playSpell' pHandle card >>= \case
        Left msg -> return $ Failure msg
        Right sHandle -> do
            promptGameEvent $ PlayedSpell pHandle sHandle
            res <- enactSpell sHandle
            removeSpell sHandle
            return res
    when (result /= Success) $ put st
    return result


playSpell' :: (HearthMonad k m) => Handle Player -> HandCard k -> Hearth k m (Either String (Handle Spell))
playSpell' pHandle card = logCall 'playSpell' $ playCommon pHandle card >>= \case
    Failure msg -> return $ Left msg
    Success -> case card of
        HandCardSpell spell -> do
            sHandle <- genHandle
            let spell' = CastSpell {
                    _castSpellHandle = sHandle,
                    _castSpell = spell }
            getPlayer pHandle.playerSpells %= (spell' :)
            return $ Right sHandle
        _ -> return $ Left "Must pick a spell to play a spell."


equipWeapon :: (HearthMonad k m) => Handle Player -> WeaponCard k -> Hearth k m (Handle Weapon)
equipWeapon player weapon = do
    wHandle <- genHandle
    let weapon' = BoardWeapon {
            _boardWeaponDurability = weapon^.weaponDurability,
            _boardWeaponEnchantments = [],
            _boardWeaponAbilities = weapon^.weaponAbilities,
            _boardWeaponHandle = wHandle,
            _boardWeapon = weapon }
    getPlayer player.playerWeapon .= Just weapon'
    return wHandle


enactSpell :: (HearthMonad k m) => Handle Spell -> Hearth k m Result
enactSpell spell = logCall 'enactSpell $ scopedPhase SpellPhase $ do
    st <- get
    let abort msg = put st >> return (Failure msg)
    cont <- view $ getSpell spell.castSpell.spellEffect
    enactElect (cont spell) >>= \case
        Available (TargetedPick _) -> return Success
        Available AbortTargetedPick -> abort "Targeted pick aborted."
        NotAvailable -> abort "Not available."


enactAnyDeathrattles :: (HearthMonad k m) => Handle Minion -> Hearth k m ()
enactAnyDeathrattles bmHandle = logCall 'enactAnyDeathrattles $ do
    abilities <- dynamic $ viewMinionAbilities bmHandle
    forM_ abilities $ \case
        Deathrattle elect -> enactDeathrattle bmHandle elect
        _ -> return ()


enactAnyBattlecries :: (HearthMonad k m) => Handle a -> Hearth k m Result
enactAnyBattlecries handle = logCall 'enactAnyBattlecries $ do
    st <- get
    abilities <- dynamic $ viewAbilities handle
    result <- liftM condensePickResults $ forM abilities $ \case
        Battlecry effect -> enactBattlecry handle effect
        _ -> return $ purePick ()
    case result of
        NotAvailable -> do
            put st
            return Success
        Available result' -> case result' of
            TargetedPick _ -> return Success
            AbortTargetedPick -> do
                put st
                return $ Failure "Targeted pick aborted."


enactDeathrattle :: (HearthMonad k m) => Handle a -> (Handle a -> Elect k AtRandom) -> Hearth k m ()
enactDeathrattle handle cont = logCall 'enactDeathrattle $ scopedPhase DeathrattlePhase $ do
    _ <- enactElect $ cont handle
    return ()


enactBattlecry :: (HearthMonad k m) => Handle a -> (Handle a -> Elect k Targeted) -> Hearth k m (SimplePickResult Targeted)
enactBattlecry handle cont = logCall 'enactBattlecry $ scopedPhase BattlecryPhase $ do
    case handle of
        MinionHandle _ -> gameBattlecryMinion .= Just handle
        _ -> return ()
    result <- enactElect $ cont handle
    gameBattlecryMinion .= Nothing
    return result


whenM :: (Monad m) => m Bool -> m () -> m ()
whenM bool action = bool >>= flip when action


enactEffect :: (HearthMonad k m) => Effect k -> Hearth k m (SimplePickResult AtRandom)
enactEffect = logCall 'enactEffect . \case
    Elect elect -> enactEffectElect elect
    DoNothing -> return success
    Unreferenced _ -> return success
    ForEach handles cont -> enactForEach handles cont
    Sequence effects -> sequenceEffects effects
    If cond true false -> enactIf cond true false
    DrawCards handle n -> drawCards handle n >> return success
    DealDamage victim damage source -> enactDealDamage victim damage source >> return success
    Enchant handle enchantment -> enactEnchant handle enchantment >> return success
    GainManaCrystals handle amount crystalState -> gainManaCrystals handle amount crystalState >> return success
    DestroyMinion handle -> enactDestroyMinion handle >> return success
    DestroyWeapon handle -> enactDestroyWeapon handle >> return success
    EquipWeapon handle weapon -> equipWeapon handle weapon >> return success
    RestoreHealth character amount -> enactRestoreHealth character amount >> return success
    RestoreToFullHealth character -> enactRestoreToFullHealth character >> return success
    Transform handle minion -> transform handle minion >> return success
    Silence handle -> silence handle >> return success
    GainArmor handle armor -> gainArmor handle armor >> return success
    Freeze handle -> freeze handle >> return success
    Observing effect listener -> enactObserving effect listener
    PutInHand player card -> enactPutInHand player card >> return success
    Summon player minion loc -> enactSummon player minion loc >> return success
    RandomMissiles reqs n spell -> enactRandomMissiles reqs n spell >> return success
    DiscardAtRandom player -> enactDiscardAtRandom player >> return success
    TakeControl player minion -> enactTakeControl player minion >> return success
    where
        success = purePick ()


enactTakeControl :: (HearthMonad k m) => Handle Player -> Handle Minion -> Hearth k m ()
enactTakeControl newOwner victim = logCall 'enactTakeControl $ do
    oldOwner <- ownerOf victim
    case oldOwner == newOwner of
        True -> return ()
        False -> do
            view (getPlayer newOwner.playerMinions.to length) >>= \case
                MaxBoardMinionsPerPlayer -> enactDestroyMinion victim
                _ -> do
                    victimMinion <- view $ getMinion victim
                    getPlayer oldOwner.playerMinions %= filter (not . isVictim)
                    getPlayer newOwner.playerMinions %= (++ [victimMinion])
    where
        isVictim = (victim ==) . _boardMinionHandle


enactDiscardAtRandom :: (HearthMonad k m) => Handle Player -> Hearth k m ()
enactDiscardAtRandom player = logCall 'enactDiscardAtRandom $ do
    Hand cards <- view $ getPlayer player.playerHand
    pickFrom cards >>= \case
        NotAvailable -> return ()
        Available (AtRandomPick card) -> discardCard player card


discardCard :: (HearthMonad k m) => Handle Player -> HandCard k -> Hearth k m ()
discardCard player candidate = logCall 'discardCard $ do
    Hand cards <- view $ getPlayer player.playerHand
    let cards' = deleteBy (on (==) cardName) candidate cards
    getPlayer player.playerHand .= Hand cards'


getMinionCount :: (HearthMonad k m) => Handle Player -> Hearth k m Int
getMinionCount player = logCall 'getMinionCount $ do
    view $ getPlayer player.playerMinions.to length


boardIndexOf :: (HearthMonad k m) => Handle Minion -> Hearth k m BoardIndex
boardIndexOf minion = logCall 'boardIndexOf $ do
    owner <- ownerOf minion
    minions <- viewListOf $ getPlayer owner.playerMinions.traversed.boardMinionHandle
    case findIndex (== minion) minions of
        Just idx -> return $ BoardIndex idx
        Nothing -> $logicError 'boardIndexOf "xxx"


enactSummon :: (HearthMonad k m) => Handle Player -> MinionCard k -> BoardLocation -> Hearth k m ()
enactSummon player minion loc = do
    idx <- case loc of
        RightOf m -> boardIndexOf m >>= return . \case
            BoardIndex n -> BoardIndex $ n + 1
        Rightmost -> liftM BoardIndex $ getMinionCount player
    _ <- summon player minion idx
    return ()


enactPutInHand :: (HearthMonad k m) => Handle Player -> Card k -> Hearth k m ()
enactPutInHand player card = do
    _ <- putInHand player $ toHandCard card
    return ()


enactObserving :: (HearthMonad k m) => Effect k -> EventListener k -> Hearth k m (SimplePickResult AtRandom)
enactObserving effect listener = logCall 'enactObserving $ do
    originalObservers <- view gameEffectObservers
    gameEffectObservers %= (listener :)
    result <- enactEffect effect
    gameEffectObservers .= originalObservers
    return result


enactIf :: (HearthMonad k m) => Condition -> Effect k -> Effect k -> Hearth k m (SimplePickResult AtRandom)
enactIf cond true false = logCall 'enactIf $ enactCondition cond >>= enactEffect . \case
    True -> true
    False -> false


enactCondition :: (HearthMonad k m) => Condition -> Hearth k m Bool
enactCondition = logCall 'enactCondition $ \case
    Or x y -> enactOr x y
    And x y -> enactAnd x y
    Satisfies handle requirements -> satisfies handle requirements


enactOr :: (HearthMonad k m) => Condition -> Condition -> Hearth k m Bool
enactOr x y = enactCondition x >>= \case
    True -> return True
    False -> enactCondition y


enactAnd :: (HearthMonad k m) => Condition -> Condition -> Hearth k m Bool
enactAnd x y = enactCondition x >>= \case
    False -> return False
    True -> enactCondition y


freeze :: (HearthMonad k m) => Handle Character -> Hearth k m ()
freeze character = logCall 'freeze $ do
    active <- getActivePlayerHandle
    owner <- ownerOf character
    timePoint <- case active == owner of
        False -> return $ Delay 1 BeginOfTurn
        True -> liftM (== 0) (viewAttackCount character) >>= \case
            True -> return EndOfTurn
            False -> return $ Delay 1 EndOfTurn
    case character of
        PlayerCharacter player -> do
            getPlayer player.playerEnchantments %= (++ [Limited $ Until timePoint $ PlayerEnchantment Frozen])
        MinionCharacter minion -> do
            getMinion minion.boardMinionEnchantments %= (++ [Limited $ Until timePoint $ MinionEnchantment Frozen])


enactWhen :: (HearthMonad k m) => Handle a -> [Requirement a] -> Effect k -> Hearth k m ()
enactWhen handle requirements effect = do
    whenM (satisfies handle requirements) $ do
        _ <- enactEffect effect
        return ()


transform :: (HearthMonad k m) => Handle Minion -> MinionCard k -> Hearth k m ()
transform handle newMinion = logCall 'transform $ do
    getMinion handle .= toBoardMinion handle newMinion
    promptGameEvent $ Transformed handle newMinion


enactEffectElect :: (HearthMonad k m) => Elect k AtRandom -> Hearth k m (SimplePickResult AtRandom)
enactEffectElect elect = do
    mMinion <- view gameBattlecryMinion
    gameBattlecryMinion .= Nothing
    result <- enactElect elect >>= return . \case
        NotAvailable -> NotAvailable
        Available (AtRandomPick _) -> purePick ()
    gameBattlecryMinion .= mMinion
    return result


gainArmor :: (HearthMonad k m) => Handle Player -> Armor -> Hearth k m ()
gainArmor handle armor = logCall 'gainArmor $ do
    getPlayer handle.playerHero.boardHeroArmor += armor
    promptGameEvent $ GainedArmor handle armor


enactRestoreHealth :: (HearthMonad k m) => Handle Character -> Health -> Hearth k m ()
enactRestoreHealth character (Health amount) = logCall 'enactRestoreHealth $ do
    actualAmount <- case character of
        MinionCharacter minion -> zoom (getMinion minion.boardMinionDamage) restore
        PlayerCharacter player -> zoom (getPlayer player.playerHero.boardHeroDamage) restore
    case actualAmount of
        0 -> return ()
        _ -> promptGameEvent $ HealthRestored character (Health actualAmount)
    where
        restore = do
            before <- view id
            after <- id <%= max 0 . (subtract $ Damage amount)
            return $ unDamage $ before - after


enactRestoreToFullHealth :: (HearthMonad k m) => Handle Character -> Hearth k m ()
enactRestoreToFullHealth character = logCall 'enactRestoreToFullHealth $ do
    enactRestoreHealth character $ Health maxBound


enactDestroyMinion :: (HearthMonad k m) => Handle Minion -> Hearth k m ()
enactDestroyMinion handle = logCall 'enactDestroyMinion $ do
    getMinion handle.boardMinionPendingDestroy .= True
    promptGameEvent $ MinionDestroyed handle


enactForEach :: (HearthMonad k m) => HandleList a -> (Handle a -> Effect k) -> Hearth k m (SimplePickResult AtRandom)
enactForEach (HandleList _ handles) cont = logCall 'enactForEach $ do
    liftM condensePickResults $ forM handles (enactEffect . cont)


sequenceEffects :: (HearthMonad k m) => [Effect k] -> Hearth k m (SimplePickResult AtRandom)
sequenceEffects effects = liftM condensePickResults $ mapM enactEffect effects


condensePickResults :: (PurePick s) => [SimplePickResult s] -> SimplePickResult s
condensePickResults results = case dropWhile (== purePick ()) results of
    [] -> purePick ()
    r : _ -> r


enactEnchant :: (HearthMonad k m) => Handle a -> AnyEnchantment k a -> Hearth k m ()
enactEnchant = logCall 'enactEnchant $ \case
    SpellHandle _ -> const $ return ()
    h @ WeaponHandle{} -> enactEnchantWeapon h
    h @ PlayerHandle{} -> enactEnchantPlayer h
    h @ MinionHandle{} -> enactEnchantMinion h
    h @ PlayerCharacter{} -> enactEnchantCharacter h
    h @ MinionCharacter{} -> enactEnchantCharacter h


enactEnchantWeapon :: (HearthMonad k m) => Handle Weapon -> AnyEnchantment k Weapon -> Hearth k m ()
enactEnchantWeapon weapon enchantment = logCall 'enactEnchantWeapon $ do
    getWeapon weapon.boardWeaponEnchantments %= (++ [enchantment])


enactEnchantPlayer :: (HearthMonad k m) => Handle Player -> AnyEnchantment k Player -> Hearth k m ()
enactEnchantPlayer player enchantment = logCall 'enactEnchantPlayer $ do
    getPlayer player.playerEnchantments %= (++ [enchantment])


enactEnchantMinion :: (HearthMonad k m) => Handle Minion -> AnyEnchantment k Minion -> Hearth k m ()
enactEnchantMinion minion enchantment = logCall 'enactEnchantMinion $ do
    getMinion minion.boardMinionEnchantments %= (++ [enchantment])


enactEnchantCharacter :: (HearthMonad k m) => Handle Character -> AnyEnchantment k Character -> Hearth k m ()
enactEnchantCharacter character enchantment = logCall 'enactEnchantCharacter $ case character of
    PlayerCharacter player -> enactEnchantPlayer player $ liftE PlayerEnchantment enchantment
    MinionCharacter player -> enactEnchantMinion player $ liftE MinionEnchantment enchantment
    where
        liftE :: forall k a. (forall t. Enchantment k t Character -> Enchantment k t a) -> AnyEnchantment k Character -> AnyEnchantment k a
        liftE f = \case
            Limited e -> Limited $ f e
            Continuous e -> Continuous $ f e


viewIsDamaged :: BoardMinion k -> Bool
viewIsDamaged bm = bm^.boardMinionDamage > 0


viewAbilities :: (HearthMonad k m) => Handle a -> Hearth k m [Ability k a]
viewAbilities = \case
    h @ WeaponHandle{} -> viewWeaponAbilities h
    h @ MinionHandle{} -> viewMinionAbilities h
    _ -> $logicError 'viewAbilities "xxx"


viewWeaponAbilities :: (HearthMonad k m) => Handle Weapon -> Hearth k m [Ability k Weapon]
viewWeaponAbilities weapon = logCall 'viewWeaponAbilities $ do
    view $ getWeapon weapon.boardWeaponAbilities


viewMinionAbilities :: (HearthMonad k m) => Handle Minion -> Hearth k m [Ability k Minion]
viewMinionAbilities bmHandle = logCall 'viewMinionAbilities $ do
    bm <- view $ getMinion bmHandle
    return $ bm^.boardMinionAbilities >>= \ability -> case ability of
        Enrage abilities _ -> case viewIsDamaged bm of
            True -> ability : abilities  -- TODO: Need to check correct interleaving.
            False -> [ability]
        _ -> [ability]


viewMinionEnchantments :: (HearthMonad k m) => Handle Minion -> Hearth k m [AnyEnchantment k Minion]
viewMinionEnchantments bmHandle = logCall 'viewMinionEnchantments $ do
    bm <- view $ getMinion bmHandle
    let baseEnchantments = bm^.boardMinionEnchantments
        enrageEnchantments = case viewIsDamaged bm of
            False -> []
            True -> bm^.boardMinionAbilities >>= \case
                Enrage _ es -> es
                _ -> []
    return $ baseEnchantments ++ map Continuous enrageEnchantments -- TODO: Need to check correct interleaving.


viewPlayerEnchantments :: (HearthMonad k m) => Handle Player -> Hearth k m [AnyEnchantment k Player]
viewPlayerEnchantments player = logCall 'viewPlayerEnchantments $ do
    view $ getPlayer player.playerEnchantments


viewWeaponEnchantments :: (HearthMonad k m) => Handle Weapon -> Hearth k m [AnyEnchantment k Weapon]
viewWeaponEnchantments weapon = logCall 'viewWeaponEnchantments $ do
    view $ getWeapon weapon.boardWeaponEnchantments


underAnyEnchantment :: forall a b k. (forall t. Enchantment k t a -> b) -> AnyEnchantment k a -> b
underAnyEnchantment f = \case
    Continuous e -> f e
    Limited e -> f e


class SatisfiesEnchantment a where
    viewSatisfiesEnchantment :: (HearthMonad k m) => (forall t. Enchantment k t a -> Bool) -> Handle a -> Hearth k m Bool


instance SatisfiesEnchantment Minion where
    viewSatisfiesEnchantment p minion = logCall 'viewSatisfiesEnchantment $ do
        enchantments <- viewMinionEnchantments minion
        return $ any (underAnyEnchantment p) enchantments


instance SatisfiesEnchantment Player where
    viewSatisfiesEnchantment p player = logCall 'viewSatisfiesEnchantment $ do
        enchantments <- viewPlayerEnchantments player
        return $ any (underAnyEnchantment p) enchantments


discoverEnchantment :: (forall t2 a2. Enchantment k t2 a2 -> Bool) -> Enchantment k t a -> Bool
discoverEnchantment p = \case
    Until _ e -> discoverEnchantment p e
    MinionEnchantment e -> discoverEnchantment p e
    PlayerEnchantment e -> discoverEnchantment p e
    e -> p e


grantsFrozen :: Enchantment k t a -> Bool
grantsFrozen = \case
    MinionEnchantment e -> grantsFrozen e
    PlayerEnchantment e -> grantsFrozen e
    Until _ e -> grantsFrozen e
    Frozen -> True
    _ -> False


enactLoseDurability :: (HearthMonad k m) => Int -> Handle Weapon -> Hearth k m ()
enactLoseDurability n weapon = logCall 'enactLoseDurability $ do
    remaining <- zoom (getWeapon weapon) $ do
        boardWeaponDurability %= max 0 . subtract (Durability n)
        view boardWeaponDurability
    case remaining of
        0 -> enactDestroyWeapon weapon
        _ -> return ()


enactDestroyWeapon :: (HearthMonad k m) => Handle Weapon -> Hearth k m ()
enactDestroyWeapon weapon = logCall 'enactDestroyWeapon $ do
    owner <- ownerOf weapon
    getPlayer owner.playerWeapon .= Nothing


class IsCharacterHandle a where
    characterHandle :: a -> Handle Character


instance IsCharacterHandle (Handle Player) where
    characterHandle = PlayerCharacter


instance IsCharacterHandle (Handle Minion) where
    characterHandle = MinionCharacter


instance IsCharacterHandle (Handle Character) where
    characterHandle = id


instance IsCharacterHandle (BoardMinion k) where
    characterHandle = characterHandle . _boardMinionHandle


-- TODO: Make CharacterTraits not a class and just make it a constraint over a bunch of modular classes
instance IsCharacterHandle (PlayerObject k) where
    characterHandle = characterHandle . _playerHandle


-- TODO: Make CharacterTraits not a class and just make it a constraint over a bunch of modular classes
class (Ownable h, IsCharacterHandle h) => CharacterTraits h where
    bumpAttackCount :: (HearthMonad k m) => h -> Hearth k m ()
    viewAttackCount :: (HearthMonad k m) => h -> Hearth k m Int
    viewMaxAttackCount :: (HearthMonad k m) => h -> Hearth k m Int
    viewDamage :: (HearthMonad k m) => h -> Hearth k m Damage
    viewAttack :: (HearthMonad k m) => h -> Hearth k m Attack
    viewMaxHealth :: (HearthMonad k m) => h -> Hearth k m Health
    viewSummoningSickness :: (HearthMonad k m) => h -> Hearth k m Bool
    viewIsFrozen :: (HearthMonad k m) => h -> Hearth k m Bool


instance CharacterTraits (Handle Player) where
    bumpAttackCount player = logCall 'bumpAttackCount $ do
        getPlayer player.playerHero.boardHeroAttackCount += 1
        view (getPlayer player.playerWeapon) >>= \case
            Nothing -> return ()
            Just weapon -> enactLoseDurability 1 $ weapon^.boardWeaponHandle
    viewAttackCount pHandle = logCall 'viewAttackCount $ do
        view $ getPlayer pHandle.playerHero.boardHeroAttackCount
    viewMaxAttackCount _ = logCall 'viewMaxAttackCount $ do
        return 1
    viewDamage pHandle = logCall 'viewDamage $ do
        view $ getPlayer pHandle.playerHero.boardHeroDamage
    viewAttack pHandle = logCall 'viewAttack $ do
        view $ getPlayer pHandle.playerHero.boardHero.heroAttack
    viewMaxHealth pHandle = logCall 'viewMaxHealth $ do
        view $ getPlayer pHandle.playerHero.boardHero.heroHealth
    viewSummoningSickness _ = logCall 'viewSummoningSickness $ do
        return False
    viewIsFrozen = logCall 'viewIsFrozen $ do
        viewSatisfiesEnchantment grantsFrozen


auraAbilitiesOf :: [Ability k Minion] -> [Handle Minion -> Aura k]
auraAbilitiesOf = mapMaybe $ \case
    Aura aura -> Just aura
    _ -> Nothing


dynamic :: (HearthMonad k m) => Hearth k m a -> Hearth k m a
dynamic action = logCall 'dynamic $ local id $ do
    -- TODO: This function should not be use recursively. There should be a $logicError guard.
    enactMinionAuras
    applyMinionEnchantments
    applyPlayerEnchantments
    applyWeaponEnchantments
    applyPlayerWeapons
    action
    where
        viewAllMinions :: (HearthMonad k m) => Hearth k m [Handle Minion]
        viewAllMinions = viewListOf $ gamePlayers.traversed.playerMinions.traversed.boardMinionHandle

        viewAllWeapons :: (HearthMonad k m) => Hearth k m [Handle Weapon]
        viewAllWeapons = viewListOf $ gamePlayers.traversed.playerWeapon.traversed.boardWeaponHandle

        enactMinionAuras :: (HearthMonad k m) => Hearth k m ()
        enactMinionAuras = do
            minions <- viewAllMinions
            forM_ minions $ \minion -> do
                auras <- liftM auraAbilitiesOf $ viewMinionAbilities minion
                forM_ auras $ enactAura . ($ minion)

        applyMinionEnchantments :: (HearthMonad k m) => Hearth k m ()
        applyMinionEnchantments = do
            minions <- viewAllMinions
            forM_ minions $ \minion -> do
                enchantments <- viewMinionEnchantments minion
                forM_ enchantments $ underAnyEnchantment $ applyMinionEnchantment minion

        applyWeaponEnchantments :: (HearthMonad k m) => Hearth k m ()
        applyWeaponEnchantments = do
            weapons <- viewAllWeapons
            forM_ weapons $ \weapon -> do
                enchantments <- viewWeaponEnchantments weapon
                forM_ enchantments $ underAnyEnchantment $ applyWeaponEnchantment weapon

        applyPlayerEnchantments :: (HearthMonad k m) => Hearth k m ()
        applyPlayerEnchantments = do
            players <- viewListOf $ gamePlayers.traversed.playerHandle
            forM_ players $ \player -> do
                enchantments <- viewPlayerEnchantments player
                forM_ enchantments $ underAnyEnchantment $ applyPlayerEnchantment player

        applyWeaponEnchantment :: (HearthMonad k m) => Handle Weapon -> Enchantment k t Weapon -> Hearth k m ()
        applyWeaponEnchantment weapon = \case
            Until _ enchantment -> applyWeaponEnchantment weapon enchantment
            AttackDelta attack -> getWeapon weapon.boardWeapon.weaponAttack += attack
            Grant _ -> $todo 'dynamic "xxx"

        applyPlayerEnchantment :: (HearthMonad k m) => Handle Player -> Enchantment k t Player -> Hearth k m ()
        applyPlayerEnchantment player = \case
            PlayerEnchantment e -> applyCharacterEnchantment (PlayerCharacter player) e
            Until _ enchantment -> applyPlayerEnchantment player enchantment
            Grant _ -> $todo 'dynamic "xxx"

        applyMinionEnchantment :: (HearthMonad k m) => Handle Minion -> Enchantment k t Minion -> Hearth k m ()
        applyMinionEnchantment minion = \case
            MinionEnchantment e -> applyCharacterEnchantment (MinionCharacter minion) e
            StatsScale a h -> do
                getMinion minion.boardMinion.minionAttack *= a
                getMinion minion.boardMinion.minionHealth *= h
            ChangeStat e -> case e of
                Left a -> getMinion minion.boardMinion.minionAttack .= a
                Right h -> getMinion minion.boardMinion.minionHealth .= h
            SwapStats -> do
                Damage damage <- view $ getMinion minion.boardMinionDamage
                Attack attack <- view $ getMinion minion.boardMinion.minionAttack
                Health health <- view $ getMinion minion.boardMinion.minionHealth
                getMinion minion.boardMinionDamage .= 0
                getMinion minion.boardMinion.minionAttack .= Attack (health - damage)
                getMinion minion.boardMinion.minionHealth .= Health attack
            Grant ability -> getMinion minion.boardMinionAbilities %= (++ [ability])
            Until _ enchantment -> applyMinionEnchantment minion enchantment
            DelayedEffect {} -> return ()

        applyCharacterEnchantment :: (HearthMonad k m) => Handle Character -> Enchantment k t Character -> Hearth k m ()
        applyCharacterEnchantment character = \case
            Until _ enchantment -> applyCharacterEnchantment character enchantment
            Frozen -> return ()
            StatsDelta a h -> case character of
                MinionCharacter minion -> do
                    getMinion minion.boardMinion.minionAttack += a
                    getMinion minion.boardMinion.minionHealth += h
                PlayerCharacter player -> do
                    getPlayer player.playerHero.boardHero.heroAttack += a
                    getPlayer player.playerHero.boardHero.heroHealth += h
            Grant _ -> $todo 'dynamic "xxx"

        applyPlayerWeapons :: (HearthMonad k m) => Hearth k m ()
        applyPlayerWeapons = view gamePlayers >>= mapM_ (applyPlayerWeapon . _playerHandle)

        applyPlayerWeapon :: (HearthMonad k m) => Handle Player -> Hearth k m ()
        applyPlayerWeapon player = zoom (getPlayer player) $ do
            view playerWeapon >>= \case
                Nothing -> return ()
                Just weapon -> playerHero.boardHero.heroAttack += weapon^.boardWeapon.weaponAttack


enactAura :: (HearthMonad k m) => Aura k -> Hearth k m ()
enactAura = logCall 'enactAura $ \case
    AuraOwnerOf handle cont -> ownerOf handle >>= enactAura . cont
    AuraOpponentOf handle cont -> opponentOf handle >>= enactAura . cont
    While handle requirements aura -> enactWhile handle requirements aura
    EachMinion requirements cont -> enactEachMinion requirements cont
    Has handle enchantment -> enactHas handle enchantment
    HasAbility handle ability -> enactHasAbility handle ability


enactEachMinion :: (HearthMonad k m) => [Requirement Minion] -> (Handle Minion -> Aura k) -> Hearth k m ()
enactEachMinion requirements cont = logCall 'enactEachMinion $ do
    minions <- viewListOf $ gamePlayers.traversed.playerMinions.traversed.boardMinionHandle
    forM_ minions $ \minion -> do
        whenM (satisfies minion requirements) $ enactAura $ cont minion


enactWhile :: (HearthMonad k m) => Handle a -> [Requirement a] -> Aura k -> Hearth k m ()
enactWhile handle requirements = logCall 'enactWhile $ whenM (satisfies handle requirements) . enactAura


enactHas :: (HearthMonad k m) => Handle Minion -> Enchantment k Continuous Minion -> Hearth k m ()
enactHas handle enchantment = logCall 'enactHas $ do
    getMinion handle.boardMinionEnchantments %= (++ [Continuous enchantment])


enactHasAbility :: (HearthMonad k m) => Handle Minion -> Ability k Minion -> Hearth k m ()
enactHasAbility handle ability = logCall 'enactHasAbility $ do
    getMinion handle.boardMinionAbilities %= (++ [ability])


instance CharacterTraits (Handle Minion) where
    bumpAttackCount bmHandle = logCall 'bumpAttackCount $ do
        getMinion bmHandle.boardMinionAttackCount += 1
    viewAttackCount bmHandle = logCall 'viewAttackCount $ do
        view $ getMinion bmHandle.boardMinionAttackCount
    viewMaxAttackCount minion = logCall 'viewMaxAttackCount $ do
        viewWindfury minion >>= return . \case
            True -> 2
            False -> 1
    viewDamage bmHandle = logCall 'viewDamage $ do
        view $ getMinion bmHandle.boardMinionDamage
    viewAttack bmHandle = logCall 'viewAttack $ do
        view $ getMinion bmHandle.boardMinion.minionAttack
    viewMaxHealth bmHandle = logCall 'viewMaxHealth $ do
        view $ getMinion bmHandle.boardMinion.minionHealth
    viewSummoningSickness bmHandle = logCall 'viewSummoningSickness $ do
        bm <- view $ getMinion bmHandle
        case bm^.boardMinionNewlySummoned of
            False -> return False
            True -> liftM not $ viewCharge bmHandle
    viewIsFrozen = logCall 'viewIsFrozen $ viewSatisfiesEnchantment grantsFrozen


instance CharacterTraits (Handle Character) where
    bumpAttackCount = logCall 'bumpAttackCount $ \case
        PlayerCharacter h -> bumpAttackCount h
        MinionCharacter h -> bumpAttackCount h
    viewAttackCount = logCall 'viewAttackCount $ \case
        PlayerCharacter h -> viewAttackCount h
        MinionCharacter h -> viewAttackCount h
    viewMaxAttackCount = logCall 'viewMaxAttackCount $ \case
        PlayerCharacter h -> viewMaxAttackCount h
        MinionCharacter h -> viewMaxAttackCount h
    viewDamage = logCall 'viewDamage $ \case
        PlayerCharacter h -> viewDamage h
        MinionCharacter h -> viewDamage h
    viewAttack = logCall 'viewAttack $ \case
        PlayerCharacter h -> viewAttack h
        MinionCharacter h -> viewAttack h
    viewMaxHealth = logCall 'viewMaxHealth $ \case
        PlayerCharacter h -> viewMaxHealth h
        MinionCharacter h -> viewMaxHealth h
    viewSummoningSickness = logCall 'viewSummoningSickness $ \case
        PlayerCharacter h -> viewSummoningSickness h
        MinionCharacter h -> viewSummoningSickness h
    viewIsFrozen = logCall 'viewIsFrozen $ \case
        PlayerCharacter h -> viewIsFrozen h
        MinionCharacter h -> viewIsFrozen h


viewRemainingHealth :: (HearthMonad k m) => Handle Character -> Hearth k m Health
viewRemainingHealth h = logCall 'viewRemainingHealth $ do
    damage <- viewDamage h
    health <- viewMaxHealth h
    return $ health - Health (unDamage damage)


viewSpellDamage :: (HearthMonad k m) => Handle Player -> Hearth k m Int
viewSpellDamage player = logCall 'viewSpellDamage $ do
    minions <- viewListOf $ getPlayer player.playerMinions.traversed.boardMinionHandle
    liftM sum $ forM minions $ \minion -> do
        abilities <- viewMinionAbilities minion
        return $ sum $ flip mapMaybe abilities $ \case
            SpellDamage n -> Just n
            _ -> Nothing


enactRandomMissiles :: (HearthMonad k m) => [Requirement Character] -> Int -> Handle Spell -> Hearth k m ()
enactRandomMissiles reqs n spell = logCall 'enactRandomMissiles $ do
    modifier <- ownerOf spell >>= dynamic . viewSpellDamage
    enactRandomMissilesPrim reqs (n + modifier) spell


enactRandomMissilesPrim :: (HearthMonad k m) => [Requirement Character] -> Int -> Handle Spell -> Hearth k m ()
enactRandomMissilesPrim reqs n spell = logCall 'enactRandomMissilesPrim $ case n of
    0 -> return ()
    _ -> do
        players <- viewListOf $ gamePlayers.traversed.playerHandle
        minions <- viewListOf $ gamePlayers.traversed.playerMinions.traversed.boardMinionHandle
        let characters = map PlayerCharacter players ++ map MinionCharacter minions
        restrict reqs characters >>= pickFrom >>= \case
            NotAvailable -> return ()
            Available (AtRandomPick victim) -> do
                dealDamagePrim victim 1 $ DamagingSpell spell
                enactRandomMissilesPrim reqs (n - 1) spell


enactDealDamage :: (HearthMonad k m) => Handle Character -> Damage -> DamageSource -> Hearth k m ()
enactDealDamage charHandle (Damage baseDamage) source = logCall 'enactDealDamage $ do
    modifier <- case source of
        DamagingSpell spell -> ownerOf spell >>= dynamic . viewSpellDamage
        _ -> return 0
    let damage = Damage $ baseDamage + modifier
    dealDamagePrim charHandle damage source


dealDamagePrim :: (HearthMonad k m) => Handle Character -> Damage -> DamageSource -> Hearth k m ()
dealDamagePrim charHandle damage source = logCall 'dealDamagePrim $ case damage <= 0 of
    True -> return ()
    False -> case charHandle of
        PlayerCharacter handle -> do
            bh <- view $ getPlayer handle.playerHero
            let dmg = unDamage damage
                armor = bh^.boardHeroArmor
                armor' = max 0 $ armor - Armor dmg
                armorDamage = Damage $ unArmor $ armor - armor'
                healthDamage = damage - armorDamage
            zoom (getPlayer handle.playerHero) $ do
                boardHeroDamage += healthDamage
                boardHeroArmor .= armor'
            promptGameEvent $ DealtDamage charHandle damage source
        MinionCharacter handle -> do
            bm <- view $ getMinion handle
            case loseDivineShield bm of
                Just bm' -> do
                    getMinion handle .= bm'
                    promptGameEvent $ LostDivineShield $ handle
                Nothing -> do
                    let bm' = bm & boardMinionDamage +~ damage
                    getMinion handle .= bm'
                    promptGameEvent $ DealtDamage charHandle damage source


silence :: (HearthMonad k m) => Handle Minion -> Hearth k m ()
silence victim = logCall 'silence $ do
    health <- dynamic $ viewMaxHealth victim
    zoom (getMinion victim) $ do
        boardMinionAbilities .= []
        boardMinionEnchantments .= []
    health' <- dynamic $ viewMaxHealth victim
    getMinion victim %= \bm -> let
        delta = Damage $ unHealth $ health' - health
        in case delta < 0 of
            True -> bm & boardMinionDamage %~ max 0 . (+ delta)
            False -> bm
    promptGameEvent $ Silenced victim


data Available a = Available a | NotAvailable
    deriving (Show, Eq, Ord)


type SimplePickResult s = Available (PickResult s ())


class (Eq (PickResult s ())) => PurePick s where
    purePick :: a -> Available (PickResult s a)


instance PurePick Targeted where
    purePick = Available . TargetedPick


instance PurePick AtRandom where
    purePick = Available . AtRandomPick


enactElect :: (HearthMonad k m, PickFrom s) => Elect k s -> Hearth k m (SimplePickResult s)
enactElect = logCall 'enactElect $ \case
    A x -> enactA x
    All x -> enactAll x
    Effect x -> enactElectedEffect x
    OwnerOf handle cont -> enactOwnerOf handle cont
    OpponentOf handle cont -> enactOpponentOf handle cont
    Choice choices -> enactChoice choices


enactChoice :: (HearthMonad k m, PickFrom s) => [Elect k s] -> Hearth k m (SimplePickResult s)
enactChoice = logCall 'enactChoice $ pickFrom >=> enactElect' id


enactOwnerOf :: (HearthMonad k m, PickFrom s) => Handle a -> (Handle Player -> Elect k s) -> Hearth k m (SimplePickResult s)
enactOwnerOf handle cont = logCall 'enactOwnerOf $ do
    ownerOf handle >>= enactElect . cont


enactOpponentOf :: (HearthMonad k m, PickFrom s) => Handle Player -> (Handle Player -> Elect k s) -> Hearth k m (SimplePickResult s)
enactOpponentOf handle cont = logCall 'enactOpponentOf $ do
    opponentOf handle >>= enactElect . cont


enactElectedEffect :: (HearthMonad k m, PickFrom s) => Effect k -> Hearth k m (SimplePickResult s)
enactElectedEffect = logCall 'enactElectedEffect $ enactEffect >=> \case
    NotAvailable -> return NotAvailable
    Available (AtRandomPick ()) -> return $ purePick ()


class EnactElect s where
    enactElect' :: (HearthMonad k m) => (a -> Elect k s) -> Available (PickResult s a) -> Hearth k m (SimplePickResult s)


instance EnactElect AtRandom where
    enactElect' cont = \case
        NotAvailable -> return NotAvailable
        Available (AtRandomPick x) -> enactElect $ cont x


instance EnactElect Targeted where
    enactElect' cont = \case
        NotAvailable -> return NotAvailable
        Available p -> case p of
            AbortTargetedPick -> return $ Available AbortTargetedPick
            TargetedPick x -> enactElect $ cont x


enactA :: (HearthMonad k m, PickFrom s) => A k s -> Hearth k m (SimplePickResult s)
enactA = logCall 'enactA $ \case
    Weapon requirements cont -> enactWeapon requirements cont
    Minion requirements cont -> enactMinion requirements cont
    Player requirements cont -> enactPlayer requirements cont
    Character requirements cont -> enactCharacter requirements cont


enactAll :: (HearthMonad k m, PickFrom s) => All k s -> Hearth k m (SimplePickResult s)
enactAll = logCall 'enactAll . \case
    Minions requirements cont -> enactMinions requirements cont
    Players requirements cont -> enactPlayers requirements cont
    Characters requirements cont -> enactCharacters requirements cont


enactWeapon :: (HearthMonad k m, PickFrom s) => [Requirement Weapon] -> (Handle Weapon -> Elect k s) -> Hearth k m (SimplePickResult s)
enactWeapon requirements cont = logCall 'enactWeapon $ do
    candidates <- viewListOf $ gamePlayers.traversed.playerWeapon.traversed.to _boardWeaponHandle
    restrict requirements candidates >>= pickFrom >>= enactElect' cont


enactMinion :: (HearthMonad k m, PickFrom s) => [Requirement Minion] -> (Handle Minion -> Elect k s) -> Hearth k m (SimplePickResult s)
enactMinion requirements cont = logCall 'enactMinion $ do
    requirements' <- view gameBattlecryMinion >>= return . \case
        Nothing -> requirements
        Just h -> Not h : requirements
    candidates <- viewListOf $ gamePlayers.traversed.playerMinions.traversed.to _boardMinionHandle
    restrict requirements' candidates >>= pickFrom >>= enactElect' cont


enactPlayer :: (HearthMonad k m, PickFrom s) => [Requirement Player] -> (Handle Player -> Elect k s) -> Hearth k m (SimplePickResult s)
enactPlayer requirements cont = logCall 'enactPlayer $ do
    candidates <- getPlayerHandles
    restrict requirements candidates >>= pickFrom >>= enactElect' cont


enactCharacter :: (HearthMonad k m, PickFrom s) => [Requirement Character] -> (Handle Character -> Elect k s) -> Hearth k m (SimplePickResult s)
enactCharacter requirements cont = logCall 'enactCharacter $ do
    requirements' <- view gameBattlecryMinion >>= return . \case
        Nothing -> requirements
        Just h -> Not (MinionCharacter h) : requirements
    playerCandidates <- viewListOf $ gamePlayers.traversed.to characterHandle
    minionCandidates <- viewListOf $ gamePlayers.traversed.playerMinions.traversed.to characterHandle
    let candidates = playerCandidates ++ minionCandidates
    restrict requirements' candidates >>= pickFrom >>= enactElect' cont


enactMinions :: (HearthMonad k m, PickFrom s) => [Requirement Minion] -> (HandleList Minion -> Elect k s) -> Hearth k m (SimplePickResult s)
enactMinions requirements cont = logCall 'enactMinions $ do
    candidates <- viewListOf $ gamePlayers.traversed.playerMinions.traversed.boardMinionHandle
    restrict requirements candidates >>= enactElect . cont . handleList


enactPlayers :: (HearthMonad k m, PickFrom s) => [Requirement Player] -> (HandleList Player -> Elect k s) -> Hearth k m (SimplePickResult s)
enactPlayers requirements cont = logCall 'enactPlayers $ do
    candidates <- getPlayerHandles
    restrict requirements candidates >>= enactElect . cont . handleList


enactCharacters :: (HearthMonad k m, PickFrom s) => [Requirement Character] -> (HandleList Character -> Elect k s) -> Hearth k m (SimplePickResult s)
enactCharacters requirements cont = logCall 'enactCharacters $ do
    playerCandidates <- getPlayerHandles
    minionCandidates <- viewListOf $ gamePlayers.traversed.playerMinions.traversed.boardMinionHandle
    let candidates = map PlayerCharacter playerCandidates ++ map MinionCharacter minionCandidates
    restrict requirements candidates >>= enactElect . cont . handleList


restrict :: (HearthMonad k m) => [Requirement a] -> [Handle a] -> Hearth k m [Handle a]
restrict rs hs = flip filterM hs $ \h -> h `satisfies` rs


fromComparison :: (Ord a) => Comparison -> (a -> a -> Bool)
fromComparison = \case
    Less -> (<)
    LessEqual -> (<=)
    Equal -> (==)
    GreaterEqual -> (>=)
    Greater -> (>)


class CanSatisfy a r | r -> a where
    satisfies :: (HearthMonad k m) => Handle a -> r -> Hearth k m Bool


instance CanSatisfy a (Requirement a) where
    satisfies candidate = \case
        RequireMinion r -> MinionCharacter candidate `satisfies` r
        RequirePlayer r -> PlayerCharacter candidate `satisfies` r
        OwnedBy owner -> liftM (owner ==) $ ownerOf candidate
        Is object -> return $ candidate == object
        Not object -> return $ candidate /= object
        IsDamageSource source -> case source of
            Fatigue -> return False
            DamagingCharacter character -> return $ case castHandle candidate of
                Just candidate' -> candidate' == character
                Nothing -> False
            DamagingSpell spell -> return $ case castHandle candidate of
                Just candidate' -> candidate' == spell
                Nothing ->  False
        WithAttack cmp attackVal -> do
            actualAttack <- dynamic $ viewAttack candidate
            return $ fromComparison cmp actualAttack attackVal
        WithHealth cmp healthVal -> do
            actualHealth <- dynamic $ viewRemainingHealth candidate
            return $ fromComparison cmp actualHealth healthVal
        Damaged -> liftM (> 0) $ dynamic $ viewDamage candidate
        Undamaged -> liftM not $ candidate `satisfies` Damaged
        IsMinion -> return $ case candidate of
            MinionCharacter {} -> True
            _ -> False
        AdjacentTo handle -> areAdjacent handle candidate
        HasMaxManaCrystals -> zoom (getPlayer candidate) $ do
            view playerTotalManaCrystals >>= \case
                MaxManaCrystals -> liftM (== 0) $ view playerTemporaryManaCrystals
                _ -> return False
        HasType minionType -> view $ getMinion candidate.boardMinion.minionTypes.to (Set.member minionType)
        HasCharge -> dynamic $ viewCharge candidate
        HasMinion reqs -> do
            minions <- viewListOf $ getPlayer candidate.playerMinions.traversed.boardMinionHandle
            anyM (`satisfies` reqs) minions


instance CanSatisfy a [Requirement a] where
    satisfies h rs = allM (satisfies h) rs


class Pickable (s :: Selection) k a where
    promptPick :: NonEmpty a -> PromptPick s k a
    pickFailError :: Proxy s -> Proxy k -> Proxy a -> HearthError
    pickGuard :: Proxy s -> Proxy k -> [a] -> a -> Bool


instance Pickable s k (Handle Weapon) where
    promptPick = PickWeapon
    pickFailError _ _ _ = InvalidWeapon
    pickGuard _ _ = flip elem


instance Pickable s k (Handle Minion) where
    promptPick = PickMinion
    pickFailError _ _ _ = InvalidMinion
    pickGuard _ _ = flip elem


instance Pickable s k (Handle Player) where
    promptPick = PickPlayer
    pickFailError _ _ _ = InvalidPlayer
    pickGuard _ _ = flip elem


instance Pickable s k (Handle Character) where
    promptPick = PickCharacter
    pickFailError _ _ _ = InvalidCharacter
    pickGuard _ _ = flip elem


instance Pickable s k (HandCard k) where
    promptPick = PickHandCard
    pickFailError _ _ _ = InvalidHandCard
    pickGuard _ _ cs c = cardName c `elem` map cardName cs


instance Pickable s k (Elect k s) where
    promptPick = PickElect
    pickFailError _ _ _ = InvalidElect
    pickGuard _ _ _ _ = True -- TODO: Make this actually guard


class (EnactElect s, PurePick s) => PickFrom s where
    pickFrom :: forall k m a. (HearthMonad k m, Pickable s k a) => [a] -> Hearth k m (Available (PickResult s a))


instance PickFrom AtRandom where
    pickFrom :: forall k m a. (HearthMonad k m, Pickable AtRandom k a) => [a] -> Hearth k m (Available (PickResult AtRandom a))
    pickFrom = logCall 'pickFrom . \case
        [] -> return NotAvailable
        xs -> liftM Available $ do
            snapshot <- gets GameSnapshot
            guardedPrompt (PromptPickAtRandom snapshot $ promptPick $ NonEmpty.fromList xs) $ \case
                AtRandomPick x -> case pickGuard (Proxy :: Proxy AtRandom) (Proxy :: Proxy k) xs x of
                    True -> return True
                    False -> do
                        prompt $ PromptError $ pickFailError (Proxy :: Proxy AtRandom) (Proxy :: Proxy k) (Proxy :: Proxy a)
                        return False


instance PickFrom Targeted where
    pickFrom :: forall k m a. (HearthMonad k m, Pickable Targeted k a) => [a] -> Hearth k m (Available (PickResult Targeted a))
    pickFrom = logCall 'pickFrom . \case
        [] -> return NotAvailable
        xs -> liftM Available $ do
            snapshot <- gets GameSnapshot
            guardedPrompt (PromptPickTargeted snapshot $ promptPick $ NonEmpty.fromList xs) $ \case
                AbortTargetedPick -> return True
                TargetedPick x -> case pickGuard (Proxy :: Proxy Targeted) (Proxy :: Proxy k) xs x of
                    True -> return True
                    False -> do
                        prompt $ PromptError $ pickFailError (Proxy :: Proxy Targeted) (Proxy :: Proxy k) (Proxy :: Proxy a)
                        return False


playCommon :: (HearthMonad k m) => Handle Player -> HandCard k -> Hearth k m Result
playCommon handle card = logCall 'playCommon $ removeFromHand handle card >>= \case
    False -> return $ Failure "Could not play card because it is not in hand."
    True -> payCost handle $ costOf card


costOf :: HandCard k -> Cost
costOf = \case
    HandCardMinion minion -> minion^.minionCost
    HandCardSpell spell -> spell^.spellCost
    HandCardWeapon weapon -> weapon^.weaponCost


payCost :: (HearthMonad k m) => Handle Player -> Cost -> Hearth k m Result
payCost who = logCall 'payCost $  \case
    ManaCost mana -> payManaCost who mana


payManaCost :: (HearthMonad k m) => Handle Player -> Mana -> Hearth k m Result
payManaCost who (Mana cost) = logCall 'payManaCost $ zoom (getPlayer who) $ do
    totalMana <- view playerTotalManaCrystals
    emptyMana <- view playerEmptyManaCrystals
    let availableMana = totalMana - emptyMana
    case cost <= availableMana of
        False -> return $ Failure "Not enough mana."
        True -> do
            playerEmptyManaCrystals += cost
            return Success


clearDeadMinions :: (HearthMonad k m) => Hearth k m ()
clearDeadMinions = logCall 'clearDeadMinions $ do
    snap <- gets GameSnapshot
    minions <- viewListOf $ gamePlayers.traversed.playerMinions.traversed.boardMinionHandle
    forM_ minions $ \minion -> do
        dead <- isDead $ MinionCharacter minion
        when dead $ do
            prompt $ PromptGameEvent snap $ MinionDied minion
            enactAnyDeathrattles minion
            removeMinion minion


comesAfter :: (Eq a) => a -> a -> [a] -> Bool
comesAfter x y = \case
    v : w : zs -> case x == v && y == w of
        True -> True
        False -> comesAfter x y (w : zs)
    _ -> False


areAdjacent :: (HearthMonad k m) => Handle Minion -> Handle Minion -> Hearth k m Bool
areAdjacent handle1 handle2 = do
    minionsByPlayer <- viewListOf $ gamePlayers.traversed.playerMinions
    return $ flip any minionsByPlayer $ \minions -> let
        handles = map _boardMinionHandle minions
        in comesAfter handle1 handle2 handles || comesAfter handle2 handle1 handles


removeMinion :: (HearthMonad k m) => Handle Minion -> Hearth k m ()
removeMinion minion = do
    owner <- ownerOf minion
    getPlayer owner.playerMinions %= filter (\bm -> bm^.boardMinionHandle /= minion)


removeSpell :: (HearthMonad k m) => Handle Spell -> Hearth k m ()
removeSpell spell = do
    owner <- ownerOf spell
    getPlayer owner.playerSpells %= filter (\s -> s^.castSpellHandle /= spell)


viewAbility :: (HearthMonad k m) => (Ability k Minion -> Bool) -> Handle Minion -> Hearth k m Bool
viewAbility predicate bmHandle = logCall 'viewAbility $ do
    abilities <- viewMinionAbilities bmHandle
    return $ any predicate abilities


viewCan'tAttack :: (HearthMonad k m) => Handle Minion -> Hearth k m Bool
viewCan'tAttack = logCall 'viewCan'tAttack $ viewAbility $ \case
    Can'tAttack -> True
    _ -> False


viewWindfury :: (HearthMonad k m) => Handle Minion -> Hearth k m Bool
viewWindfury = logCall 'viewWindfury $ viewAbility $ \case
    Windfury -> True
    _ -> False


viewTaunt :: (HearthMonad k m) => Handle Minion -> Hearth k m Bool
viewTaunt = logCall 'viewTaunt $ viewAbility $ \case
    Taunt -> True
    _ -> False


viewCharge :: (HearthMonad k m) => Handle Minion -> Hearth k m Bool
viewCharge = logCall 'viewCharge $ viewAbility $ \case
    Charge -> True
    _ -> False


viewHasTauntMinions :: (HearthMonad k m) => Handle Player -> Hearth k m Bool
viewHasTauntMinions player = logCall 'viewHasTauntMinions $ do
    view (getPlayer player.playerMinions) >>= anyM (viewTaunt . _boardMinionHandle)


isAlly :: (Ownable a, HearthMonad k m) => a -> Hearth k m Bool
isAlly bm = do
    owner <- ownerOf bm
    active <- getActivePlayerHandle
    return $ owner == active


isEnemy :: (Ownable a, HearthMonad k m) => a -> Hearth k m Bool
isEnemy = liftM not . isAlly


viewRemainingAttacks :: (CharacterTraits a, HearthMonad k m) => a -> Hearth k m Int
viewRemainingAttacks c = logCall 'viewRemainingAttacks $ liftM2 (\x y -> max 0 $ x - y) (viewMaxAttackCount c) (viewAttackCount c)


isLegalAttackSetup :: (HearthMonad k m) => Handle Character -> Handle Character -> Hearth k m Result
isLegalAttackSetup attacker defender = logCall 'isLegalAttackSetup $ dynamic $ do
    let viewDefenderTaunt = case defender of
            PlayerCharacter _ -> return False
            MinionCharacter m -> viewTaunt m
        viewAttackerCan'tAttack = case attacker of
            PlayerCharacter _ -> return False
            MinionCharacter m -> viewCan'tAttack m
    isAlly attacker >>= \case
        False -> do
            promptGameEvent $ AttackFailed AttackWithEnemy
            return $ Failure "Can't attack with an enemy character."
        True -> do
            viewAttackerCan'tAttack >>= \case
                True -> do
                    promptGameEvent $ AttackFailed AttackerCan'tAttack
                    return $ Failure "Attacker can't attack."
                False -> do
                    attack <- viewAttack attacker
                    case attack <= 0 of
                        True -> do
                            promptGameEvent $ AttackFailed ZeroAttack
                            return $ Failure "Can't attack with a zero attack minion."
                        False -> isEnemy defender >>= \case
                            False -> do
                                promptGameEvent $ AttackFailed DefendWithFriendly
                                return $ Failure "Can't attack into a friendly character."
                            True -> viewSummoningSickness attacker >>= \case
                                True -> do
                                    promptGameEvent $ AttackFailed DoesNotHaveCharge
                                    return $ Failure "Minion needs charge to attack."
                                False -> viewRemainingAttacks attacker >>= \case
                                    0 -> do
                                        promptGameEvent $ AttackFailed OutOfAttacks
                                        return $ Failure "Character is out of attacks."
                                    _ -> viewIsFrozen attacker >>= \case
                                        True -> do
                                            promptGameEvent $ AttackFailed AttackerIsFrozen
                                            return $ Failure "Attacker is frozen."
                                        False -> viewDefenderTaunt >>= \case
                                            True -> return Success
                                            False -> do
                                                defenderController <- ownerOf defender
                                                viewHasTauntMinions defenderController >>= \case
                                                    True -> do
                                                        promptGameEvent $ AttackFailed TauntsExist
                                                        return $ Failure "Must attack a minion with taunt."
                                                    False -> return Success


enactAttack :: (HearthMonad k m) => Handle Character -> Handle Character -> Hearth k m Result
enactAttack attacker defender = logCall 'enactAttack $ do
    isLegalAttackSetup attacker defender >>= \case
        Failure msg -> return $ Failure msg
        Success -> do
            promptGameEvent $ EnactAttack attacker defender
            let source `harms` victim = do
                    dmg <- liftM (Damage . unAttack) $ dynamic $ viewAttack source
                    enactDealDamage (characterHandle victim) dmg (DamagingCharacter source)
            scopedPhase AttackResolutionPhase $ do
                attacker `harms` defender
                defender `harms` attacker
                bumpAttackCount attacker
                return Success


replaceMinionByHandle :: (HearthMonad k m) => BoardMinion k -> Hearth k m ()
replaceMinionByHandle bm' = logCall 'replaceMinionByHandle $ do
    owner <- ownerOf $ bm'^.boardMinionHandle
    getPlayer owner.playerMinions %= \bms -> let
        (front, _ : end) = span (\bm -> bm^.boardMinionHandle /= bm'^.boardMinionHandle) bms
        in front ++ [bm'] ++ end
    

loseDivineShield :: BoardMinion k -> Maybe (BoardMinion k)
loseDivineShield bm = let
    abilities = bm^.boardMinionAbilities
    abilities' = flip filter abilities $ \case
        DivineShield -> False
        _ -> True
    in case on (==) length abilities abilities' of
        True -> Nothing
        False -> Just $ bm & boardMinionAbilities .~ abilities'


viewEventListeners :: (HearthMonad k m) => Hearth k m [EventListener k]
viewEventListeners = logCall 'viewEventListeners $ do
    minions <- viewListOf $ gamePlayers.traversed.playerMinions.traversed.boardMinionHandle
    minionListeners <- liftM concat $ forM minions $ \minion -> do
        abilities <- viewMinionAbilities minion
        return $ flip mapMaybe abilities $ \case
            Whenever listener -> Just $ listener minion
            _ -> Nothing
    effectObservers <- view gameEffectObservers
    return $ effectObservers ++ minionListeners


processEvent :: (HearthMonad k m) => (EventListener k -> Hearth k m ()) -> Hearth k m ()
processEvent f = dynamic viewEventListeners >>= mapM_ f


handleGameEvent :: (HearthMonad k m) => GameEvent k -> Hearth k m ()
handleGameEvent = \case
    PlayedSpell _ spell -> processEvent $ \case
        SpellIsCast listener -> run $ listener spell
        _ -> return ()
    DealtDamage victim damage source -> processEvent $ \case
        DamageIsDealt listener -> run $ listener victim damage source
        _ -> return ()
    HealthRestored recipient health -> processEvent $ \case
        HealthIsRestored listener -> run $ listener recipient health
        _ -> return ()
    PhaseEvent (Begin EndTurnPhase) -> processEvent $ \case
        EndOfTurnEvent listener -> getActivePlayerHandle >>= run . listener
        _ -> return ()
    _ -> return ()
    where
        run cont = scopedPhase TriggeredEffectPhase $ enactElect cont >> return ()







































