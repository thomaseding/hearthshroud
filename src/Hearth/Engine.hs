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
import Hearth.Engine.Data
import Hearth.GameEvent
import Hearth.Model
import Hearth.Prompt
import Hearth.Set.Basic.Names (BasicCardName(TheCoin))


--------------------------------------------------------------------------------


instance (HearthMonad m) => MonadPrompt HearthPrompt (Hearth m) where
    prompt p = do
        result <- lift (prompt p)
        case p of
            PromptGameEvent _ e -> handleGameEvent e
            _ -> return ()
        return result


promptGameEvent :: (HearthMonad m) => GameEvent -> Hearth m ()
promptGameEvent event = do
    snap <- gets GameSnapshot
    prompt $ PromptGameEvent snap $ event


guardedPrompt :: (MonadPrompt p m) => p a -> (a -> m Bool) -> m a
guardedPrompt p predicate = prompt p >>= \x -> predicate x >>= \case
    True -> return x
    False -> guardedPrompt p predicate


isSubsetOf :: (Ord a) => [a] -> [a] -> Bool
isSubsetOf = subset `on` sort


runQuery :: (HearthMonad m) => GameSnapshot -> Hearth m a -> m a
runQuery snapshot query = evalStateT (unHearth query') $ snapshot^.snapshotGameState
    where
        query' = logCall 'runQuery query


runHearth :: (HearthMonad m) => Pair PlayerData -> m GameResult
runHearth = evalStateT (unHearth runHearth') . mkGameState


mkGameState :: Pair PlayerData -> GameState
mkGameState (p1, p2) = let
    ps = [p1, p2]
    in GameState {
        _gameTurn = Turn 1,
        _gameHandleSeed = length ps,
        _gamePlayerTurnOrder = [],
        _gameEffectObservers = [],
        _gamePlayers = zipWith mkPlayer (map (PlayerHandle . RawHandle ()) [0..]) ps }


mkPlayer :: Handle Player -> PlayerData -> Player
mkPlayer handle (PlayerData hero deck) = Player' {
    _playerHandle = handle,
    _playerDeck = deck,
    _playerExcessDrawCount = 0,
    _playerHand = Hand [],
    _playerMinions = [],
    _playerSpells = [],
    _playerEnchantments = [],
    _playerTotalManaCrystals = 0,
    _playerEmptyManaCrystals = 0,
    _playerTemporaryManaCrystals = 0,
    _playerHero = mkBoardHero hero }


mkBoardHero :: Hero -> BoardHero
mkBoardHero hero = BoardHero {
    _boardHeroDamage = 0,
    _boardHeroArmor = 0,
    _boardHeroAttackCount = 0,
    _boardHeroPower = hero^.heroPower,
    _boardHeroPowerCount = 0,
    _boardHero = hero }


getPlayer :: Handle Player -> Lens' GameState Player
getPlayer pHandle = lens getter setter
    where
        getter st = case find (\p -> p^.playerHandle == pHandle) $ st^.gamePlayers of
            Just p -> p
            Nothing -> $logicError 'getPlayer "Non-existent handle."
        setter st p' = st & gamePlayers.traversed %~ \p ->
            case p^.playerHandle == pHandle of
                True -> p'
                False -> p


getMinion :: Handle Minion -> Lens' GameState BoardMinion
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


getSpell :: Handle Spell -> Lens' GameState CastSpell
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


getActivePlayerHandle :: (HearthMonad m) => Hearth m PlayerHandle
getActivePlayerHandle = logCall 'getActivePlayerHandle $ do
    (h : _) <- view gamePlayerTurnOrder
    return h


getNonActivePlayerHandle :: (HearthMonad m) => Hearth m PlayerHandle
getNonActivePlayerHandle = logCall 'getNonActivePlayerHandle $ do
    (_ : h : _) <- view gamePlayerTurnOrder
    return h


isActivePlayer :: (HearthMonad m) => Handle Player -> Hearth m Bool
isActivePlayer h = liftM (h ==) getActivePlayerHandle


class Ownable a where
    ownerOf :: (HearthMonad m) => a -> Hearth m PlayerHandle


instance Ownable (Handle a) where
    ownerOf = logCall 'ownerOf $ \case
        h @ SpellHandle {} -> do
            players <- view gamePlayers
            let isEq spell = spell^.castSpellHandle == h
                players' = flip filter players $ \player -> any isEq $ player^.playerSpells
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


opponentOf :: (HearthMonad m) => Handle Player -> Hearth m (Handle Player)
opponentOf handle = logCall 'opponentOf $ do
    handles <- getPlayerHandles
    case filter (/= handle) handles of
        [opponent] -> return opponent
        _ -> $logicError 'enactOpponentOf "Opponent should exist and be unique."


getPlayerHandles :: (HearthMonad m) => Hearth m [Handle Player]
getPlayerHandles = viewListOf $ gamePlayers.traversed.playerHandle


genRawHandle :: (HearthMonad m) => Hearth m RawHandle
genRawHandle = do
    handle <- liftM (RawHandle ()) $ view gameHandleSeed
    gameHandleSeed += 1
    return handle


class GenHandle handle where
    genHandle :: (HearthMonad m) => Hearth m handle


instance GenHandle MinionHandle where
    genHandle = liftM MinionHandle genRawHandle


instance GenHandle SpellHandle where
    genHandle = liftM SpellHandle genRawHandle


runHearth' :: (HearthMonad m) => Hearth m GameResult
runHearth' = logCall 'runHearth' $ do
    promptGameEvent GameBegins
    initGame
    tickTurn
    let gameResult = GameResult {}
    promptGameEvent $ GameEnds gameResult
    return gameResult


initGame :: (HearthMonad m) => Hearth m ()
initGame = logCall 'initGame $ do
    flipCoin
    handles <- getPlayerHandles
    mapM_ initPlayer handles


flipCoin :: (HearthMonad m) => Hearth m ()
flipCoin = logCall 'flipCoin $ getPlayerHandles >>= \handles -> do
    snap <- gets GameSnapshot
    AtRandomPick handle <- prompt $ PromptPickAtRandom snap $ PickPlayer $ NonEmpty.fromList handles
    let handles' = dropWhile (/= handle) $ cycle handles
    gamePlayerTurnOrder .= handles'


initPlayer :: (HearthMonad m) => Handle Player -> Hearth m ()
initPlayer = initHand


initHand :: (HearthMonad m) => Handle Player -> Hearth m ()
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


drawCards :: (HearthMonad m) => Handle Player -> Int -> Hearth m [HandCard]
drawCards handle = logCall 'drawCards $ liftM catMaybes . flip replicateM (drawCard handle)


putInHand :: (HearthMonad m) => Handle Player -> HandCard -> Hearth m Bool
putInHand handle card = logCall 'putInHand $ zoom (getPlayer handle.playerHand.handCards) $ do
    to length >>=. \case
        MaxHandSize -> return False
        _ -> do
            id %= (card :)
            return True


removeFromHand :: (HearthMonad m) => Handle Player -> HandCard -> Hearth m Bool
removeFromHand handle card = logCall 'removeFromHand $ zoom (getPlayer handle.playerHand.handCards) $ do
    hand <- view id
    id %= deleteBy (on (==) cardName) card
    hand' <- view id
    return $ length hand /= length hand'


drawCard :: (HearthMonad m) => Handle Player -> Hearth m (Maybe HandCard)
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


isDead :: (HearthMonad m) => Handle Character -> Hearth m Bool
isDead character = logCall 'isDead $ isMortallyWounded character >>= \case
    True -> return True
    False -> case character of
        PlayerCharacter {} -> return False
        MinionCharacter minion -> view $ getMinion minion.boardMinionPendingDestroy


isMortallyWounded :: (HearthMonad m) => Handle Character -> Hearth m Bool
isMortallyWounded = logCall 'isMortallyWounded $ liftM (<= 0) . dynamic . viewRemainingHealth


shuffleDeck :: (HearthMonad m) => Handle Player -> Hearth m ()
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


isGameOver :: (HearthMonad m) => Hearth m Bool
isGameOver = logCall 'isGameOver $ do
    handles <- getPlayerHandles
    anyM (isDead . PlayerCharacter) handles


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


gainManaCrystals :: (HearthMonad m) => Handle Player -> Int -> CrystalState -> Hearth m ()
gainManaCrystals handle amount crystalState = logCall 'gainManaCrystals $ do
    replicateM_ amount $ gainManaCrystal handle crystalState


gainManaCrystal :: (HearthMonad m) => Handle Player -> CrystalState -> Hearth m ()
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


scopedPhase :: (HearthMonad m) => Phase -> Hearth m a -> Hearth m a
scopedPhase phase action = logCall 'scopedPhase $ do
    promptGameEvent $ PhaseEvent $ Begin phase
    x <- action
    promptGameEvent $ PhaseEvent $ End phase
    return x


tickTimePointEnchantments :: (HearthMonad m) => TimePoint -> Hearth m ()
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

        generalConsume :: (HearthMonad m) => AnyEnchantment a -> Hearth m (Maybe (AnyEnchantment a))
        generalConsume = \case
            Continuous e -> liftM (liftM Continuous) $ continuousConsume e
            Limited e -> liftM (liftM Limited) $ limitedConsume e

        continuousConsume :: (HearthMonad m) => Enchantment Continuous a -> Hearth m (Maybe (Enchantment Continuous a))
        continuousConsume = return . Just

        limitedConsume :: (HearthMonad m) => Enchantment Limited a -> Hearth m (Maybe (Enchantment Limited a))
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


beginTurn :: (HearthMonad m) => Hearth m ()
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


endTurn :: (HearthMonad m) => Hearth m ()
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


pumpTurn :: (HearthMonad m) => Hearth m ()
pumpTurn = logCall 'pumpTurn $ do
    let cond = \case
            Nothing -> True
            Just evo -> case evo of
                Right EndTurn -> True
                Left () -> False
    _ <- iterateUntil cond pumpTurn'
    return ()


pumpTurn' :: (HearthMonad m) => Hearth m (Maybe (Either () EndTurn))
pumpTurn' = logCall 'pumpTurn' $ isGameOver >>= \case
    True -> return Nothing
    False -> liftM Just performAction


performAction :: (HearthMonad m) => Hearth m (Either () EndTurn)
performAction = logCall 'performAction $ do
    snapshot <- gets GameSnapshot
    evolution <- prompt (PromptAction snapshot) >>= enactAction
    clearDeadMinions
    return $ case evolution of
        Left _ -> Left ()
        Right EndTurn -> Right EndTurn


enactAction :: (HearthMonad m) => Action -> Hearth m (Either Result EndTurn)
enactAction = logCall 'enactAction . \case
    ActionPlayerConceded p -> concede p >> return (Left Success)
    ActionPlayMinion card pos -> liftM Left $ actionPlayMinion card pos
    ActionPlaySpell card -> liftM Left $ actionPlaySpell card
    ActionAttack attacker defender -> liftM Left $ actionAttack attacker defender
    ActionEndTurn -> return $ Right EndTurn
    ActionHeroPower -> liftM Left actionHeroPower


actionHeroPower :: (HearthMonad m) => Hearth m Result
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


concede :: (HearthMonad m) => Handle Player -> Hearth m ()
concede p = logCall 'concede $ do
    health <- dynamic $ viewMaxHealth p
    getPlayer p.playerHero.boardHeroDamage .= Damage (unHealth health)


isCardInHand :: (HearthMonad m) => Handle Player -> HandCard -> Hearth m Bool
isCardInHand handle card = logCall 'isCardInHand $ local id $ removeFromHand handle card


insertAt :: Int -> a -> [a] -> [a]
insertAt n x xs = let
    (left, right) = splitAt n xs
    in left ++ [x] ++ right


toBoardMinion :: Handle Minion -> Minion -> BoardMinion
toBoardMinion handle minion = BoardMinion {
    _boardMinionDamage = 0,
    _boardMinionEnchantments = [],
    _boardMinionAbilities = minion^.minionAbilities,
    _boardMinionAttackCount = 0,
    _boardMinionNewlySummoned = True,
    _boardMinionPendingDestroy = False,
    _boardMinionHandle = handle,
    _boardMinion = minion }


summon :: (HearthMonad m) => Handle Player -> Minion -> BoardIndex -> Hearth m (Either String MinionHandle)
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


actionAttack :: (HearthMonad m) => Handle Character -> Handle Character -> Hearth m Result
actionAttack attacker defender = logCall 'actionAttack $ do
    enactAttack attacker defender


actionPlayMinion :: (HearthMonad m) => HandCard -> BoardIndex -> Hearth m Result
actionPlayMinion card idx = logCall 'actionPlayMinion $ do
    pHandle <- getActivePlayerHandle
    playMinion pHandle card idx


actionPlaySpell :: (HearthMonad m) => HandCard -> Hearth m Result
actionPlaySpell card = logCall 'actionPlaySpell $ do
    handle <- getActivePlayerHandle
    playSpell handle card


playMinion :: (HearthMonad m) => Handle Player -> HandCard -> BoardIndex -> Hearth m Result
playMinion pHandle card idx = logCall 'playMinion $ do
    st <- get
    playMinion' pHandle card idx >>= \case
        Left msg -> do
            put st
            return $ Failure msg
        Right bmHandle -> do
            promptGameEvent $ PlayedMinion pHandle bmHandle
            result <- enactAnyBattleCries bmHandle
            when (result /= Success) $ put st
            return result


playMinion' :: (HearthMonad m) => Handle Player -> HandCard -> BoardIndex -> Hearth m (Either String MinionHandle)
playMinion' player card idx = logCall 'playMinion' $ playCommon player card >>= \case
    Failure msg -> return $ Left msg
    Success -> case card of
        HandCardMinion minion -> summon player minion idx
        _ -> return $ Left "Must pick a minion to play a minion."


playSpell :: (HearthMonad m) => Handle Player -> HandCard -> Hearth m Result
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


playSpell' :: (HearthMonad m) => Handle Player -> HandCard -> Hearth m (Either String SpellHandle)
playSpell' pHandle card = logCall 'playSpell' $ playCommon pHandle card >>= \case
    Failure msg -> return $ Left msg
    Success -> case card of
        HandCardSpell spell -> do
            sHandle <- genHandle
            let spell' = CastSpell { _castSpellHandle = sHandle, _castSpell = spell }
            getPlayer pHandle.playerSpells %= (spell' :)
            return $ Right sHandle
        _ -> return $ Left "Must pick a spell to play a spell."


enactSpell :: (HearthMonad m) => Handle Spell -> Hearth m Result
enactSpell spell = logCall 'enactSpell $ scopedPhase SpellPhase $ do
    st <- get
    let abort msg = put st >> return (Failure msg)
    cont <- view $ getSpell spell.castSpell.spellEffect
    enactElect (cont spell) >>= \case
        Available (TargetedPick _) -> return Success
        Available AbortTargetedPick -> abort "Targeted pick aborted."
        NotAvailable -> abort "Not available."


enactAnyDeathrattles :: (HearthMonad m) => Handle Minion -> Hearth m ()
enactAnyDeathrattles bmHandle = logCall 'enactAnyDeathrattles $ do
    abilities <- dynamic $ viewMinionAbilities bmHandle
    forM_ abilities $ \case
        Deathrattle elect -> enactDeathrattle bmHandle elect
        _ -> return ()


enactAnyBattleCries :: (HearthMonad m) => Handle Minion -> Hearth m Result
enactAnyBattleCries bmHandle = logCall 'enactAnyBattleCries $ do
    st <- get
    abilities <- dynamic $ viewMinionAbilities bmHandle
    result <- liftM condensePickResults $ forM abilities $ \case
        Battlecry effect -> enactBattlecry bmHandle effect
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


enactDeathrattle :: (HearthMonad m) => Handle Minion -> (Handle Minion -> Elect AtRandom) -> Hearth m ()
enactDeathrattle handle cont = logCall 'enactDeathrattle $ scopedPhase DeathrattlePhase $ do
    _ <- enactElect $ cont handle
    return ()


enactBattlecry :: (HearthMonad m) => Handle Minion -> (Handle Minion -> Elect Targeted) -> Hearth m (SimplePickResult Targeted)
enactBattlecry handle cont = logCall 'enactBattlecry $ scopedPhase BattlecryPhase $ do
    enactElect $ cont handle


whenM :: (Monad m) => m Bool -> m () -> m ()
whenM bool action = bool >>= flip when action


enactEffect :: (HearthMonad m) => Effect -> Hearth m (SimplePickResult AtRandom)
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
    RestoreHealth handle amount -> restoreHealth handle amount >> return success
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


enactTakeControl :: (HearthMonad m) => Handle Player -> Handle Minion -> Hearth m ()
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


enactDiscardAtRandom :: (HearthMonad m) => Handle Player -> Hearth m ()
enactDiscardAtRandom player = logCall 'enactDiscardAtRandom $ do
    Hand cards <- view $ getPlayer player.playerHand
    pickFrom cards >>= \case
        NotAvailable -> return ()
        Available (AtRandomPick card) -> discardCard player card


discardCard :: (HearthMonad m) => Handle Player -> HandCard -> Hearth m ()
discardCard player candidate = logCall 'discardCard $ do
    Hand cards <- view $ getPlayer player.playerHand
    let cards' = deleteBy (on (==) cardName) candidate cards
    getPlayer player.playerHand .= Hand cards'


getMinionCount :: (HearthMonad m) => Handle Player -> Hearth m Int
getMinionCount player = logCall 'getMinionCount $ do
    view $ getPlayer player.playerMinions.to length


boardIndexOf :: (HearthMonad m) => Handle Minion -> Hearth m BoardIndex
boardIndexOf minion = logCall 'boardIndexOf $ do
    owner <- ownerOf minion
    minions <- viewListOf $ getPlayer owner.playerMinions.traversed.boardMinionHandle
    case findIndex (== minion) minions of
        Just idx -> return $ BoardIndex idx
        Nothing -> $logicError 'boardIndexOf "xxx"


enactSummon :: (HearthMonad m) => Handle Player -> Minion -> BoardLocation -> Hearth m ()
enactSummon player minion loc = do
    idx <- case loc of
        RightOf m -> boardIndexOf m >>= return . \case
            BoardIndex n -> BoardIndex $ n + 1
        Rightmost -> liftM BoardIndex $ getMinionCount player
    _ <- summon player minion idx
    return ()


enactPutInHand :: (HearthMonad m) => Handle Player -> Card -> Hearth m ()
enactPutInHand player card = do
    _ <- putInHand player $ toHandCard card
    return ()


enactObserving :: (HearthMonad m) => Effect -> EventListener -> Hearth m (SimplePickResult AtRandom)
enactObserving effect listener = logCall 'enactObserving $ do
    originalObservers <- view gameEffectObservers
    gameEffectObservers %= (listener :)
    result <- enactEffect effect
    gameEffectObservers .= originalObservers
    return result


enactIf :: (HearthMonad m) => Condition -> Effect -> Effect -> Hearth m (SimplePickResult AtRandom)
enactIf cond true false = logCall 'enactIf $ enactCondition cond >>= enactEffect . \case
    True -> true
    False -> false


enactCondition :: (HearthMonad m) => Condition -> Hearth m Bool
enactCondition = logCall 'enactCondition $ \case
    Or x y -> enactOr x y
    And x y -> enactAnd x y
    Satisfies handle requirements -> satisfies handle requirements


enactOr :: (HearthMonad m) => Condition -> Condition -> Hearth m Bool
enactOr x y = enactCondition x >>= \case
    True -> return True
    False -> enactCondition y


enactAnd :: (HearthMonad m) => Condition -> Condition -> Hearth m Bool
enactAnd x y = enactCondition x >>= \case
    False -> return False
    True -> enactCondition y


freeze :: (HearthMonad m) => Handle Character -> Hearth m ()
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


enactWhen :: (HearthMonad m) => Handle a -> [Requirement a] -> Effect -> Hearth m ()
enactWhen handle requirements effect = do
    whenM (satisfies handle requirements) $ do
        _ <- enactEffect effect
        return ()


transform :: (HearthMonad m) => Handle Minion -> Minion -> Hearth m ()
transform handle newMinion = logCall 'transform $ do
    getMinion handle .= toBoardMinion handle newMinion
    promptGameEvent $ Transformed handle newMinion


enactEffectElect :: (HearthMonad m) => Elect AtRandom -> Hearth m (SimplePickResult AtRandom)
enactEffectElect elect = enactElect elect >>= return . \case
    NotAvailable -> NotAvailable
    Available (AtRandomPick _) -> purePick ()


gainArmor :: (HearthMonad m) => Handle Player -> Armor -> Hearth m ()
gainArmor handle armor = logCall 'gainArmor $ do
    getPlayer handle.playerHero.boardHeroArmor += armor
    promptGameEvent $ GainedArmor handle armor


restoreHealth :: (HearthMonad m) => Handle Character -> Health -> Hearth m ()
restoreHealth charHandle (Health amount) = logCall 'restoreHealth $ do
    actualAmount <- case charHandle of
        MinionCharacter handle -> zoom (getMinion handle.boardMinionDamage) restoreM
        PlayerCharacter handle -> zoom (getPlayer handle.playerHero.boardHeroDamage) restoreM
    case actualAmount of
        0 -> return ()
        _ -> promptGameEvent $ HealthRestored charHandle (Health actualAmount)
    where
        restore = max 0 . (subtract $ Damage amount)
        restoreM = do
            before <- view id
            after <- id <%= restore
            return $ unDamage $ before - after


enactDestroyMinion :: (HearthMonad m) => Handle Minion -> Hearth m ()
enactDestroyMinion handle = logCall 'enactDestroyMinion $ do
    getMinion handle.boardMinionPendingDestroy .= True
    promptGameEvent $ MinionDestroyed handle


enactForEach :: (HearthMonad m) => HandleList a -> (Handle a -> Effect) -> Hearth m (SimplePickResult AtRandom)
enactForEach (HandleList _ handles) cont = logCall 'enactForEach $ do
    liftM condensePickResults $ forM handles (enactEffect . cont)


sequenceEffects :: (HearthMonad m) => [Effect] -> Hearth m (SimplePickResult AtRandom)
sequenceEffects effects = liftM condensePickResults $ mapM enactEffect effects


condensePickResults :: (PurePick s) => [SimplePickResult s] -> SimplePickResult s
condensePickResults results = case dropWhile (== purePick ()) results of
    [] -> purePick ()
    r : _ -> r


enactEnchant :: (HearthMonad m) => Handle a -> AnyEnchantment a -> Hearth m ()
enactEnchant = logCall 'enactEnchant $ \case
    SpellHandle _ -> const $ return ()
    h @ PlayerHandle{} -> enactEnchantPlayer h
    h @ MinionHandle{} -> enactEnchantMinion h
    h @ PlayerCharacter{} -> enactEnchantCharacter h
    h @ MinionCharacter{} -> enactEnchantCharacter h


enactEnchantPlayer :: (HearthMonad m) => Handle Player -> AnyEnchantment Player -> Hearth m ()
enactEnchantPlayer handle enchantment = logCall 'enactEnchantPlayer $ do
    getPlayer handle.playerEnchantments %= (++ [enchantment])


enactEnchantMinion :: (HearthMonad m) => Handle Minion -> AnyEnchantment Minion -> Hearth m ()
enactEnchantMinion handle enchantment = logCall 'enactEnchantMinion $ do
    getMinion handle.boardMinionEnchantments %= (++ [enchantment])


enactEnchantCharacter :: (HearthMonad m) => Handle Character -> AnyEnchantment Character -> Hearth m ()
enactEnchantCharacter character enchantment = logCall 'enactEnchantCharacter $ case character of
    PlayerCharacter player -> enactEnchantPlayer player $ liftE PlayerEnchantment enchantment
    MinionCharacter player -> enactEnchantMinion player $ liftE MinionEnchantment enchantment
    where
        liftE :: forall a. (forall t. Enchantment t Character -> Enchantment t a) -> AnyEnchantment Character -> AnyEnchantment a
        liftE f = \case
            Limited e -> Limited $ f e
            Continuous e -> Continuous $ f e


viewIsDamaged :: BoardMinion -> Bool
viewIsDamaged bm = bm^.boardMinionDamage > 0


viewMinionAbilities :: (HearthMonad m) => Handle Minion -> Hearth m [Ability]
viewMinionAbilities bmHandle = logCall 'viewMinionAbilities $ do
    bm <- view $ getMinion bmHandle
    return $ bm^.boardMinionAbilities >>= \ability -> case ability of
        Enrage abilities _ -> case viewIsDamaged bm of
            True -> ability : abilities  -- TODO: Need to check correct interleaving.
            False -> [ability]
        _ -> [ability]


viewMinionEnchantments :: (HearthMonad m) => Handle Minion -> Hearth m [AnyEnchantment Minion]
viewMinionEnchantments bmHandle = logCall 'viewMinionEnchantments $ do
    bm <- view $ getMinion bmHandle
    let baseEnchantments = bm^.boardMinionEnchantments
        enrageEnchantments = case viewIsDamaged bm of
            False -> []
            True -> bm^.boardMinionAbilities >>= \case
                Enrage _ es -> es
                _ -> []
    return $ baseEnchantments ++ map Continuous enrageEnchantments -- TODO: Need to check correct interleaving.


viewPlayerEnchantments :: (HearthMonad m) => Handle Player -> Hearth m [AnyEnchantment Player]
viewPlayerEnchantments player = logCall 'viewPlayerEnchantments $ do
    view $ getPlayer player.playerEnchantments


underAnyEnchantment :: forall a b. (forall t. Enchantment t a -> b) -> AnyEnchantment a -> b
underAnyEnchantment f = \case
    Continuous e -> f e
    Limited e -> f e


class SatisfiesEnchantment a where
    viewSatisfiesEnchantment :: (HearthMonad m) => (forall t. Enchantment t a -> Bool) -> Handle a -> Hearth m Bool


instance SatisfiesEnchantment Minion where
    viewSatisfiesEnchantment p minion = logCall 'viewSatisfiesEnchantment $ do
        enchantments <- viewMinionEnchantments minion
        return $ any (underAnyEnchantment p) enchantments


instance SatisfiesEnchantment Player where
    viewSatisfiesEnchantment p player = logCall 'viewSatisfiesEnchantment $ do
        enchantments <- viewPlayerEnchantments player
        return $ any (underAnyEnchantment p) enchantments


discoverEnchantment :: (forall t2 a2. Enchantment t2 a2 -> Bool) -> Enchantment t a -> Bool
discoverEnchantment p = \case
    Until _ e -> discoverEnchantment p e
    MinionEnchantment e -> discoverEnchantment p e
    PlayerEnchantment e -> discoverEnchantment p e
    e -> p e


grantsFrozen :: Enchantment t a -> Bool
grantsFrozen = \case
    MinionEnchantment e -> grantsFrozen e
    PlayerEnchantment e -> grantsFrozen e
    Until _ e -> grantsFrozen e
    Frozen -> True
    _ -> False


class IsCharacterHandle a where
    characterHandle :: a -> Handle Character


instance IsCharacterHandle PlayerHandle where
    characterHandle = PlayerCharacter


instance IsCharacterHandle MinionHandle where
    characterHandle = MinionCharacter


instance IsCharacterHandle CharacterHandle where
    characterHandle = id


instance IsCharacterHandle BoardMinion where
    characterHandle = characterHandle . _boardMinionHandle


-- TODO: Make CharacterTraits not a class and just make it a constraint over a bunch of modular classes
class (Ownable h, IsCharacterHandle h) => CharacterTraits h where
    bumpAttackCount :: (HearthMonad m) => h -> Hearth m ()
    viewAttackCount :: (HearthMonad m) => h -> Hearth m Int
    viewMaxAttackCount :: (HearthMonad m) => h -> Hearth m Int
    viewDamage :: (HearthMonad m) => h -> Hearth m Damage
    viewAttack :: (HearthMonad m) => h -> Hearth m Attack
    viewMaxHealth :: (HearthMonad m) => h -> Hearth m Health
    viewSummoningSickness :: (HearthMonad m) => h -> Hearth m Bool
    viewIsFrozen :: (HearthMonad m) => h -> Hearth m Bool


instance CharacterTraits PlayerHandle where
    bumpAttackCount pHandle = logCall 'bumpAttackCount $ do
        getPlayer pHandle.playerHero.boardHeroAttackCount += 1
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


auraAbilitiesOf :: [Ability] -> [Handle Minion -> Aura]
auraAbilitiesOf = mapMaybe $ \case
    Aura aura -> Just aura
    _ -> Nothing


dynamic :: (HearthMonad m) => Hearth m a -> Hearth m a
dynamic action = logCall 'dynamic $ local id $ do
    -- TODO: This function should not be use recursively. There should be a $logicError guard.
    enactMinionAuras
    applyMinionEnchantments
    applyPlayerEnchantments
    action
    where
        getAllMinions = viewListOf $ gamePlayers.traversed.playerMinions.traversed.boardMinionHandle
        enactMinionAuras = do
            minions <- getAllMinions
            forM_ minions $ \minion -> do
                auras <- liftM auraAbilitiesOf $ viewMinionAbilities minion
                forM_ auras $ enactAura . ($ minion)
        applyMinionEnchantments = do
            minions <- getAllMinions
            forM_ minions $ \minion -> do
                enchantments <- viewMinionEnchantments minion
                forM_ enchantments $ underAnyEnchantment $ applyMinionEnchantment minion
        applyPlayerEnchantments = do
            players <- viewListOf $ gamePlayers.traversed.playerHandle
            forM_ players $ \player -> do
                enchantments <- viewPlayerEnchantments player
                forM_ enchantments $ underAnyEnchantment $ applyPlayerEnchantment player
        applyPlayerEnchantment :: (HearthMonad m) => Handle Player -> Enchantment t Player -> Hearth m ()
        applyPlayerEnchantment player = \case
            PlayerEnchantment e -> applyCharacterEnchantment (PlayerCharacter player) e
            Until _ enchantment -> applyPlayerEnchantment player enchantment
        applyMinionEnchantment :: (HearthMonad m) => Handle Minion -> Enchantment t Minion -> Hearth m ()
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
        applyCharacterEnchantment :: (HearthMonad m) => Handle Character -> Enchantment t Character -> Hearth m ()
        applyCharacterEnchantment character = \case
            Until _ enchantment -> applyCharacterEnchantment character enchantment
            Frozen -> return ()
            StatsDelta a h -> case character of
                PlayerCharacter player -> do
                    getPlayer player.playerHero.boardHero.heroAttack += a
                    getPlayer player.playerHero.boardHero.heroHealth += h
                MinionCharacter minion -> do
                    getMinion minion.boardMinion.minionAttack += a
                    getMinion minion.boardMinion.minionHealth += h


enactAura :: (HearthMonad m) => Aura -> Hearth m ()
enactAura = logCall 'enactAura $ \case
    AuraOwnerOf handle cont -> ownerOf handle >>= enactAura . cont
    AuraOpponentOf handle cont -> opponentOf handle >>= enactAura . cont
    While handle requirements aura -> enactWhile handle requirements aura
    EachMinion requirements cont -> enactEachMinion requirements cont
    Has handle enchantment -> enactHas handle enchantment
    HasAbility handle ability -> enactHasAbility handle ability


enactEachMinion :: (HearthMonad m) => [Requirement Minion] -> (Handle Minion -> Aura) -> Hearth m ()
enactEachMinion requirements cont = logCall 'enactEachMinion $ do
    minions <- viewListOf $ gamePlayers.traversed.playerMinions.traversed.boardMinionHandle
    forM_ minions $ \minion -> do
        whenM (satisfies minion requirements) $ enactAura $ cont minion


enactWhile :: (HearthMonad m) => Handle a -> [Requirement a] -> Aura -> Hearth m ()
enactWhile handle requirements = logCall 'enactWhile $ whenM (satisfies handle requirements) . enactAura


enactHas :: (HearthMonad m) => Handle Minion -> Enchantment Continuous Minion -> Hearth m ()
enactHas handle enchantment = logCall 'enactHas $ do
    getMinion handle.boardMinionEnchantments %= (++ [Continuous enchantment])


enactHasAbility :: (HearthMonad m) => Handle Minion -> Ability -> Hearth m ()
enactHasAbility handle ability = logCall 'enactHasAbility $ do
    getMinion handle.boardMinionAbilities %= (++ [ability])


instance CharacterTraits MinionHandle where
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


instance CharacterTraits CharacterHandle where
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


viewRemainingHealth :: (HearthMonad m) => Handle Character -> Hearth m Health
viewRemainingHealth h = logCall 'viewRemainingHealth $ do
    damage <- viewDamage h
    health <- viewMaxHealth h
    return $ health - Health (unDamage damage)


viewSpellDamage :: (HearthMonad m) => Handle Player -> Hearth m Int
viewSpellDamage player = logCall 'viewSpellDamage $ do
    minions <- viewListOf $ getPlayer player.playerMinions.traversed.boardMinionHandle
    liftM sum $ forM minions $ \minion -> do
        abilities <- viewMinionAbilities minion
        return $ sum $ flip mapMaybe abilities $ \case
            SpellDamage n -> Just n
            _ -> Nothing


enactRandomMissiles :: (HearthMonad m) => [Requirement Character] -> Int -> Handle Spell -> Hearth m ()
enactRandomMissiles reqs n spell = logCall 'enactRandomMissiles $ do
    modifier <- ownerOf spell >>= dynamic . viewSpellDamage
    enactRandomMissilesPrim reqs (n + modifier) spell


enactRandomMissilesPrim :: (HearthMonad m) => [Requirement Character] -> Int -> Handle Spell -> Hearth m ()
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


enactDealDamage :: (HearthMonad m) => Handle Character -> Damage -> DamageSource -> Hearth m ()
enactDealDamage charHandle (Damage baseDamage) source = logCall 'enactDealDamage $ do
    modifier <- case source of
        DamagingSpell spell -> ownerOf spell >>= dynamic . viewSpellDamage
        _ -> return 0
    let damage = Damage $ baseDamage + modifier
    dealDamagePrim charHandle damage source


dealDamagePrim :: (HearthMonad m) => Handle Character -> Damage -> DamageSource -> Hearth m ()
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


silence :: (HearthMonad m) => Handle Minion -> Hearth m ()
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


enactElect :: (HearthMonad m, PickFrom s) => Elect s -> Hearth m (SimplePickResult s)
enactElect = logCall 'enactElect $ \case
    A x -> enactA x
    All x -> enactAll x
    Effect x -> enactElectedEffect x
    OwnerOf handle cont -> enactOwnerOf handle cont
    OpponentOf handle cont -> enactOpponentOf handle cont
    Choice choices -> enactChoice choices


enactChoice :: (HearthMonad m, PickFrom s) => [Elect s] -> Hearth m (SimplePickResult s)
enactChoice = logCall 'enactChoice $ pickFrom >=> enactElect' id


enactOwnerOf :: (HearthMonad m, PickFrom s) => Handle a -> (Handle Player -> Elect s) -> Hearth m (SimplePickResult s)
enactOwnerOf handle cont = logCall 'enactOwnerOf $ do
    ownerOf handle >>= enactElect . cont


enactOpponentOf :: (HearthMonad m, PickFrom s) => Handle Player -> (Handle Player -> Elect s) -> Hearth m (SimplePickResult s)
enactOpponentOf handle cont = logCall 'enactOpponentOf $ do
    opponentOf handle >>= enactElect . cont


enactElectedEffect :: (HearthMonad m, PickFrom s) => Effect -> Hearth m (SimplePickResult s)
enactElectedEffect = logCall 'enactElectedEffect $ enactEffect >=> \case
    NotAvailable -> return NotAvailable
    Available (AtRandomPick ()) -> return $ purePick ()


class EnactElect s where
    enactElect' :: (HearthMonad m) => (a -> Elect s) -> Available (PickResult s a) -> Hearth m (SimplePickResult s)


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


enactA :: (HearthMonad m, PickFrom s) => A s -> Hearth m (SimplePickResult s)
enactA = logCall 'enactA $ \case
    Minion requirements cont -> enactMinion requirements cont
    Player requirements cont -> enactPlayer requirements cont
    Character requirements cont -> enactCharacter requirements cont


enactAll :: (HearthMonad m, PickFrom s) => All s -> Hearth m (SimplePickResult s)
enactAll = logCall 'enactAll . \case
    Minions requirements cont -> enactMinions requirements cont
    Players requirements cont -> enactPlayers requirements cont
    Characters requirements cont -> enactCharacters requirements cont


enactMinion :: (HearthMonad m, PickFrom s) => [Requirement Minion] -> (Handle Minion -> Elect s) -> Hearth m (SimplePickResult s)
enactMinion requirements cont = logCall 'enactMinion $ do
    pHandles <- getPlayerHandles
    candidates <- liftM concat $ forM pHandles $ \pHandle -> do
        bms <- view $ getPlayer pHandle.playerMinions
        return $ map (\bm -> bm^.boardMinionHandle) bms
    restrict requirements candidates >>= pickFrom >>= enactElect' cont


enactPlayer :: (HearthMonad m, PickFrom s) => [Requirement Player] -> (Handle Player -> Elect s) -> Hearth m (SimplePickResult s)
enactPlayer requirements cont = logCall 'enactPlayer $ do
    candidates <- getPlayerHandles
    restrict requirements candidates >>= pickFrom >>= enactElect' cont


enactCharacter :: (HearthMonad m, PickFrom s) => [Requirement Character] -> (Handle Character -> Elect s) -> Hearth m (SimplePickResult s)
enactCharacter requirements cont = logCall 'enactCharacter $ do
    pHandles <- getPlayerHandles
    minionCandidates <- liftM concat $ forM pHandles $ \pHandle -> do
        bms <- view $ getPlayer pHandle.playerMinions
        return $ map (\bm -> MinionCharacter $ bm^.boardMinionHandle) bms
    let playerCandidates = map PlayerCharacter pHandles
        candidates = playerCandidates ++ minionCandidates
    restrict requirements candidates >>= pickFrom >>= enactElect' cont


enactMinions :: (HearthMonad m, PickFrom s) => [Requirement Minion] -> (HandleList Minion -> Elect s) -> Hearth m (SimplePickResult s)
enactMinions requirements cont = logCall 'enactMinions $ do
    candidates <- viewListOf $ gamePlayers.traversed.playerMinions.traversed.boardMinionHandle
    restrict requirements candidates >>= enactElect . cont . handleList


enactPlayers :: (HearthMonad m, PickFrom s) => [Requirement Player] -> (HandleList Player -> Elect s) -> Hearth m (SimplePickResult s)
enactPlayers requirements cont = logCall 'enactPlayers $ do
    candidates <- getPlayerHandles
    restrict requirements candidates >>= enactElect . cont . handleList


enactCharacters :: (HearthMonad m, PickFrom s) => [Requirement Character] -> (HandleList Character -> Elect s) -> Hearth m (SimplePickResult s)
enactCharacters requirements cont = logCall 'enactCharacters $ do
    playerCandidates <- getPlayerHandles
    minionCandidates <- viewListOf $ gamePlayers.traversed.playerMinions.traversed.boardMinionHandle
    let candidates = map PlayerCharacter playerCandidates ++ map MinionCharacter minionCandidates
    restrict requirements candidates >>= enactElect . cont . handleList


restrict :: (HearthMonad m) => [Requirement a] -> [Handle a] -> Hearth m [Handle a]
restrict rs hs = flip filterM hs $ \h -> h `satisfies` rs


fromComparison :: (Ord a) => Comparison -> (a -> a -> Bool)
fromComparison = \case
    Less -> (<)
    LessEqual -> (<=)
    Equal -> (==)
    GreaterEqual -> (>=)
    Greater -> (>)


class CanSatisfy a r | r -> a where
    satisfies :: (HearthMonad m) => Handle a -> r -> Hearth m Bool


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
        HasMinion reqs -> do
            minions <- viewListOf $ getPlayer candidate.playerMinions.traversed.boardMinionHandle
            anyM (`satisfies` reqs) minions


instance CanSatisfy a [Requirement a] where
    satisfies h rs = allM (satisfies h) rs


class Pickable (s :: Selection) a where
    promptPick :: NonEmpty a -> PromptPick s a
    pickFailError :: Proxy s -> Proxy a -> HearthError
    pickGuard :: Proxy s -> [a] -> a -> Bool


instance Pickable s MinionHandle where
    promptPick = PickMinion
    pickFailError _ _ = InvalidMinion
    pickGuard _ = flip elem


instance Pickable s PlayerHandle where
    promptPick = PickPlayer
    pickFailError _ _ = InvalidPlayer
    pickGuard _ = flip elem


instance Pickable s CharacterHandle where
    promptPick = PickCharacter
    pickFailError _ _ = InvalidCharacter
    pickGuard _ = flip elem


instance Pickable s HandCard where
    promptPick = PickHandCard
    pickFailError _ _ = InvalidHandCard
    pickGuard _ cs c = cardName c `elem` map cardName cs


instance Pickable s (Elect s) where
    promptPick = PickElect
    pickFailError _ _ = InvalidElect
    pickGuard _ _ _ = True -- TODO: Make this actually guard


class (EnactElect s, PurePick s) => PickFrom s where
    pickFrom :: forall m a. (HearthMonad m, Pickable s a) => [a] -> Hearth m (Available (PickResult s a))


instance PickFrom AtRandom where
    pickFrom :: forall m a. (HearthMonad m, Pickable AtRandom a) => [a] -> Hearth m (Available (PickResult AtRandom a))
    pickFrom = logCall 'pickFrom . \case
        [] -> return NotAvailable
        xs -> liftM Available $ do
            snapshot <- gets GameSnapshot
            guardedPrompt (PromptPickAtRandom snapshot $ promptPick $ NonEmpty.fromList xs) $ \case
                AtRandomPick x -> case pickGuard (Proxy :: Proxy AtRandom) xs x of
                    True -> return True
                    False -> do
                        prompt $ PromptError $ pickFailError (Proxy :: Proxy AtRandom) (Proxy :: Proxy a)
                        return False


instance PickFrom Targeted where
    pickFrom :: forall m a. (HearthMonad m, Pickable Targeted a) => [a] -> Hearth m (Available (PickResult Targeted a))
    pickFrom = logCall 'pickFrom . \case
        [] -> return NotAvailable
        xs -> liftM Available $ do
            snapshot <- gets GameSnapshot
            guardedPrompt (PromptPickTargeted snapshot $ promptPick $ NonEmpty.fromList xs) $ \case
                AbortTargetedPick -> return True
                TargetedPick x -> case pickGuard (Proxy :: Proxy Targeted) xs x of
                    True -> return True
                    False -> do
                        prompt $ PromptError $ pickFailError (Proxy :: Proxy Targeted) (Proxy :: Proxy a)
                        return False


playCommon :: (HearthMonad m) => Handle Player -> HandCard -> Hearth m Result
playCommon handle card = logCall 'playCommon $ removeFromHand handle card >>= \case
    False -> return $ Failure "Could not play card because it is not in hand."
    True -> payCost handle $ costOf card


costOf :: HandCard -> Cost
costOf = \case
    HandCardMinion minion -> minion^.minionCost
    HandCardSpell spell -> spell^.spellCost


payCost :: (HearthMonad m) => Handle Player -> Cost -> Hearth m Result
payCost who = logCall 'payCost $  \case
    ManaCost mana -> payManaCost who mana


payManaCost :: (HearthMonad m) => Handle Player -> Mana -> Hearth m Result
payManaCost who (Mana cost) = logCall 'payManaCost $ zoom (getPlayer who) $ do
    totalMana <- view playerTotalManaCrystals
    emptyMana <- view playerEmptyManaCrystals
    let availableMana = totalMana - emptyMana
    case cost <= availableMana of
        False -> return $ Failure "Not enough mana."
        True -> do
            playerEmptyManaCrystals += cost
            return Success


clearDeadMinions :: (HearthMonad m) => Hearth m ()
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


areAdjacent :: (HearthMonad m) => Handle Minion -> Handle Minion -> Hearth m Bool
areAdjacent handle1 handle2 = do
    minionsByPlayer <- viewListOf $ gamePlayers.traversed.playerMinions
    return $ flip any minionsByPlayer $ \minions -> let
        handles = map _boardMinionHandle minions
        in comesAfter handle1 handle2 handles || comesAfter handle2 handle1 handles


removeMinion :: (HearthMonad m) => Handle Minion -> Hearth m ()
removeMinion minion = do
    owner <- ownerOf minion
    getPlayer owner.playerMinions %= filter (\bm -> bm^.boardMinionHandle /= minion)


removeSpell :: (HearthMonad m) => Handle Spell -> Hearth m ()
removeSpell spell = do
    owner <- ownerOf spell
    getPlayer owner.playerSpells %= filter (\s -> s^.castSpellHandle /= spell)


viewAbility :: (HearthMonad m) => (Ability -> Bool) -> Handle Minion -> Hearth m Bool
viewAbility predicate bmHandle = logCall 'viewAbility $ do
    abilities <- viewMinionAbilities bmHandle
    return $ any predicate abilities


viewWindfury :: (HearthMonad m) => Handle Minion -> Hearth m Bool
viewWindfury = logCall 'viewWindfury $ viewAbility $ \case
    Windfury -> True
    _ -> False


viewTaunt :: (HearthMonad m) => Handle Minion -> Hearth m Bool
viewTaunt = logCall 'viewTaunt $ viewAbility $ \case
    Taunt -> True
    _ -> False


viewCharge :: (HearthMonad m) => Handle Minion -> Hearth m Bool
viewCharge = logCall 'viewCharge $ viewAbility $ \case
    Charge -> True
    _ -> False


viewHasTauntMinions :: (HearthMonad m) => Handle Player -> Hearth m Bool
viewHasTauntMinions player = logCall 'viewHasTauntMinions $ do
    view (getPlayer player.playerMinions) >>= anyM (viewTaunt . _boardMinionHandle)


isAlly :: (Ownable a, HearthMonad m) => a -> Hearth m Bool
isAlly bm = do
    owner <- ownerOf bm
    active <- getActivePlayerHandle
    return $ owner == active


isEnemy :: (Ownable a, HearthMonad m) => a -> Hearth m Bool
isEnemy = liftM not . isAlly


viewRemainingAttacks :: (CharacterTraits a, HearthMonad m) => a -> Hearth m Int
viewRemainingAttacks c = logCall 'viewRemainingAttacks $ liftM2 (\x y -> max 0 $ x - y) (viewMaxAttackCount c) (viewAttackCount c)


isLegalAttackSetup :: (HearthMonad m) => Handle Character -> Handle Character -> Hearth m Result
isLegalAttackSetup attacker defender = logCall 'isLegalAttackSetup $ dynamic $ do
    let viewDefenderTaunt = case defender of
            PlayerCharacter _ -> return False
            MinionCharacter m -> viewTaunt m
    isAlly attacker >>= \case
        False -> do
            promptGameEvent $ AttackFailed AttackWithEnemy
            return $ Failure "Can't attack with an enemy character."
        True -> do
            attack <- viewAttack attacker
            case attack <= 0 of
                True -> do
                    promptGameEvent $ AttackFailed ZeroAttack
                    return $ Failure "Can't attack with a zero attack minion"
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


enactAttack :: (HearthMonad m) => Handle Character -> Handle Character -> Hearth m Result
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


replaceMinionByHandle :: (HearthMonad m) => BoardMinion -> Hearth m ()
replaceMinionByHandle bm' = logCall 'replaceMinionByHandle $ do
    owner <- ownerOf $ bm'^.boardMinionHandle
    getPlayer owner.playerMinions %= \bms -> let
        (front, _ : end) = span (\bm -> bm^.boardMinionHandle /= bm'^.boardMinionHandle) bms
        in front ++ [bm'] ++ end
    

loseDivineShield :: BoardMinion -> Maybe BoardMinion
loseDivineShield bm = let
    abilities = bm^.boardMinionAbilities
    abilities' = flip filter abilities $ \case
        DivineShield -> False
        _ -> True
    in case on (==) length abilities abilities' of
        True -> Nothing
        False -> Just $ bm & boardMinionAbilities .~ abilities'


viewEventListeners :: (HearthMonad m) => Hearth m [EventListener]
viewEventListeners = logCall 'viewEventListeners $ do
    minions <- viewListOf $ gamePlayers.traversed.playerMinions.traversed.boardMinionHandle
    minionListeners <- liftM concat $ forM minions $ \minion -> do
        abilities <- viewMinionAbilities minion
        return $ flip mapMaybe abilities $ \case
            Whenever listener -> Just $ listener minion
            _ -> Nothing
    effectObservers <- view gameEffectObservers
    return $ effectObservers ++ minionListeners


processEvent :: (HearthMonad m) => (EventListener -> Hearth m ()) -> Hearth m ()
processEvent f = dynamic viewEventListeners >>= mapM_ f


handleGameEvent :: (HearthMonad m) => GameEvent -> Hearth m ()
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







































