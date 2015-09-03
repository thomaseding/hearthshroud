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
import Hearth.Action
import Hearth.CardName
import Hearth.Cards (cardByName)
import Hearth.DebugEvent
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
        _gameEffectObservers = [],
        _gamePlayers = zipWith mkPlayer (map PlayerHandle [0..]) ps }


mkPlayer :: Handle Player -> PlayerData -> Player
mkPlayer handle (PlayerData hero deck) = Player' {
    _playerHandle = handle,
    _playerDeck = deck,
    _playerExcessDrawCount = 0,
    _playerHand = Hand [],
    _playerMinions = [],
    _playerSpells = [],
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
        h @ PlayerHandle {} -> do
            prompt $ PromptDebugEvent $ DiagnosticMessage $ "Warning: " ++ show 'ownerOf ++ " used with " ++ show 'PlayerHandle
            return h
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
    handle <- view $ gameHandleSeed
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
    AtRandomPick handle <- prompt $ PromptPickAtRandom $ PickPlayer snap $ NonEmpty.fromList handles
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
        in case on isSubsetOf (map handCardName) keptCards drawnCards of
            True -> return True
            False -> do
                prompt $ PromptError InvalidMulligan
                return False
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


drawCards :: (HearthMonad m) => Handle Player -> Int -> Hearth m [HandCard]
drawCards handle = logCall 'drawCards $ liftM catMaybes . flip replicateM (drawCard handle)


putInHand :: (HearthMonad m) => Handle Player -> HandCard -> Hearth m Bool
putInHand handle card = logCall 'putInHand $ zoom (getPlayer handle.playerHand.handCards) $ do
    to length >>=. \case
        10 -> return False
        _ -> do
            id %= (card :)
            return True


removeFromHand :: (HearthMonad m) => Handle Player -> HandCard -> Hearth m Bool
removeFromHand handle card = logCall 'removeFromHand $ zoom (getPlayer handle.playerHand.handCards) $ do
    hand <- view id
    id %= deleteBy (on (==) handCardName) card
    hand' <- view id
    return $ length hand /= length hand'


drawCard :: (HearthMonad m) => Handle Player -> Hearth m (Maybe HandCard)
drawCard handle = logCall 'drawCard $ getPlayer handle.playerDeck >>=. \case
    Deck [] -> do
        getPlayer handle.playerExcessDrawCount += 1
        excess <- view $ getPlayer handle.playerExcessDrawCount
        dealDamage (PlayerCharacter handle) (Damage excess) Fatigue
        return Nothing
    Deck (c:cs) -> do
        let c' = deckToHand c
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
isMortallyWounded = logCall 'isMortallyWounded $ liftM (<= 0) . dynamicRemainingHealth


shuffleDeck :: (HearthMonad m) => Handle Player -> Hearth m ()
shuffleDeck handle = logCall 'shuffleDeck $ do
    Deck deck <- view $ getPlayer handle.playerDeck
    deck' <- liftM Deck $ guardedPrompt (PromptShuffle deck) $ \deck' -> let
        f = sort . map deckCardName
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
        10 -> do
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


clearUntil :: (HearthMonad m) => TimePoint -> Hearth m ()
clearUntil candidate = do
    minions <- viewListOf $ gamePlayers.traversed.playerMinions.traversed.boardMinionHandle
    forM_ minions $ \minion -> do
        getMinion minion.boardMinionEnchantments %= mapMaybe predicate
    where
        predicate = \case
            Continuous e -> Just $ Continuous e
            Limited e -> fmap Limited $ case e of
                Until timePoint e' -> case timePoint of
                    Delay n timePoint' -> case timePoint' == candidate of
                        True -> Just $ case n of
                            1 -> Until timePoint' e'
                            _ -> Until (Delay (n - 1) timePoint') e'
                        False -> Just e
                    _ -> case timePoint == candidate of
                        True -> Nothing
                        False -> Just e


beginTurn :: (HearthMonad m) => Hearth m ()
beginTurn = logCall 'beginTurn $ scopedPhase BeginTurnPhase $ do
    clearUntil BeginOfTurn
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
    clearUntil EndOfTurn
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
    health <- dynamicMaxHealth p
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


placeOnBoard :: (HearthMonad m) => Handle Player -> BoardPos -> Minion -> Hearth m (Either String MinionHandle)
placeOnBoard handle (BoardPos pos) minion = logCall 'placeOnBoard $ do
    minionHandle <- genHandle
    zoom (getPlayer handle.playerMinions) $ do
        to length >>=. \case
            7 -> return $ Left "Board is too full."
            len -> case 0 <= pos && pos <= len of
                False -> return $ Left "Invalid board index."
                True -> do
                    id %= insertAt pos (toBoardMinion minionHandle minion)
                    return $ Right minionHandle


actionAttack :: (HearthMonad m) => Handle Character -> Handle Character -> Hearth m Result
actionAttack attacker defender = logCall 'actionAttack $ do
    enactAttack attacker defender


actionPlayMinion :: (HearthMonad m) => HandCard -> BoardPos -> Hearth m Result
actionPlayMinion card pos = logCall 'actionPlayMinion $ do
    pHandle <- getActivePlayerHandle
    playMinion pHandle card pos


actionPlaySpell :: (HearthMonad m) => HandCard -> Hearth m Result
actionPlaySpell card = logCall 'actionPlaySpell $ do
    handle <- getActivePlayerHandle
    playSpell handle card


playMinion :: (HearthMonad m) => Handle Player -> HandCard -> BoardPos -> Hearth m Result
playMinion pHandle card pos = logCall 'playMinion $ do
    st <- get
    playMinion' pHandle card pos >>= \case
        Left msg -> do
            put st
            return $ Failure msg
        Right bmHandle -> do
            promptGameEvent $ PlayedMinion pHandle bmHandle
            result <- enactAnyBattleCries bmHandle
            when (result /= Success) $ put st
            return result


playMinion' :: (HearthMonad m) => Handle Player -> HandCard -> BoardPos -> Hearth m (Either String MinionHandle)
playMinion' handle card pos = logCall 'playMinion' $ playCommon handle card >>= \case
    Failure msg -> return $ Left msg
    Success -> case card of
        HandCardMinion minion -> placeOnBoard handle pos minion
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
    abilities <- dynamicAbilities bmHandle
    forM_ abilities $ \case
        KeywordAbility (Deathrattle elect) -> enactDeathrattle bmHandle elect
        _ -> return ()


enactAnyBattleCries :: (HearthMonad m) => Handle Minion -> Hearth m Result
enactAnyBattleCries bmHandle = logCall 'enactAnyBattleCries $ do
    st <- get
    abilities <- dynamicAbilities bmHandle
    result <- liftM condensePickResults $ forM abilities $ \case
        KeywordAbility (Battlecry effect) -> enactBattlecry bmHandle effect
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
    DealDamage victim damage source -> dealDamage victim damage source >> return success
    Enchant handle enchantment -> enchant handle enchantment >> return success
    GrantAbilities handle abilities -> grantAbilities handle abilities >> return success
    GainManaCrystals handle amount crystalState -> gainManaCrystals handle amount crystalState >> return success
    DestroyMinion handle -> destroyMinion handle >> return success
    RestoreHealth handle amount -> restoreHealth handle amount >> return success
    Transform handle minion -> transform handle minion >> return success
    Silence handle -> silence handle >> return success
    GainArmor handle armor -> gainArmor handle armor >> return success
    Freeze handle -> freeze handle >> return success
    Observing effect listener -> enactObserving effect listener
    where
        success = purePick ()


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
    Satisfies handle restrictions -> satisfies handle restrictions


freeze :: (HearthMonad m) => Handle Character -> Hearth m ()
freeze character = logCall 'freeze $ do
    active <- getActivePlayerHandle
    owner <- ownerOf character
    timePoint <- case active == owner of
        False -> return $ Delay 1 BeginOfTurn
        True -> liftM (== 0) (getAttackCount character) >>= \case
            True -> return EndOfTurn
            False -> return $ Delay 1 EndOfTurn
    case character of
        PlayerCharacter {} -> $todo 'freeze "need to implement player enchantments"
        MinionCharacter minion -> do
            getMinion minion.boardMinionEnchantments %= (++ [Limited $ Until timePoint Frozen])


enactWhen :: (HearthMonad m) => Handle a -> [Restriction a] -> Effect -> Hearth m ()
enactWhen handle restrictions effect = do
    whenM (satisfies handle restrictions) $ do
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
    promptGameEvent $ HealthRestored charHandle (Health actualAmount)
    where
        restore = max 0 . (subtract $ Damage amount)
        restoreM = do
            before <- view id
            after <- id <%= restore
            return $ unDamage $ before - after


destroyMinion :: (HearthMonad m) => Handle Minion -> Hearth m ()
destroyMinion handle = logCall 'destroyMinion $ do
    getMinion handle.boardMinionPendingDestroy .= True
    promptGameEvent $ MinionDestroyed handle


enactForEach :: (HearthMonad m) => HandleList a -> (Handle a -> Effect) -> Hearth m (SimplePickResult AtRandom)
enactForEach (HandleList handles) cont = logCall 'enactForEach $ do
    liftM condensePickResults $ forM handles (enactEffect . cont)


sequenceEffects :: (HearthMonad m) => [Effect] -> Hearth m (SimplePickResult AtRandom)
sequenceEffects effects = liftM condensePickResults $ mapM enactEffect effects


condensePickResults :: (PurePick s) => [SimplePickResult s] -> SimplePickResult s
condensePickResults results = case dropWhile (== purePick ()) results of
    [] -> purePick ()
    r : _ -> r


grantAbilities :: (HearthMonad m) => Handle Minion -> [Ability] -> Hearth m ()
grantAbilities handle abilities = logCall 'grantAbilities $ do
    getMinion handle.boardMinionAbilities %= (abilities ++)


enchant :: (HearthMonad m) => Handle Minion -> AnyEnchantment -> Hearth m ()
enchant handle enchantment = logCall 'enchant $ do
    getMinion handle.boardMinionEnchantments %= (++ [enchantment])


isDamaged :: BoardMinion -> Bool
isDamaged bm = bm^.boardMinionDamage > 0


isEnraged :: (HearthMonad m) => Handle Minion -> Hearth m Bool
isEnraged bmHandle = do
    bm <- view $ getMinion bmHandle
    case isDamaged bm of
         False -> return False
         True -> do
            abilities <- dynamicAbilities bmHandle
            return $ any isEnrage abilities
    where
        isEnrage = \case
            KeywordAbility (Enrage {}) -> True
            _ -> False


dynamicAbilities :: (HearthMonad m) => Handle Minion -> Hearth m [Ability]
dynamicAbilities bmHandle = do
    bm <- view $ getMinion bmHandle
    return $ bm^.boardMinionAbilities >>= \ability -> case ability of
        KeywordAbility (Enrage abilities _) -> case isDamaged bm of
            True -> ability : abilities  -- TODO: Need to check correct interleaving.
            False -> [ability]
        _ -> [ability]


dynamicEnchantments :: (HearthMonad m) => Handle Minion -> Hearth m [AnyEnchantment]
dynamicEnchantments bmHandle = logCall 'dynamicEnchantments $ do
    bm <- view $ getMinion bmHandle
    let baseEnchantments = bm^.boardMinionEnchantments
        enrageEnchantments = case isDamaged bm of
            False -> []
            True -> bm^.boardMinionAbilities >>= \case
                KeywordAbility (Enrage _ es) -> es
                _ -> []
    return $ baseEnchantments ++ map Continuous enrageEnchantments -- TODO: Need to check correct interleaving.


underAnyEnchantment :: forall b. (forall a. Enchantment a -> b) -> AnyEnchantment -> b
underAnyEnchantment f = \case
    Continuous e -> f e
    Limited e -> f e


hasEnchantment :: (HearthMonad m) => Enchantment Continuous -> Handle Minion -> Hearth m Bool
hasEnchantment candidate minion = logCall 'hasEnchantment $ do
    enchantments <- dynamicEnchantments minion
    return $ any (underAnyEnchantment predicate) enchantments
    where
        predicate :: Enchantment a -> Bool
        predicate = \case
            Until _ e -> predicate e
            e @ StatsDelta{} -> e == candidate
            e @ StatsScale{} -> e == candidate
            e @ ChangeStat{} -> e == candidate
            e @ SwapStats{} -> e == candidate
            e @ Frozen{} -> e == candidate


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


class (Ownable h, IsCharacterHandle h) => CharacterTraits h where
    getDamage :: (HearthMonad m) => h -> Hearth m Damage
    dynamicAttack :: (HearthMonad m) => h -> Hearth m Attack
    dynamicMaxHealth :: (HearthMonad m) => h -> Hearth m Health
    dynamicMaxAttackCount :: (HearthMonad m) => h -> Hearth m Int
    getAttackCount :: (HearthMonad m) => h -> Hearth m Int
    bumpAttackCount :: (HearthMonad m) => h -> Hearth m ()
    hasSummoningSickness :: (HearthMonad m) => h -> Hearth m Bool
    isFrozen :: (HearthMonad m) => h -> Hearth m Bool


instance CharacterTraits PlayerHandle where
    getDamage pHandle = logCall 'getDamage $ do
        view $ getPlayer pHandle.playerHero.boardHeroDamage
    dynamicAttack pHandle = logCall 'dynamicAttack $ do
        view $ getPlayer pHandle.playerHero.boardHero.heroAttack
    dynamicMaxHealth pHandle = logCall 'dynamicMaxHealth $ do
        view $ getPlayer pHandle.playerHero.boardHero.heroHealth
    dynamicMaxAttackCount _ = logCall 'dynamicMaxAttackCount $ do
        return 1
    getAttackCount pHandle = logCall 'getAttackCount $ do
        view $ getPlayer pHandle.playerHero.boardHeroAttackCount
    bumpAttackCount pHandle = logCall 'bumpAttackCount $ do
        getPlayer pHandle.playerHero.boardHeroAttackCount += 1
    hasSummoningSickness _ = return False
    isFrozen _ = logCall 'isFrozen $ return False


auraAbilitiesOf :: [Ability] -> [Handle Minion -> Aura]
auraAbilitiesOf = mapMaybe $ \case
    Aura aura -> Just aura
    _ -> Nothing


observeDynamicState :: (HearthMonad m) => Handle Minion -> Hearth m a -> Hearth m a
observeDynamicState handle action = logCall 'observeDynamicState $ local id $ do
    applyAuras
    applyEnchantments
    action
    where
        applyAuras = do
            minions <- viewListOf $ gamePlayers.traversed.playerMinions.traversed.boardMinionHandle
            forM_ minions $ \minion -> do
                auras <- liftM auraAbilitiesOf $ dynamicAbilities minion
                forM_ auras $ enactAura . ($ minion)
        applyEnchantments = do
            enchantments <- dynamicEnchantments handle
            forM_ enchantments $ underAnyEnchantment applyEnchantment
        applyEnchantment :: (HearthMonad m) => Enchantment a -> Hearth m ()
        applyEnchantment = \case
                StatsDelta a h -> do
                    getMinion handle.boardMinion.minionAttack += a
                    getMinion handle.boardMinion.minionHealth += h
                StatsScale a h -> do
                    getMinion handle.boardMinion.minionAttack *= a
                    getMinion handle.boardMinion.minionHealth *= h
                ChangeStat e -> case e of
                    Left a -> getMinion handle.boardMinion.minionAttack .= a
                    Right h -> getMinion handle.boardMinion.minionHealth .= h
                SwapStats -> do
                    Attack attack <- view $ getMinion handle.boardMinion.minionAttack
                    Health health <- view $ getMinion handle.boardMinion.minionHealth
                    getMinion handle.boardMinion.minionAttack .= Attack health
                    getMinion handle.boardMinion.minionHealth .= Health attack
                Frozen -> return ()
                Until _ enchantment -> applyEnchantment enchantment


enactAura :: (HearthMonad m) => Aura -> Hearth m ()
enactAura = logCall 'enactAura $ \case
    AuraOwnerOf handle cont -> ownerOf handle >>= enactAura . cont
    AuraOpponentOf handle cont -> opponentOf handle >>= enactAura . cont
    While handle restrictions aura -> enactWhile handle restrictions aura
    EachMinion restrictions cont -> enactEachMinion restrictions cont
    Has handle enchantment -> enactHas handle enchantment


enactEachMinion :: (HearthMonad m) => [Restriction Minion] -> (Handle Minion -> Aura) -> Hearth m ()
enactEachMinion restrictions cont = logCall 'enactEachMinion $ do
    minions <- viewListOf $ gamePlayers.traversed.playerMinions.traversed.boardMinionHandle
    forM_ minions $ \minion -> do
        whenM (satisfies minion restrictions) $ enactAura $ cont minion


enactWhile :: (HearthMonad m) => Handle a -> [Restriction a] -> Aura -> Hearth m ()
enactWhile handle restrictions = logCall 'enactWhile $ whenM (satisfies handle restrictions) . enactAura


enactHas :: (HearthMonad m) => Handle Minion -> Enchantment Continuous -> Hearth m ()
enactHas handle enchantment = logCall 'enactHas $ do
    getMinion handle.boardMinionEnchantments %= (++ [Continuous enchantment])


instance CharacterTraits MinionHandle where
    getDamage bmHandle = logCall 'getDamage $ do
        view $ getMinion bmHandle.boardMinionDamage
    dynamicAttack bmHandle = logCall 'dynamicAttack $ do
        observeDynamicState bmHandle $ view $ getMinion bmHandle.boardMinion.minionAttack
    dynamicMaxHealth bmHandle = logCall 'dynamicMaxHealth $ do
        observeDynamicState bmHandle $ view $ getMinion bmHandle.boardMinion.minionHealth
    dynamicMaxAttackCount _ = logCall 'dynamicMaxAttackCount $ do
        return 1
    getAttackCount bmHandle = logCall 'getAttackCount $ do
        view $ getMinion bmHandle.boardMinionAttackCount
    bumpAttackCount bmHandle = logCall 'bumpAttackCount $ do
        getMinion bmHandle.boardMinionAttackCount += 1
    hasSummoningSickness bmHandle = logCall 'hasSummoningSickness $ do
        bm <- view $ getMinion bmHandle
        case bm^.boardMinionNewlySummoned of
            False -> return False
            True -> liftM not $ dynamicHasCharge bmHandle
    isFrozen handle = logCall 'isFrozen $ hasEnchantment Frozen handle


instance CharacterTraits CharacterHandle where
    getDamage = logCall 'getDamage $ \case
        PlayerCharacter h -> getDamage h
        MinionCharacter h -> getDamage h
    dynamicAttack = logCall 'dynamicAttack $ \case
        PlayerCharacter h -> dynamicAttack h
        MinionCharacter h -> dynamicAttack h
    dynamicMaxHealth = logCall 'dynamicMaxHealth $ \case
        PlayerCharacter h -> dynamicMaxHealth h
        MinionCharacter h -> dynamicMaxHealth h
    dynamicMaxAttackCount = logCall 'dynamicMaxAttackCount $ \case
        PlayerCharacter h -> dynamicMaxAttackCount h
        MinionCharacter h -> dynamicMaxAttackCount h
    getAttackCount = logCall 'getAttackCount $ \case
        PlayerCharacter h -> getAttackCount h
        MinionCharacter h -> getAttackCount h
    bumpAttackCount = logCall 'bumpAttackCount $ \case
        PlayerCharacter h -> bumpAttackCount h
        MinionCharacter h -> bumpAttackCount h
    hasSummoningSickness = logCall 'hasSummoningSickness $ \case
        PlayerCharacter h -> hasSummoningSickness h
        MinionCharacter h -> hasSummoningSickness h
    isFrozen = logCall 'isFrozen $ \case
        PlayerCharacter h -> isFrozen h
        MinionCharacter h -> isFrozen h


dynamicRemainingHealth :: (HearthMonad m) => Handle Character -> Hearth m Health
dynamicRemainingHealth h = do
    damage <- getDamage h
    health <- dynamicMaxHealth h
    return $ health - Health (unDamage damage)


dealDamage :: (HearthMonad m) => Handle Character -> Damage -> DamageSource -> Hearth m ()
dealDamage charHandle damage source = logCall 'dealDamage $ case damage <= 0 of
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
    health <- dynamicMaxHealth victim
    zoom (getMinion victim) $ do
        boardMinionAbilities .= []
        boardMinionEnchantments .= []
    health' <- dynamicMaxHealth victim
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
    Minion restrictions cont -> enactMinion restrictions cont
    Player restrictions cont -> enactPlayer restrictions cont
    Character restrictions cont -> enactCharacter restrictions cont


enactAll :: (HearthMonad m, PickFrom s) => All s -> Hearth m (SimplePickResult s)
enactAll = logCall 'enactAll . \case
    Minions restrictions cont -> enactMinions restrictions cont
    Players restrictions cont -> enactPlayers restrictions cont
    Characters restrictions cont -> enactCharacters restrictions cont


enactMinion :: (HearthMonad m, PickFrom s) => [Restriction Minion] -> (Handle Minion -> Elect s) -> Hearth m (SimplePickResult s)
enactMinion restrictions cont = logCall 'enactMinion $ do
    pHandles <- getPlayerHandles
    candidates <- liftM concat $ forM pHandles $ \pHandle -> do
        bms <- view $ getPlayer pHandle.playerMinions
        return $ map (\bm -> bm^.boardMinionHandle) bms
    restrict restrictions candidates >>= pickFrom >>= enactElect' cont


enactPlayer :: (HearthMonad m, PickFrom s) => [Restriction Player] -> (Handle Player -> Elect s) -> Hearth m (SimplePickResult s)
enactPlayer restrictions cont = logCall 'enactPlayer $ do
    candidates <- getPlayerHandles
    restrict restrictions candidates >>= pickFrom >>= enactElect' cont


enactCharacter :: (HearthMonad m, PickFrom s) => [Restriction Character] -> (Handle Character -> Elect s) -> Hearth m (SimplePickResult s)
enactCharacter restrictions cont = logCall 'enactCharacter $ do
    pHandles <- getPlayerHandles
    minionCandidates <- liftM concat $ forM pHandles $ \pHandle -> do
        bms <- view $ getPlayer pHandle.playerMinions
        return $ map (\bm -> MinionCharacter $ bm^.boardMinionHandle) bms
    let playerCandidates = map PlayerCharacter pHandles
        candidates = playerCandidates ++ minionCandidates
    restrict restrictions candidates >>= pickFrom >>= enactElect' cont


enactMinions :: (HearthMonad m, PickFrom s) => [Restriction Minion] -> (HandleList Minion -> Elect s) -> Hearth m (SimplePickResult s)
enactMinions restrictions cont = logCall 'enactMinions $ do
    candidates <- viewListOf $ gamePlayers.traversed.playerMinions.traversed.boardMinionHandle
    restrict restrictions candidates >>= enactElect . cont . HandleList


enactPlayers :: (HearthMonad m, PickFrom s) => [Restriction Player] -> (HandleList Player -> Elect s) -> Hearth m (SimplePickResult s)
enactPlayers restrictions cont = logCall 'enactPlayers $ do
    candidates <- getPlayerHandles
    restrict restrictions candidates >>= enactElect . cont . HandleList


enactCharacters :: (HearthMonad m, PickFrom s) => [Restriction Character] -> (HandleList Character -> Elect s) -> Hearth m (SimplePickResult s)
enactCharacters restrictions cont = logCall 'enactCharacters $ do
    playerCandidates <- getPlayerHandles
    minionCandidates <- viewListOf $ gamePlayers.traversed.playerMinions.traversed.boardMinionHandle
    let candidates = map PlayerCharacter playerCandidates ++ map MinionCharacter minionCandidates
    restrict restrictions candidates >>= enactElect . cont . HandleList


restrict :: (HearthMonad m) => [Restriction a] -> [Handle a] -> Hearth m [Handle a]
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


instance CanSatisfy a (Restriction a) where
    satisfies candidate = \case
        WithMinion r -> MinionCharacter candidate `satisfies` r
        WithPlayer r -> PlayerCharacter candidate `satisfies` r
        OwnedBy owner -> liftM (owner ==) $ ownerOf candidate
        Is object -> return $ candidate == object
        Not object -> return $ candidate /= object
        IsDamageSource source -> case source of
            DamagingCharacter character -> return $ candidate == character
            _ -> return False
        WithAttack cmp attackVal -> do
            actualAttack <- dynamicAttack candidate
            return $ fromComparison cmp actualAttack attackVal
        WithHealth cmp healthVal -> do
            actualHealth <- dynamicRemainingHealth candidate
            return $ fromComparison cmp actualHealth healthVal
        Damaged -> liftM (> 0) $ getDamage candidate
        Undamaged -> liftM not $ candidate `satisfies` Damaged
        IsMinion -> return $ case candidate of
            MinionCharacter {} -> True
            _ -> False
        AdjacentTo handle -> areAdjacent handle candidate


instance CanSatisfy a [Restriction a] where
    satisfies h rs = allM (satisfies h) rs


class Pickable (s :: Selection) a where
    promptPick :: GameSnapshot -> NonEmpty a -> PromptPick s a
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
            guardedPrompt (PromptPickAtRandom $ promptPick snapshot $ NonEmpty.fromList xs) $ \case
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
            guardedPrompt (PromptPickTargeted $ promptPick snapshot $ NonEmpty.fromList xs) $ \case
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


dynamicHasAbility :: (HearthMonad m) => (Ability -> Bool) -> Handle Minion -> Hearth m Bool
dynamicHasAbility predicate bmHandle = logCall 'dynamicHasAbility $ do
    abilities <- dynamicAbilities bmHandle
    return $ any predicate abilities


dynamicHasTaunt :: (HearthMonad m) => Handle Minion -> Hearth m Bool
dynamicHasTaunt = logCall 'dynamicHasTaunt $ dynamicHasAbility $ \case
    KeywordAbility Taunt -> True
    _ -> False


dynamicHasCharge :: (HearthMonad m) => Handle Minion -> Hearth m Bool
dynamicHasCharge = logCall 'dynamicHasCharge $ dynamicHasAbility $ \case
    KeywordAbility Charge -> True
    _ -> False


hasTauntMinions :: (HearthMonad m) => Handle Player -> Hearth m Bool
hasTauntMinions player = logCall 'hasTauntMinions $ do
    view (getPlayer player.playerMinions) >>= anyM (dynamicHasTaunt . _boardMinionHandle)


isAlly :: (Ownable a, HearthMonad m) => a -> Hearth m Bool
isAlly bm = do
    owner <- ownerOf bm
    active <- getActivePlayerHandle
    return $ owner == active


isEnemy :: (Ownable a, HearthMonad m) => a -> Hearth m Bool
isEnemy = liftM not . isAlly


hasRemainingAttacks :: (CharacterTraits a, HearthMonad m) => a -> Hearth m Bool
hasRemainingAttacks c = liftM2 (<) (getAttackCount c) (dynamicMaxAttackCount c)


enactAttack :: (HearthMonad m) => Handle Character -> Handle Character -> Hearth m Result
enactAttack attacker defender = logCall 'enactAttack $ do
    let attackerHandle = characterHandle attacker
        defenderHandle = characterHandle defender
        defenderHasTaunt = case defender of
            PlayerCharacter _ -> return False
            MinionCharacter m -> dynamicHasTaunt m
    result <- isAlly attacker >>= \case
        False -> do
            promptGameEvent $ AttackFailed AttackWithEnemy
            return $ Failure "Can't attack with an enemy character."
        True -> do
            attack <- dynamicAttack attacker
            case attack <= 0 of
                True -> do
                    promptGameEvent $ AttackFailed ZeroAttack
                    return $ Failure "Can't attack with a zero attack minion"
                False -> isEnemy defender >>= \case
                    False -> do
                        promptGameEvent $ AttackFailed DefendWithFriendly
                        return $ Failure "Can't attack into a friendly character."
                    True -> hasSummoningSickness attacker >>= \case
                        True -> do
                            promptGameEvent $ AttackFailed DoesNotHaveCharge
                            return $ Failure "Minion needs charge to attack."
                        False -> hasRemainingAttacks attacker >>= \case
                            False -> do
                                promptGameEvent $ AttackFailed OutOfAttacks
                                return $ Failure "Character is out of attacks."
                            True -> isFrozen attacker >>= \case
                                True -> do
                                    promptGameEvent $ AttackFailed AttackerIsFrozen
                                    return $ Failure "Attacker is frozen."
                                False -> defenderHasTaunt >>= \case
                                    True -> return Success
                                    False -> do
                                        defenderController <- ownerOf defender
                                        hasTauntMinions defenderController >>= \case
                                            True -> do
                                                promptGameEvent $ AttackFailed TauntsExist
                                                return $ Failure "Must attack a minion with taunt."
                                            False -> return Success
    case result of
        Failure msg -> return $ Failure msg
        Success -> do
            promptGameEvent $ EnactAttack attackerHandle defenderHandle
            let source `harms` victim = do
                    dmg <- liftM (Damage . unAttack) $ dynamicAttack source
                    dealDamage (characterHandle victim) dmg (DamagingCharacter source)
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
        KeywordAbility DivineShield -> False
        _ -> True
    in case on (==) length abilities abilities' of
        True -> Nothing
        False -> Just $ bm & boardMinionAbilities .~ abilities'


dynamicEventListeners :: (HearthMonad m) => Hearth m [EventListener]
dynamicEventListeners = do
    minions <- viewListOf $ gamePlayers.traversed.playerMinions.traversed.boardMinionHandle
    minionListeners <- liftM concat $ forM minions $ \minion -> do
        abilities <- dynamicAbilities minion
        return $ flip mapMaybe abilities $ \case
                Whenever listener -> Just $ listener minion
                _ -> Nothing
    effectObservers <- view gameEffectObservers
    return $ effectObservers ++ minionListeners


processEvent :: (HearthMonad m) => (EventListener -> Hearth m ()) -> Hearth m ()
processEvent f = dynamicEventListeners >>= mapM_ f


handleGameEvent :: (HearthMonad m) => GameEvent -> Hearth m ()
handleGameEvent = \case
    PlayedSpell _ spell -> processEvent $ \case
        SpellIsCast listener -> run $ listener spell
        _ -> return ()
    DealtDamage victim damage source -> processEvent $ \case
        DamageIsDealt listener -> run $ listener victim damage source
        _ -> return ()
    _ -> return ()
    where
        run cont = scopedPhase TriggeredEffectPhase $ enactElect cont >> return ()



























































