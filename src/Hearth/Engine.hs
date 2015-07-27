{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Hearth.Engine (
    module Hearth.Engine,
    module Hearth.Engine.Data,
) where


--------------------------------------------------------------------------------


import Control.Error
import Control.Lens
import Control.Lens.Helper
import Control.Lens.Internal.Zoom (Zoomed)
import Control.Monad.Loops
import Control.Monad.Prompt
import Control.Monad.Reader
import Control.Monad.State
import Data.Function
import Data.List
import Data.List.Ordered
import Data.Maybe
import qualified Data.NonEmpty as NonEmpty
import Hearth.Action
import Hearth.Cards (cardByName)
import Hearth.DeckToHand
import Hearth.Engine.Data
import Hearth.GameEvent
import Hearth.HandToDeck
import Hearth.Model
import Hearth.Names (CardName(BasicCardName))
import Hearth.Names.Basic (BasicCardName(TheCoin))
import Hearth.Prompt


--------------------------------------------------------------------------------


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
getPlayer pHandle = lens getter setter
    where
        getter st = case find (\p -> p^.playerHandle == pHandle) $ st^.gamePlayers of
            Just p -> p
            Nothing -> $logicError 'getPlayer "Non-existent handle."
        setter st p' = let
            f p = case p^.playerHandle == pHandle of
                True -> p'
                False -> p
            in st & gamePlayers %~ map f


getMinion :: PlayerHandle -> MinionHandle -> Lens' GameState BoardMinion
getMinion pHandle bmHandle = lens getter setter
    where
        getter st = let
            minions = st^.getPlayer pHandle.playerMinions
            in case find (\bm -> bm^.boardMinionHandle == bmHandle) minions of
                Just bm -> bm
                Nothing -> $logicError 'getMinion "Non-existent handle."
        setter st bm' = let
            f bm = case bm^.boardMinionHandle == bmHandle of
                True -> bm'
                False -> bm
            in st & getPlayer pHandle . playerMinions %~ map f


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


class Controllable a where
    controllerOf :: (HearthMonad m) => a -> Hearth m PlayerHandle


instance Controllable CharacterHandle where
    controllerOf = \case
        Left p -> controllerOf p
        Right m -> controllerOf m


instance Controllable PlayerHandle where
    controllerOf = return


instance Controllable MinionHandle where
    controllerOf handle = logCall 'controllerOf $ do
        players <- view gamePlayers
        let isEq minion = minion^.boardMinionHandle == handle
            players' = flip filter players $ \player -> any isEq $ player^.playerMinions
        case players' of
            [player] -> return $ player^.playerHandle
            _ -> $logicError 'controllerOf "Invalid handle."


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
        receiveDamage (Left handle) $ Damage excess
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


gainManaCrystal :: (HearthMonad m) => CrystalState -> PlayerHandle -> Hearth m ()
gainManaCrystal crystalState handle = logCall 'gainManaCrystal $ zoomPlayer handle $ do
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
    gainManaCrystal CrystalFull handle
    getPlayer handle.playerEmptyManaCrystals .= 0
    getPlayer handle.playerMinions.traversed.boardMinionAttackCount .= 0
    getPlayer handle.playerMinions.traversed.boardMinionNewlySummoned .= False
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
    evolution <- prompt (PromptAction snapshot) >>= liftM Just . enactAction
    clearDeadMinions
    return evolution
    
    
enactAction :: (HearthMonad m) => Action -> Hearth m TurnEvolution
enactAction = logCall 'enactAction . \case
    ActionPlayerConceded _ -> $todo 'pumpTurn' "concede"
    ActionPlayMinion card pos -> actionPlayMinion card pos
    ActionPlaySpell card -> actionPlaySpell card
    ActionAttack attacker defender -> actionAttack attacker defender
    ActionEndTurn -> return EndTurn


isCardInHand :: (HearthMonad m) => PlayerHandle -> HandCard -> Hearth m Bool
isCardInHand handle card = logCall 'isCardInHand $ local id $ removeFromHand handle card


insertAt :: Int -> a -> [a] -> [a]
insertAt n x xs = let
    (left, right) = splitAt n xs
    in left ++ [x] ++ right


placeOnBoard :: (HearthMonad m) => PlayerHandle -> BoardPos -> Minion -> Hearth m (Maybe MinionHandle)
placeOnBoard handle (BoardPos pos) minion = logCall 'placeOnBoard $ do
    minionHandle <- genHandle
    let minion' = BoardMinion {
            _boardMinionDamage = 0,
            _boardMinionEnchantments = [],
            _boardMinionAbilities = minion^.minionAbilities,
            _boardMinionEnrageEnchantments = [],
            _boardMinionAttackCount = 0,
            _boardMinionNewlySummoned = True,
            _boardMinionHandle = minionHandle,
            _boardMinion = minion }
    zoom (getPlayer handle.playerMinions) $ do
        to length >>=. \case
            7 -> return Nothing
            len -> case 0 <= pos && pos <= len of
                False -> return Nothing
                True -> do
                    id %= insertAt pos minion'
                    return $ Just minionHandle


actionAttack :: (HearthMonad m) => CharacterHandle -> CharacterHandle -> Hearth m TurnEvolution
actionAttack attacker defender = logCall 'actionAttack $ do
    _ <- enactAttack attacker defender
    return ContinueTurn


actionPlayMinion :: (HearthMonad m) => HandCard -> BoardPos -> Hearth m TurnEvolution
actionPlayMinion card pos = logCall 'actionPlayMinion $ do
    pHandle <- getActivePlayerHandle
    _ <- playMinion pHandle card pos
    return ContinueTurn


actionPlaySpell :: (HearthMonad m) => HandCard -> Hearth m TurnEvolution
actionPlaySpell card = logCall 'actionPlaySpell $ do
    handle <- getActivePlayerHandle
    _ <- playSpell handle card
    return ContinueTurn


playMinion :: (HearthMonad m) => PlayerHandle -> HandCard -> BoardPos -> Hearth m Result
playMinion pHandle card pos = logCall 'playMinion $ do
    st <- get
    playMinion' pHandle card pos >>= \case
        Nothing -> do
            put st
            return Failure
        Just bmHandle -> do
            prompt $ PromptGameEvent $ PlayedMinion pHandle bmHandle
            enactAnyBattleCries bmHandle
            return Success


playMinion' :: (HearthMonad m) => PlayerHandle -> HandCard -> BoardPos -> Hearth m (Maybe MinionHandle)
playMinion' handle card pos = logCall 'playMinion' $ playCommon handle card >>= \case
    Failure -> return Nothing
    Success -> case card of
        HandCardMinion minion -> placeOnBoard handle pos minion
        _ -> return Nothing


playSpell :: (HearthMonad m) => PlayerHandle -> HandCard -> Hearth m Result
playSpell pHandle card = logCall 'playSpell $ do
    st <- get
    playSpell' pHandle card >>= \case
        Nothing -> do
            put st
            return Failure
        Just sHandle -> do
            prompt $ PromptGameEvent $ PlayedSpell pHandle sHandle
            enactSpell sHandle
            return Success


-- TODO: This should return a SpellHandle instead of a Spell
playSpell' :: (HearthMonad m) => PlayerHandle -> HandCard -> Hearth m (Maybe Spell)
playSpell' handle card = logCall 'playSpell' $ playCommon handle card >>= \case
    Failure -> return Nothing
    Success -> case card of
        HandCardSpell spell -> return $ Just spell
        _ -> return Nothing


-- TODO: This should accept a SpellHandle instead of a Spell
enactSpell :: (HearthMonad m) => Spell -> Hearth m ()
enactSpell spell = let
    f = spell^.spellEffect
    in genHandle >>= enactEffect . f


enactAnyBattleCries :: (HearthMonad m) => MinionHandle -> Hearth m ()
enactAnyBattleCries bmHandle = logCall 'enactAnyBattleCries $ do
    pHandle <- controllerOf bmHandle
    bm <- view $ getMinion pHandle bmHandle
    forM_ (bm^.boardMinionAbilities) $ \case
        KeywordAbility (Battlecry effect) -> enactBattlecry bmHandle effect
        _ -> return ()


enactBattlecry :: (HearthMonad m) => MinionHandle -> (MinionHandle -> Effect) -> Hearth m ()
enactBattlecry handle = logCall 'enactBattlecry . enactEffect . ($ handle)


enactEffect :: (HearthMonad m) => Effect -> Hearth m ()
enactEffect = logCall 'enactEffect . \case
    Elect elect -> enactElect elect
    Sequence effects -> mapM_ enactEffect effects
    DrawCards handle n -> drawCards handle n >> return ()
    KeywordEffect effect -> enactKeywordEffect effect
    DealDamage handle damage -> receiveDamage handle damage
    Enchant handle enchantments -> enchant handle enchantments
    Give handle abilities -> giveAbilities handle abilities
    GainManaCrystal crystalState handle -> gainManaCrystal crystalState handle


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


class (Controllable a) => CharacterTraits a where
    characterHandle :: a -> CharacterHandle
    dynamicAttack :: (HearthMonad m) => a -> Hearth m Attack
    dynamicHealth :: (HearthMonad m) => a -> Hearth m Health
    bumpAttackCount :: (HearthMonad m) => a -> Hearth m ()
    getAttackCount :: (HearthMonad m) => a -> Hearth m Int
    dynamicMaxAttackCount :: (HearthMonad m) => a -> Hearth m Int
    hasSummoningSickness :: (HearthMonad m) => a -> Hearth m Bool


instance CharacterTraits MinionHandle where
    characterHandle = Right
    dynamicAttack bmHandle = logCall 'dynamicAttack $ do
        pHandle <- controllerOf bmHandle
        bm <- view $ getMinion pHandle bmHandle
        let delta = sum $ flip mapMaybe (allEnchantments bm) $ \case
                StatsDelta a _ -> Just a
        return $ bm^.boardMinion.minionAttack + delta
    dynamicHealth bmHandle = logCall 'dynamicHealth $ do
        pHandle <- controllerOf bmHandle
        bm <- view $ getMinion pHandle bmHandle
        let delta = sum $ flip mapMaybe (allEnchantments bm) $ \case
                StatsDelta _ h -> Just h
        return $ bm^.boardMinion.minionHealth + delta
    bumpAttackCount bmHandle = logCall 'bumpAttackCount $ do
        pHandle <- controllerOf bmHandle
        getMinion pHandle bmHandle.boardMinionAttackCount += 1
    getAttackCount bmHandle = logCall 'getAttackCount $ do
        pHandle <- controllerOf bmHandle
        bm <- view $ getMinion pHandle bmHandle
        return $ bm^.boardMinionAttackCount
    dynamicMaxAttackCount _ = logCall 'dynamicMaxAttackCount $ do
        return 1
    hasSummoningSickness bmHandle = logCall 'hasSummoningSickness $ do
        pHandle <- controllerOf bmHandle
        bm <- view $ getMinion pHandle bmHandle
        case bm^.boardMinionNewlySummoned of
            False -> return False
            True -> liftM not $ dynamicHasCharge bmHandle


instance CharacterTraits CharacterHandle where
    characterHandle = id
    dynamicAttack = logCall 'dynamicAttack $ \case
        Left p -> $todo 'dynamicAttack $ show p
        Right bm -> dynamicAttack bm
    dynamicHealth = logCall 'dynamicHealth $ \case
        Left p -> $todo 'dynamicHealth $ show p
        Right bm -> dynamicHealth bm
    bumpAttackCount = logCall 'bumpAttackCount $ \case
        Left p -> $todo 'bumpAttackCount $ show p
        Right bm -> bumpAttackCount bm
    getAttackCount = logCall 'getAttackCount $ \case
        Left p -> $todo 'getAttackCount $ show p
        Right bm -> getAttackCount bm
    dynamicMaxAttackCount = logCall 'dynamicMaxAttackCount $ \case
        Left p -> $todo 'dynamicMaxAttackCount $ show p
        Right bm -> dynamicMaxAttackCount bm
    hasSummoningSickness = logCall 'hasSummoningSickness $ \case
        Left p -> $todo 'hasSummoningSickness $ show p
        Right bm -> hasSummoningSickness bm


receiveDamage :: (HearthMonad m) => CharacterHandle -> Damage -> Hearth m ()
receiveDamage ch damage = logCall 'receiveDamage $ case damage <= 0 of
    True -> return ()
    False -> case ch of
        Left handle -> do
            bh <- view $ getPlayer handle.playerHero
            let dmg = unDamage damage
                armor = bh^.boardHeroArmor
                health = bh^.boardHeroCurrHealth
                armor' = max 0 $ armor - Armor dmg
                armorDelta = unArmor $ armor - armor'
                health' = health - Health (dmg - armorDelta)
            getPlayer handle.playerHero .= bh {
                    _boardHeroCurrHealth = health',
                    _boardHeroArmor = armor' }
            prompt $ PromptGameEvent $ HeroTakesDamage handle health armor damage
        Right handle -> do
            withMinions $ \bm -> do
                liftM Just $ case bm^.boardMinionHandle == handle of
                    False -> return bm
                    True -> case loseDivineShield bm of
                        Just bm' -> do
                            prompt $ PromptGameEvent $ LostDivineShield bm'
                            return bm'
                        Nothing -> do
                            let bm' = bm & boardMinionDamage +~ damage
                            prompt $ PromptGameEvent $ MinionTakesDamage bm damage
                            return bm'
            activateEnrage handle


enactKeywordEffect :: (HearthMonad m) => KeywordEffect -> Hearth m ()
enactKeywordEffect = logCall 'enactKeywordEffect . \case
    Silence handle -> silence handle


silence :: (HearthMonad m) => MinionHandle -> Hearth m ()
silence victim = logCall 'silence $ do
    withMinions $ \bm -> case bm^.boardMinionHandle == victim of
        False -> return $ Just bm
        True -> do
            health <- dynamicHealth victim
            let bm' = bm & boardMinionAbilities .~ [] & boardMinionEnchantments .~ []
                health' = bm^.boardMinion.minionHealth
                delta = Damage $ unHealth $ health' - health
                bm'' = case delta < 0 of
                    True -> bm' & boardMinionDamage %~ max 0 . (+ delta)
                    False -> bm'
            prompt $ PromptGameEvent $ Silenced bm''
            return $ Just bm''


enactElect :: (HearthMonad m) => Elect -> Hearth m ()
enactElect = logCall 'enactElect . \case
    CasterOf _ f -> getActivePlayerHandle >>= enactEffect . f
    OpponentOf _ f -> getNonActivePlayerHandle >>= enactEffect . f
    ControllerOf minionHandle f -> controllerOf minionHandle >>= enactEffect . f
    AnyCharacter selection f -> anyCharacter selection f
    AnyEnemy selection f -> anyEnemy selection f
    AnotherCharacter selection bannedMinion f -> anotherCharacter selection bannedMinion f
    AnotherMinion selection bannedMinion f -> anotherMinion selection bannedMinion f
    AnotherFriendlyMinion selection bannedMinion f -> anotherFriendlyMinion selection bannedMinion f
    OtherCharacters bannedMinion f -> otherCharacters bannedMinion f
    OtherEnemies bannedMinion f -> otherEnemies bannedMinion f


otherEnemies :: (HearthMonad m) => CharacterHandle -> (CharacterHandle -> Effect) -> Hearth m ()
otherEnemies bannedHandle f = logCall 'otherEnemies $ do
    opponentHandle <- getNonActivePlayerHandle
    minionCandidates <- do
        bms <- view $ getPlayer opponentHandle.playerMinions
        let bmHandles = map (\bm -> bm^.boardMinionHandle) bms
        return $ filter (/= bannedHandle) $ map Right bmHandles
    let playerCandidates = filter (/= bannedHandle) [Left opponentHandle]
        candidates = playerCandidates ++ minionCandidates
    forM_ candidates $ enactEffect . f


otherCharacters :: (HearthMonad m) => CharacterHandle -> (CharacterHandle -> Effect) -> Hearth m ()
otherCharacters bannedHandle f = logCall 'otherCharacters $ do
    pHandles <- getPlayerHandles
    minionCandidates <- liftM concat $ forM pHandles $ \pHandle -> do
        bms <- view $ getPlayer pHandle.playerMinions
        let bmHandles = map (\bm -> bm^.boardMinionHandle) bms
        return $ filter (/= bannedHandle) $ map Right bmHandles
    let playerCandidates = filter (/= bannedHandle) $ map Left pHandles
        candidates = playerCandidates ++ minionCandidates
    forM_ candidates $ enactEffect . f


anotherCharacter :: (HearthMonad m) => Selection -> CharacterHandle -> (CharacterHandle -> Effect) -> Hearth m ()
anotherCharacter selection bannedHandle f = logCall 'anotherCharacter $ do
    pHandles <- getPlayerHandles
    minionCandidates <- liftM concat $ forM pHandles $ \pHandle -> do
        bms <- view $ getPlayer pHandle.playerMinions
        let bmHandles = map (\bm -> bm^.boardMinionHandle) bms
        return $ filter (/= bannedHandle) $ map Right bmHandles
    let playerCandidates = filter (/= bannedHandle) $ map Left pHandles
        candidates = playerCandidates ++ minionCandidates
    pickFrom selection candidates >>= \case
        Nothing -> return ()
        Just candidate -> enactEffect $ f candidate


anyEnemy :: (HearthMonad m) => Selection -> (CharacterHandle -> Effect) -> Hearth m ()
anyEnemy selection f = logCall 'anyEnemy $ do
    opponentHandle <- getNonActivePlayerHandle
    minionCandidates <- do
        bms <- view $ getPlayer opponentHandle.playerMinions
        return $ map (\bm -> Right $ bm^.boardMinionHandle) bms
    let playerCandidates = [Left opponentHandle]
        candidates = playerCandidates ++ minionCandidates
    pickFrom selection candidates >>= \case
        Nothing -> return ()
        Just candidate -> enactEffect $ f candidate


anyCharacter :: (HearthMonad m) => Selection -> (CharacterHandle -> Effect) -> Hearth m ()
anyCharacter selection f = logCall 'anyCharacter $ do
    pHandles <- getPlayerHandles
    minionCandidates <- liftM concat $ forM pHandles $ \pHandle -> do
        bms <- view $ getPlayer pHandle.playerMinions
        return $ map (\bm -> Right $ bm^.boardMinionHandle) bms
    let playerCandidates = map Left pHandles
        candidates = playerCandidates ++ minionCandidates
    pickFrom selection candidates >>= \case
        Nothing -> return ()
        Just candidate -> enactEffect $ f candidate


anotherMinion :: (HearthMonad m) => Selection -> MinionHandle -> (MinionHandle -> Effect) -> Hearth m ()
anotherMinion selection bannedHandle f = logCall 'anotherMinion $ do
    handles <- getPlayerHandles
    candidates <- liftM concat $ forM handles $ \handle -> do
        bms <- view $ getPlayer handle.playerMinions
        let bmHandles = map (\bm -> bm^.boardMinionHandle) bms
        return $ filter (/= bannedHandle) bmHandles
    pickFrom selection candidates >>= \case
        Nothing -> return ()
        Just candidate -> enactEffect $ f candidate


anotherFriendlyMinion :: (HearthMonad m) => Selection -> MinionHandle -> (MinionHandle -> Effect) -> Hearth m ()
anotherFriendlyMinion selection bannedHandle f = logCall 'anotherFriendlyMinion $ do
    controller <- controllerOf bannedHandle
    candidates <- do
        bms <- view $ getPlayer controller.playerMinions
        let bmHandles = map (\bm -> bm^.boardMinionHandle) bms
        return $ filter (/= bannedHandle) bmHandles
    pickFrom selection candidates >>= \case
        Nothing -> return ()
        Just candidate -> enactEffect $ f candidate


pickFrom :: (HearthMonad m, Eq a) => Selection -> [a] -> Hearth m (Maybe a)
pickFrom _ = logCall 'pickFrom . \case
    [] -> return Nothing
    xs -> do
        x <- guardedPrompt (PromptPickRandom $ NonEmpty.fromList xs) (`elem` xs)
        return $ Just x


playCommon :: (HearthMonad m) => PlayerHandle -> HandCard -> Hearth m Result
playCommon handle card = logCall 'playCommon $ removeFromHand handle card >>= \case
    False -> return Failure
    True -> payCost handle $ costOf card


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
    threshold <- liftM (Damage . unHealth) $ dynamicHealth $ bm^.boardMinionHandle
    let alive = bm^.boardMinionDamage < threshold
    case alive of
        True -> return $ Just bm
        False -> do
            prompt $ PromptGameEvent $ MinionDied bm
            return Nothing


dynamicHasAbility :: (HearthMonad m) => (Ability -> Bool) -> MinionHandle -> Hearth m Bool
dynamicHasAbility predicate bmHandle = logCall 'dynamicHasAbility $ do
    pHandle <- controllerOf bmHandle
    bm <- view $ getMinion pHandle bmHandle
    return $ any predicate $ bm^.boardMinionAbilities


dynamicHasTaunt :: (HearthMonad m) => MinionHandle -> Hearth m Bool
dynamicHasTaunt = logCall 'dynamicHasTaunt $ dynamicHasAbility $ \case
    KeywordAbility Taunt -> True
    _ -> False


dynamicHasCharge :: (HearthMonad m) => MinionHandle -> Hearth m Bool
dynamicHasCharge = logCall 'dynamicHasCharge $ dynamicHasAbility $ \case
    KeywordAbility Charge -> True
    _ -> False


hasTauntMinions :: (HearthMonad m) => PlayerHandle -> Hearth m Bool
hasTauntMinions player = logCall 'hasTauntMinions $ do
    view (getPlayer player.playerMinions) >>= anyM (dynamicHasTaunt . _boardMinionHandle)


isAlly :: (Controllable a, HearthMonad m) => a -> Hearth m Bool
isAlly bm = do
    controller <- controllerOf bm
    active <- getActivePlayerHandle
    return $ controller == active


isEnemy :: (Controllable a, HearthMonad m) => a -> Hearth m Bool
isEnemy = liftM not . isAlly


hasRemainingAttacks :: (CharacterTraits a, HearthMonad m) => a -> Hearth m Bool
hasRemainingAttacks c = liftM2 (<) (getAttackCount c) (dynamicMaxAttackCount c)


enactAttack :: (HearthMonad m) => CharacterHandle -> CharacterHandle -> Hearth m Result
enactAttack attacker defender = logCall 'enactAttack $ do
    let attackerHandle = characterHandle attacker
        defenderHandle = characterHandle defender
        defenderHasTaunt = case defender of
            Left _ -> return False
            Right m -> dynamicHasTaunt m
    result <- isAlly attacker >>= \case
        False -> return Failure
        True -> isEnemy defender >>= \case
            False -> return Failure
            True -> hasSummoningSickness attacker >>= \case
                True -> return Failure
                False -> hasRemainingAttacks attacker >>= \case
                    False -> return Failure
                    True -> defenderHasTaunt >>= \case
                        True -> return Success
                        False -> do
                            defenderController <- controllerOf defender
                            hasTauntMinions defenderController >>= return . \case
                                True -> Failure
                                False -> Success
    case result of
        Failure -> return Failure
        Success -> do
            prompt $ PromptGameEvent $ EnactAttack attackerHandle defenderHandle
            let x `harms` y = do
                    dmg <- liftM (Damage . unAttack) $ dynamicAttack x
                    receiveDamage (characterHandle y) dmg
            attacker `harms` defender
            defender `harms` attacker
            bumpAttackCount attacker
            return Success


replaceMinionByHandle :: (HearthMonad m) => BoardMinion -> Hearth m ()
replaceMinionByHandle bm' = logCall 'replaceMinionByHandle $ do
    controller <- controllerOf $ bm'^.boardMinionHandle
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





























































