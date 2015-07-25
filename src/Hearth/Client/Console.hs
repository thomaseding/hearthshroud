{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}


module Hearth.Client.Console (
    main
) where


--------------------------------------------------------------------------------


import Control.Applicative
import Control.Error
import Control.Exception hiding (handle)
import Control.Lens
import Control.Lens.Helper
import Control.Lens.Internal.Zoom (Zoomed, Focusing)
import Control.Monad.Prompt
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.State.Local
import Data.Char
import Data.Either
import Data.List
import Data.Maybe
import Data.NonEmpty
import Data.String
import Hearth.Action
import qualified Hearth.Cards as Universe
import Hearth.Client.Console.Render.BoardMinionsColumn
import Hearth.Client.Console.Render.HandColumn
import Hearth.Client.Console.Render.PlayerColumn
import Hearth.Client.Console.SGRString
import Hearth.DebugEvent
import Hearth.Engine
import Hearth.GameEvent
import Hearth.Model
import Hearth.Names
import Hearth.Names.Basic (BasicCardName(TheCoin))
import Hearth.Names.Hero
import Hearth.Prompt
import Hearth.ShowCard
import Language.Haskell.TH.Syntax (nameBase)
import Prelude hiding (pi, log)
import System.Console.ANSI
import System.Console.Terminal.Size (Window)
import qualified System.Console.Terminal.Size as Window
import System.Random.Shuffle
import Text.LambdaOptions.Core
import Text.LambdaOptions.Keyword
import Text.LambdaOptions.List


--------------------------------------------------------------------------------


defaultVerbosity :: Verbosity
defaultVerbosity = DebugLight


data Verbosity
    = Quiet
    | GameEventsOnly
    | DebugLight
    | DebugExhaustive
    deriving (Show, Eq, Ord)


data LogState = LogState {
    _loggedLines :: [String],
    _totalLines :: !Int,
    _undisplayedLines :: !Int,
    _tagDepth :: !Int,
    _useShortTag :: !Bool,
    _verbosity :: !Verbosity
} deriving (Show, Eq, Ord)
makeLenses ''LogState


data ConsoleState = ConsoleState {
    _logState :: LogState
} deriving (Show, Eq, Ord)
makeLenses ''ConsoleState


newtype Console' st a = Console {
    unConsole :: StateT st IO a
} deriving (Functor, Applicative, Monad, MonadIO, MonadState st)


instance MonadReader st (Console' st) where
    ask = get
    local = stateLocal


type Console = Console' ConsoleState


type instance Zoomed (Console' st) = Focusing IO


instance Zoom (Console' st) (Console' st') st st' where
    zoom l = Console . zoom l . unConsole


localQuiet :: Console a -> Console a
localQuiet m = do
    v <- view $ logState.verbosity
    logState.verbosity .= Quiet
    x <- m
    logState.verbosity .= v
    return x


newLogLine :: Console ()
newLogLine = zoom logState $ do
    totalLines += 1
    undisplayedLines += 1
    loggedLines %= ("" :)


appendLogLine :: String -> Console ()
appendLogLine str = logState.loggedLines %= \case
    s : ss -> (s ++ str) : ss
    [] -> $logicError 'appendLogLine "Bad state"


logIndentation :: Console String
logIndentation = do
    n <- view $ logState.tagDepth
    return $ concat $ replicate n "    "


openTag :: String -> [(String, String)] -> Console ()
openTag name attrs = do
    logState.useShortTag >>=. \case
        True -> do
            appendLogLine ">"
            newLogLine
        False -> return ()
    logState.useShortTag .= True
    lead <- logIndentation
    appendLogLine $ lead ++ "<" ++ unwords (name : attrs')
    logState.tagDepth += 1
    where
        showAttr (k, v) = k ++ "=\"" ++ v ++ "\""
        attrs' = map showAttr $ filter (/= ("", "")) attrs


closeTag :: String -> Console ()
closeTag name = do
    logState.tagDepth -= 1
    logState.useShortTag >>=. \case
        True -> do
            appendLogLine "/>"
            newLogLine
        False -> do
            lead <- logIndentation
            appendLogLine $ lead ++ "</" ++ name ++ ">"
            newLogLine
    logState.useShortTag .= False


verbosityGate :: String -> Console () -> Console ()
verbosityGate name m = do
    view (logState.verbosity) >>= \case
        Quiet -> return ()
        GameEventsOnly -> case name of
            ':' : _ -> return ()
            _ -> m
        DebugLight -> case name of
            ':' : rest -> case isLight rest of
                True -> m
                False -> return ()
            _ -> m
        DebugExhaustive -> m
    where
        isLight = (`notElem` lightBanList)
        lightBanList = map nameBase [
            'dynamicAttack,
            'dynamicHealth,
            'getActivePlayerHandle,
            'getControllerOf,
            'withMinions ]


debugEvent :: DebugEvent -> Console ()
debugEvent e = case e of
    FunctionEntered name -> let
        name' = showName name
        in verbosityGate name' $ openTag name' []
    FunctionExited name -> let
        name' = showName name
        in verbosityGate name' $ closeTag name'
    where
        showName = (':' :) . nameBase


gameEvent :: GameEvent -> Console ()
gameEvent = \case
    GameBegins -> let
        in tag 'GameBegins []
    GameEnds gameResult -> let
        gameResultAttr = ("gameResult", show gameResult)
        in tag 'GameEnds [gameResultAttr]
    DeckShuffled (viewPlayer -> who) _ -> let
        playerAttr = ("player", show who)
        in tag 'DeckShuffled [playerAttr]
    CardDrawn (viewPlayer -> who) (eCard) _ -> let
        playerAttr = ("player", show who)
        cardAttr = ("card", either deckCardName' handCardName' eCard)
        resultAttr = ("result", show $ either (const Failure) (const Success) eCard)
        in tag 'CardDrawn [playerAttr, cardAttr, resultAttr]
    PlayedCard (viewPlayer -> who) card result -> let
        playerAttr = ("player", show who)
        cardAttr = ("card", handCardName' card)
        resultAttr = ("result", show result)
        in tag 'PlayedCard [playerAttr, cardAttr, resultAttr]
    HeroTakesDamage (viewPlayer -> who) (Health oldHealth) (Armor oldArmor) (Damage damage) -> let
        newArmor = max 0 $ oldArmor - damage
        armorDelta = oldArmor - newArmor
        newHealth = oldHealth - (damage - armorDelta)
        playerAttr = ("player", show who)
        healthAttr = ("health", show oldHealth ++ "|" ++ show newHealth)
        armorAttr = ("armor", show oldArmor ++ "|" ++ show newArmor)
        in tag 'HeroTakesDamage [playerAttr, healthAttr, armorAttr]
    MinionTakesDamage bm (Damage damage) -> let
        minionAttr = ("minion", showCardName $ bm^.boardMinion.minionName)
        dmgAttr = ("dmg", show damage)
        in tag 'MinionTakesDamage [minionAttr, dmgAttr]
    MinionDied bm -> let
        minionAttr = ("minion", showCardName $ bm^.boardMinion.minionName)
        in tag 'MinionDied [minionAttr]
    AttackMinion attacker defender -> let
        attackerAttr = ("attacker", showCardName $ attacker^.boardMinion.minionName)
        defenderAttr = ("defender", showCardName $ defender^.boardMinion.minionName)
        in tag 'AttackMinion [attackerAttr, defenderAttr]
    GainsManaCrystal (viewPlayer -> who) mCrystalState -> let
        playerAttr = ("player", show who)
        varietyAttr = ("variety", maybe (nameBase 'Nothing) show mCrystalState)
        in tag 'GainsManaCrystal [playerAttr, varietyAttr]
    ManaCrystalsRefill (viewPlayer -> who) amount -> let
        playerAttr = ("player", show who)
        amountAttr = ("amount", show amount)
        in tag 'ManaCrystalsRefill [playerAttr, amountAttr]
    ManaCrystalsEmpty (viewPlayer -> who) amount -> let
        playerAttr = ("player", show who)
        amountAttr = ("amount", show amount)
        in tag 'ManaCrystalsEmpty [playerAttr, amountAttr]
    LostDivineShield bm -> let
        minionAttr = ("minion", showCardName $ bm^.boardMinion.minionName)
        in tag 'LostDivineShield [minionAttr]
    EnrageActivated bm -> let
        minionAttr = ("minion", showCardName $ bm^.boardMinion.minionName)
        in tag 'EnrageActivated [minionAttr]
    Silenced bm -> let
        minionAttr = ("minion", showCardName $ bm^.boardMinion.minionName)
        in tag 'Silenced [minionAttr]
    where
        viewPlayer (PlayerHandle (RawHandle who)) = who
        tag name attrs = let
            name' = case nameBase name of
                (c : cs) -> toLower c : cs
                "" -> ""
            in verbosityGate name' $ openTag name' attrs >> closeTag name'


showCardName :: CardName -> String
showCardName = \case
    BasicCardName name -> show name
    ClassicCardName name -> show name


handCardName' :: HandCard -> String
handCardName' = showCardName . handCardName


deckCardName' :: DeckCard -> String
deckCardName' = showCardName . deckCardName


instance MonadPrompt HearthPrompt Console where
    prompt = \case
        PromptDebugEvent e -> debugEvent e
        PromptGameEvent e -> gameEvent e
        PromptAction snapshot -> getAction snapshot
        PromptShuffle xs -> return xs
        PromptPickRandom (NonEmpty x _) -> return x
        PromptMulligan _ xs -> return xs


main :: IO ()
main = finally runTestGame $ do
    setSGR [SetColor Background Dull Black]
    setSGR [SetColor Foreground Dull White]


runTestGame :: IO ()
runTestGame = flip evalStateT st $ unConsole $ do
    _ <- runHearth (player1, player2)
    liftIO clearScreen
    window <- liftIO getWindowSize
    renewLogWindow window 0
    _ <- liftIO getLine
    return ()
    where
        st = ConsoleState {
            _logState = LogState {
                _loggedLines = [""],
                _totalLines = 1,
                _undisplayedLines = 1,
                _tagDepth = 0,
                _useShortTag = False,
                _verbosity = defaultVerbosity } }
        power = HeroPower {
            _heroPowerCost = ManaCost 0,
            _heroPowerEffects = [] }
        hero name = Hero {
            _heroAttack = 0,
            _heroHealth = 30,
            _heroPower = power,
            _heroName = BasicHeroName name }
        cards = filter ((/= BasicCardName TheCoin) . deckCardName) Universe.cards
        deck1 = Deck $ take 30 $ cycle cards
        deck2 = Deck $ take 30 $ cycle $ reverse cards
        player1 = PlayerData (hero Thrall) deck1
        player2 = PlayerData (hero Rexxar) deck2


data Who = Alice | Bob
    deriving (Show, Eq, Ord)


getWindowSize :: IO (Window Int)
getWindowSize = Window.size >>= \case
    Just w -> return $ w { Window.width = Window.width w - 1 }
    Nothing -> $runtimeError 'getWindowSize "Could not get window size."


renewDisplay :: Hearth Console ()
renewDisplay = do
    ps <- mapM (view . getPlayer) =<< getPlayerHandles
    window <- liftIO getWindowSize
    deepestPlayer <- do
        liftIO $ do
            clearScreen
            setSGR [SetColor Foreground Dull White]
        foldM (\n -> liftM (max n) . uncurry (printPlayer window)) 0 (zip ps [Alice, Bob])
    lift $ do
        renewLogWindow window $ deepestPlayer + 1


presentPrompt :: (MonadIO m) => String -> ([String] -> m a) -> m a
presentPrompt promptMessage responseParser = do
    response <- liftM (map toLower) $ liftIO $ do
        setSGR [SetColor Foreground Dull White]
        putStrLn promptMessage
        putStrLn ""
        putStr "> "
        getLine
    let tokenizeResponse = words . map (\case { '+' -> ' '; c -> c })
    responseParser $ case tokenizeResponse response of
        [] -> [""]
        args -> args


parseActionResponse :: [String] -> Hearth Console Action
parseActionResponse response = case runOptions actionOptions response of
    Left (ParseFailed err _ _) -> do
        liftIO $ putStrLn err
        process ComplainRetryAction
    Right ms -> case ms of
        [m] -> m >>= process
        _ -> process ComplainRetryAction
    where
        process = \case
            QuitAction -> $todo 'parseActionResponse "QuitAction"
            GameAction action -> return action
            QuietRetryAction -> getAction'
            ComplainRetryAction -> helpAction >>= process


data ConsoleAction :: * where
    QuitAction :: ConsoleAction
    GameAction :: Action -> ConsoleAction
    QuietRetryAction :: ConsoleAction
    ComplainRetryAction :: ConsoleAction


actionOptions :: Options (Hearth Console) ConsoleAction ()
actionOptions = do
    addOption (kw "?" `text` "Display detailed help text.") $
        helpAction
    addOption (kw "0" `text` "Ends the active player's turn.")
        endTurnAction
    addOption (kw "1" `argText` "H B" `text` "Plays the card.")
        playCardAction
    addOption (kw "2" `argText` "M M" `text` "Attack minion.")
        attackMinionAction
    addOption (kw "9" `argText` "H" `text` "Read card in hand.")
        readCardInHandAction
    addOption (kw "" `text` "Autoplay.")
        autoplayAction


helpAction :: Hearth Console ConsoleAction
helpAction = do
    liftIO $ do
        putStrLn ""
        putStrLn "Usage:"
        putStrLn "> COMMAND ARG1 ARG2 ARG3 ..."
        putStrLn "Spaces and pluses are used to delimit arguments."
        putStrLn "Example: Summon minion 4H to board position 3B"
        putStrLn "> 1 4 3"
        putStrLn "> 1+4+3"
        putStrLn ""
        putStrLn formattedActionOptions
        putStrLn ""
        putStrLn "ENTER TO CONTINUE"
        _ <- getLine
        return ()
    return QuietRetryAction
    where


formattedActionOptions :: String
formattedActionOptions = let
    ks = getKeywords actionOptions
    kwLen k = length $ (concat $ args k : kwNames k)
    maxKwLen = maximum $ map kwLen ks
    args k = case kwArgText k of
        [] -> ""
        str -> ' ' : str
    format k = let
        name = concat $ kwNames k
        pad = replicate (maxKwLen - kwLen k) '-'
        in "-<" ++ name ++ args k ++ ">-" ++ pad ++ " " ++ kwText k
    in intercalate "\n" $ map format ks


pickRandom :: [a] -> IO (Maybe a)
pickRandom = liftM listToMaybe . shuffleM


autoplayAction :: Hearth Console ConsoleAction
autoplayAction = liftIO (shuffleM activities) >>= decideAction
    where
        decideAction = \case
            [] -> endTurnAction
            m : ms -> m >>= \case
                Nothing -> decideAction ms
                Just action -> action
        activities = [tryPlayMinion, tryPlaySpell, tryAttackMinion]
        tryPlayMinion = do
            handle <- getActivePlayerHandle
            cards <- view $ getPlayer handle.playerHand.handCards
            maxPos <- view $ getPlayer handle.playerMinions.to (BoardPos . length)
            let positions = [BoardPos 0 .. maxPos]
            pos <- liftM head $ liftIO $ shuffleM positions
            allowedCards <- flip filterM (reverse cards) $ \card -> local id (playMinion handle card pos) >>= \case
                Failure -> return False
                Success -> return True
            liftIO (pickRandom allowedCards) >>= return . \case
                Nothing -> Nothing
                Just card -> Just $ return $ GameAction $ ActionPlayMinion card pos
        tryPlaySpell = do
            handle <- getActivePlayerHandle
            cards <- view $ getPlayer handle.playerHand.handCards
            allowedCards <- flip filterM (reverse cards) $ \card -> local id (playSpell handle card) >>= \case
                Failure -> return False
                Success -> return True
            liftIO (pickRandom allowedCards) >>= return . \case
                Nothing -> Nothing
                Just card -> Just $ return $ GameAction $ ActionPlaySpell card
        tryAttackMinion = do
            activeHandle <- getActivePlayerHandle
            activeMinions <- view $ getPlayer activeHandle.playerMinions
            nonActiveHandle <- getNonActivePlayerHandle
            nonActiveMinions <- view $ getPlayer nonActiveHandle.playerMinions
            let pairs = [(a, na) | a <- activeMinions, na <- nonActiveMinions]
            allowedPairs <- flip filterM pairs $ \(activeMinion, nonActiveMinion) -> do
                local id $ attackMinion activeMinion nonActiveMinion >>= \case
                    Failure -> return False
                    Success -> return True
            liftIO (pickRandom allowedPairs) >>= return . \case
                Nothing -> Nothing
                Just (attacker, defender) -> Just $ return $ GameAction $ ActionAttackMinion attacker defender


endTurnAction :: Hearth Console ConsoleAction
endTurnAction = return $ GameAction ActionEndTurn


lookupIndex :: [a] -> Int -> Maybe a
lookupIndex (x:xs) n = case n == 0 of
    True -> Just x
    False -> lookupIndex xs (n - 1)
lookupIndex [] _ = Nothing


readCardInHandAction :: Int -> Hearth Console ConsoleAction
readCardInHandAction handIdx = do
    handle <- getActivePlayerHandle
    cards <- view $ getPlayer handle.playerHand.handCards
    let mCard = lookupIndex cards $ length cards - handIdx
    case mCard of
        Nothing -> return ComplainRetryAction
        Just card -> do
            liftIO $ do
                putStrLn $ showCard card
                putStrLn "ENTER TO CONTINUE"
                _ <- getLine
                return QuietRetryAction


attackMinionAction :: Int -> Int -> Hearth Console ConsoleAction
attackMinionAction attackerIdx defenderIdx = do
    activePlayer <- getActivePlayerHandle
    activeMinions <- view $ getPlayer activePlayer.playerMinions
    nonActivePlayer <- getNonActivePlayerHandle
    nonActiveMinions <- view $ getPlayer nonActivePlayer.playerMinions
    let mAttacker = lookupIndex activeMinions $ attackerIdx - 1
        mDefender = lookupIndex nonActiveMinions $ defenderIdx - 1
    case mAttacker of
        Nothing -> return ComplainRetryAction
        Just attacker -> case mDefender of
            Nothing -> return ComplainRetryAction
            Just defender -> return $ GameAction $ ActionAttackMinion attacker defender


playCardAction :: List Int -> Hearth Console ConsoleAction
playCardAction = let
    go handIdx f = do
        handle <- getActivePlayerHandle
        cards <- view $ getPlayer handle.playerHand.handCards
        let mCard = lookupIndex cards $ length cards - handIdx
        case mCard of
            Nothing -> return ComplainRetryAction
            Just card -> f card
    goMinion boardIdx card = do
        handle <- getActivePlayerHandle
        boardLen <- view $ getPlayer handle.playerMinions.to length
        case 0 < boardIdx && boardIdx <= boardLen + 1 of
            False -> return ComplainRetryAction
            True -> return $ GameAction $ ActionPlayMinion card $ BoardPos $ boardIdx - 1
    goSpell s = return $ GameAction $ ActionPlaySpell s
    in \case
        List [handIdx, boardIdx] -> go handIdx $ goMinion boardIdx
        List [handIdx] -> go handIdx goSpell
        _ -> return ComplainRetryAction


getAction :: GameSnapshot -> Console Action
getAction snapshot = do
    let name = showName 'getAction
    verbosityGate name $ openTag name []
    action <- localQuiet $ runQuery snapshot getAction'
    logState.undisplayedLines .= 0
    verbosityGate name $ closeTag name
    return action
    where
        showName = (':' :) . nameBase


getAction' :: Hearth Console Action
getAction' = do
    renewDisplay
    presentPrompt formattedActionOptions parseActionResponse


viewLogLineInfo :: Console (Int,  Int, [String])
viewLogLineInfo = zoom logState $ do
    tl <- view totalLines
    ul <- view undisplayedLines
    strs <- view loggedLines
    return $ case strs of
        "" : rest -> (tl - 1, ul, rest)
        _ -> (tl, ul, strs)


renewLogWindow :: Window Int -> Int -> Console ()
renewLogWindow window row = do
    let displayCount = Window.height window - 20 - row
    (totalCount, newCount, log) <- viewLogLineInfo
    let (newLines, oldLines) = id
            . splitAt (if totalCount < displayCount then min displayCount newCount + displayCount - totalCount else newCount)
            . zip (iterate pred $ max displayCount totalCount)
            . (replicate (displayCount - totalCount) "" ++)
            . take displayCount
            $ log
        lineNoLen = length $ show totalCount
        padWith c str = replicate (lineNoLen - length str) c ++ str
        putWithLineNo debugConfig gameConfig (lineNo, str) = do
            let config = case isDebug str of
                    True -> debugConfig
                    False -> gameConfig
                (intensity, color) = config
                lineNoStr = case totalCount < displayCount && null str of
                    True -> reverse $ padWith ' ' "~"
                    False -> padWith '0' $ show lineNo
            setSGR [SetColor Background Dull Blue, SetColor Foreground Vivid Black ]
            putStr $ lineNoStr
            setSGR [SetColor Background Dull Black]
            setSGR [SetColor Foreground intensity color]
            putStrLn $ " " ++ str ++ case reverse str of
                "" -> ""
                '>' : _ -> ""
                _ -> ">"
    liftIO $ do
        setSGR [SetColor Foreground (fst borderColor) (snd borderColor)]
        setCursorPosition row 0
        putStrLn $ replicate (Window.width window) '-'
        mapM_ (putWithLineNo oldDebugColor oldGameColor) $ reverse oldLines
        mapM_ (putWithLineNo newDebugColor newGameColor) $ reverse newLines
        setSGR [SetColor Foreground (fst borderColor) (snd borderColor)]
        putStrLn $ replicate (Window.width window) '-'
        putStrLn ""
    where
        isDebug s = any (`isInfixOf` s) ["<:", "</:"]
        borderColor = (Dull, Cyan)
        oldDebugColor = (Dull, Magenta)
        oldGameColor = (Vivid, Magenta)
        newDebugColor = (Dull, Cyan)
        newGameColor = (Dull, Green)


printPlayer :: Window Int -> Player -> Who -> Hearth Console Int
printPlayer window p who = do
    liftIO $ setSGR [SetColor Foreground Vivid Green]
    isActive <- liftM (p^.playerHandle ==) getActivePlayerHandle
    let playerName = fromString (map toUpper $ show who) ++ case isActive of
            True -> sgrColor (Dull, White) ++ fromString "*" ++ sgrColor (Dull, Cyan)
            False -> fromString ""
        (wx, wy, wz) = (15, 30, 30) :: (Int, Int, Int)
        width = Window.width window
        (deckLoc, handLoc, minionsLoc) = case who of
                Alice -> (0, wx, wx + wy)
                Bob -> (width - wx, width - wx - wy, width - wx - wy - wz)
    player <- playerColumn p
    hand <- handColumn $ p^.playerHand
    boardMinions <- boardMinionsColumn $ p^.playerMinions
    liftIO $ do
        n0 <- printColumn True (take (wx - 1) playerName) deckLoc player
        n1 <- printColumn True (fromString "HAND") handLoc hand
        n2 <- printColumn False (fromString "   MINIONS") minionsLoc boardMinions
        return $ maximum [n0, n1, n2]


printColumn :: Bool -> SGRString -> Int -> [SGRString] -> IO Int
printColumn extraLine label column strs = do
    let label' = filter (/= '*') $ rights label
        strs' = [
            label,
            fromString $ replicate (length $ takeWhile isSpace label') ' ' ++ replicate (length $ dropWhile isSpace label') '-'
            ] ++ (if extraLine then [fromString ""] else []) ++ strs
    zipWithM_ f [0..] strs'
    return $ length strs'
    where
        f row str = do
            setSGR [SetColor Foreground Dull Cyan]
            setCursorPosition row column
            putSGRString str


putSGRString :: SGRString -> IO ()
putSGRString = mapM_ $ \case
    Left s -> setSGR [s]
    Right c -> putChar c

















