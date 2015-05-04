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
{-# LANGUAGE OverloadedStrings #-}
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
import Control.Monad.Loops
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
import Hearth.Cards
import Hearth.Client.Console.BoardMinionsColumn
import Hearth.Client.Console.HandColumn
import Hearth.Client.Console.PlayerColumn
import Hearth.Client.Console.SGRString
import Hearth.DebugEvent
import Hearth.Engine
import Hearth.GameEvent
import Hearth.Model
import Hearth.Names
import Hearth.Prompt
import Language.Haskell.TH.Syntax (nameBase)
import Prelude hiding (pi, log)
import System.Console.ANSI
import System.Console.Terminal.Size (Window)
import qualified System.Console.Terminal.Size as Window
import Text.Read (readMaybe)


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
        isLight = (`notElem` words "")


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
        in tag "gameBegins" []
    GameEnds gameResult -> let
        gameResultAttr = ("gameResult", show gameResult)
        in tag "gameEnds" [gameResultAttr]
    DeckShuffled (viewPlayer -> who) _ -> let
        playerAttr = ("player", show who)
        in tag "deckShuffled" [playerAttr]
    CardDrawn (viewPlayer -> who) (eCard) _ -> let
        playerAttr = ("player", show who)
        cardAttr = ("card", either deckCardName' handCardName' eCard)
        resultAttr = ("result", show $ either (const Failure) (const Success) eCard)
        in tag "cardDrawn" [playerAttr, cardAttr, resultAttr]
    PlayedCard (viewPlayer -> who) card result -> let
        playerAttr = ("player", show who)
        cardAttr = ("card", handCardName' card)
        resultAttr = ("result", show result)
        in tag "playedCard" [playerAttr, cardAttr, resultAttr]
    HeroTakesDamage (viewPlayer -> who) (Health oldHealth) (Damage damage) -> let
        newHealth = oldHealth - damage
        playerAttr = ("player", show who)
        oldAttr = ("old", show oldHealth)
        newAttr = ("new", show newHealth)
        dmgAttr = ("dmg", show damage)
        in tag "heroTakesDamage" [playerAttr, oldAttr, newAttr, dmgAttr]
    GainsManaCrystal (viewPlayer -> who) mCrystalState -> let
        playerAttr = ("player", show who)
        varietyAttr = ("variety", maybe (nameBase 'Nothing) show mCrystalState)
        in tag "gainsManaCrystal" [playerAttr, varietyAttr]
    ManaCrystalsRefill (viewPlayer -> who) amount -> let
        playerAttr = ("player", show who)
        amountAttr = ("amount", show amount)
        in tag "manaCrystalsRefill" [playerAttr, amountAttr]
    ManaCrystalsEmpty (viewPlayer -> who) amount -> let
        playerAttr = ("player", show who)
        amountAttr = ("amount", show amount)
        in tag "manaCrystalsEmpty" [playerAttr, amountAttr]
    where
        viewPlayer (PlayerHandle (RawHandle who)) = who
        tag name attrs = verbosityGate name $ openTag name attrs >> closeTag name


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
    setSGR [SetColor Foreground Dull White]
    clearScreen
    setCursorPosition 0 0


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
        deck1 = Deck $ take 30 $ cycle cardUniverse
        deck2 = Deck $ take 30 $ cycle $ reverse cardUniverse
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


--data ConsoleAction :: * -> * where
    --QuitAction :: ConsoleAction ()
    --GameAction :: ConsoleAction Action


data PromptInfo m a = PromptInfo {
    _key :: SGRString,
    _desc :: SGRString,
    _action :: [Int] -> m a
}


presentPrompt :: (MonadIO m) => m a -> [PromptInfo m a] -> m a
presentPrompt retry promptInfos = do
    let descs = map (rights . _desc) promptInfos
        descMaxTrailLen = foldl' (+) 0 $ map (length . takeWhile (/= '>')) descs
    response <- liftM (map toLower) $ liftIO $ do
        setSGR [SetColor Foreground Dull White]
        forM_ promptInfos $ \pi -> let
            (innerDesc, Right '>' : outerDesc) = span (/= Right '>') $ _desc pi
            trailLen = descMaxTrailLen - length innerDesc + 1
            trail = fromString $ '>' : replicate trailLen '-'
            key = _key pi
            in putSGRString $ "-<" ++ key ++ innerDesc ++ trail ++ outerDesc ++ "\n"
        putStrLn ""
        putStr "> "
        getLine
    let mPromptInfo = flip find promptInfos $ \pi -> let
            key = map toLower $ rights $ _key pi
            in key `isPrefixOf` response
    case mPromptInfo of
        Nothing -> retry
        Just pi -> let
            massage = \case
                '+' -> ' '
                c -> c
            args = map readMaybe $ words $ map massage $ drop (length $ rights $ _key pi) $ response
            args' = catMaybes args
            in case length args == length args' of
                False -> retry
                True -> _action pi args'


actionPrompts :: Hearth Console Action -> Hearth Console Action -> [PromptInfo (Hearth Console) Action]
actionPrompts quietRetry complainRetry = [
    PromptInfo "?" "> Help" $ helpAction quietRetry,
    PromptInfo "0" "> End Turn" $ endTurnAction complainRetry,
    PromptInfo "1" " H B> Play Card" $ playCardAction complainRetry,
    PromptInfo "2" " M M> Attack Minion" $ attackMinionAction complainRetry,
    PromptInfo "" ">- Autoplay" $ autoplayAction complainRetry ]


helpAction :: Hearth Console Action -> [Int] -> Hearth Console Action
helpAction retry _ = do
    liftIO $ do
        putStrLn ""
        putStrLn "Usage:"
        putStrLn "> COMMAND ARG1 ARG2 ARG3 ..."
        putStrLn "Spaces and pluses are used to delimit arguments."
        putStrLn "Example: Summon minion 4H to board position 3B"
        putStrLn "> 1 4 3"
        putStrLn "> 1+4+3"
        putStrLn ""
        putStrLn "ENTER TO CONTINUE"
        _ <- getLine
        return ()
    retry


autoplayAction :: Hearth Console Action -> [Int] -> Hearth Console Action
autoplayAction retry = \case
    [] -> decideAction activities
    _ -> retry
    where
        decideAction = \case
            [] -> return ActionEndTurn
            m : ms -> m >>= \case
                Nothing -> decideAction ms
                Just x -> return x
        activities = [tryPlayCard, tryAttackMinion]
        tryPlayCard = do
            handle <- getActivePlayerHandle
            cards <- view $ getPlayer handle.playerHand.handCards
            pos <- view $ getPlayer handle.playerMinions.to (BoardPos . length)
            mCard <- flip firstM (reverse cards) $ \card -> playCard handle card pos >>= \case
                Failure -> return False
                Success -> return True
            return $ case mCard of
                Nothing -> Nothing
                Just card -> Just $ ActionPlayCard card pos
        tryAttackMinion = do
            activeHandle <- getActivePlayerHandle
            activeMinions <- view $ getPlayer activeHandle.playerMinions
            nonActiveHandle <- getNonActivePlayerHandle
            nonActiveMinions <- view $ getPlayer nonActiveHandle.playerMinions
            let pairs = [(a, na) | a <- activeMinions, na <- nonActiveMinions]
            mPair <- flip firstM pairs $ \(activeMinion, nonActiveMinion) -> do
                attackMinion activeMinion nonActiveMinion >>= \case
                    Failure -> return False
                    Success -> return True
            return $ case mPair of
                Nothing -> Nothing
                Just (attacker, defender) -> Just $ ActionAttackMinion attacker defender


endTurnAction :: Hearth Console Action -> [Int] -> Hearth Console Action
endTurnAction retry = \case
    [] -> return ActionEndTurn
    _ -> retry


lookupIndex :: [a] -> Int -> Maybe a
lookupIndex (x:xs) n = case n == 0 of
    True -> Just x
    False -> lookupIndex xs (n - 1)
lookupIndex [] _ = Nothing


attackMinionAction :: Hearth Console Action -> [Int] -> Hearth Console Action
attackMinionAction retry = \case
    [attackerIdx, defenderIdx] -> do
        activePlayer <- getActivePlayerHandle
        activeMinions <- view $ getPlayer activePlayer.playerMinions
        nonActivePlayer <- getNonActivePlayerHandle
        nonActiveMinions <- view $ getPlayer nonActivePlayer.playerMinions
        let mAttacker = lookupIndex activeMinions attackerIdx
            mDefender = lookupIndex nonActiveMinions defenderIdx
        case mAttacker of
            Nothing -> retry
            Just attacker -> case mDefender of
                Nothing -> retry
                Just defender -> return $ ActionAttackMinion attacker defender
    _ -> retry


playCardAction :: Hearth Console Action -> [Int] -> Hearth Console Action
playCardAction retry = \case
    [handIdx, boardIdx] -> do
        handle <- getActivePlayerHandle
        cards <- view $ getPlayer handle.playerHand.handCards
        boardLen <- view $ getPlayer handle.playerMinions.to length
        let mCard = lookupIndex cards $ length cards - handIdx
        case mCard of
            Nothing -> retry
            Just card -> case 0 < boardIdx && boardIdx <= boardLen + 1 of
                False -> retry
                True -> return $ ActionPlayCard card $ BoardPos $ boardIdx - 1
    _ -> retry


getAction :: GameSnapshot -> Console Action
getAction snapshot = do
    let name = showName 'getAction
    verbosityGate name $ openTag name []
    action <- getAction' snapshot
    verbosityGate name $ closeTag name
    return action
    where
        showName = (':' :) . nameBase


getAction' :: GameSnapshot -> Console Action
getAction' snapshot = do
    let go complain = do
            renewDisplay
            case complain of
                True -> do
                    liftIO $ do
                        setSGR [SetColor Foreground Dull White]
                        putStrLn "** UNKNOWN COMMAND **"
                        putStrLn ""
                    helpAction (go False) []
                False -> presentPrompt (go True) $ actionPrompts (go False) (go True)
    action <- localQuiet $ runQuery snapshot $ go False
    logState.undisplayedLines .= 0
    return action


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
            True -> sgrColor (Dull, White) ++ "*" ++ sgrColor (Dull, Cyan)
            False -> ""
        player = playerColumn p
        hand = handColumn $ p^.playerHand
        boardMinions = boardMinionsColumn $ p^.playerMinions
        (wx, wy, wz) = (15, 30, 30) :: (Int, Int, Int)
        width = Window.width window
        (deckLoc, handLoc, minionsLoc) = case who of
                Alice -> (0, wx, wx + wy)
                Bob -> (width - wx, width - wx - wy, width - wx - wy - wz)
    liftIO $ do
        n0 <- printColumn True (take (wx - 1) playerName) deckLoc player
        n1 <- printColumn True "HAND" handLoc hand
        n2 <- printColumn False "   MINIONS" minionsLoc boardMinions
        return $ maximum [n0, n1, n2]


printColumn :: Bool -> SGRString -> Int -> [SGRString] -> IO Int
printColumn extraLine label column strs = do
    let label' = filter (/= '*') $ rights label
        strs' = [
            label,
            fromString $ replicate (length $ takeWhile isSpace label') ' ' ++ replicate (length $ dropWhile isSpace label') '-'
            ] ++ (if extraLine then [""] else []) ++ strs
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

















