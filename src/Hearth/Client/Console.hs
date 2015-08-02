{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
import Data.Data
import Data.Either
import Data.Function
import Data.List
import Data.Maybe
import Data.NonEmpty
import Data.Ord
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
import System.Environment
import System.Random
import System.Random.Shuffle
import Text.LambdaOptions.Core
import Text.LambdaOptions.Keyword
import Text.LambdaOptions.List
import Text.LambdaOptions.Parseable
import Text.Read (readMaybe)


--------------------------------------------------------------------------------


defaultVerbosity :: Verbosity
defaultVerbosity = GameEventsOnly


data Verbosity
    = Quiet
    | GameEventsOnly
    | DebugLight
    | DebugExhaustive
    deriving (Show, Read, Eq, Ord, Enum, Data, Typeable)


instance Parseable Verbosity where
    parse = simpleParse readMaybe


data Sign = Positive | Negative
    deriving (Show, Eq, Ord, Data, Typeable)


data SignedInt = SignedInt Sign Int -- The Int part should never be negative.
    deriving (Show, Eq, Ord, Data, Typeable)


instance Parseable SignedInt where
    parse = simpleParse $ \case
        s : c : cs -> case isDigit c of
            False -> Nothing
            True -> let
                f = case s of
                    '+' -> fmap $ SignedInt Positive
                    '-' -> fmap $ SignedInt Negative
                    _ -> const Nothing
                in f $ readMaybe $ c : cs
        _ -> Nothing


data NonParseable = NonParseable
    deriving (Data, Typeable)


instance Parseable NonParseable where
    parse _ = (Nothing, 0)


nonParseable :: (Monad m) => NonParseable -> m a
nonParseable _ = $logicError 'nonParseable "This function should not be called."


data TargetType = TargetMinion | TargetPlayer | TargetCharacter
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
    _gameSeed :: Maybe Int,
    _runGame :: Bool,
    _pendingTargets :: [SignedInt],
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
            'dynamicMaxHealth,
            'getActivePlayerHandle,
            'controllerOf,
            'withMinions ]


debugEvent :: DebugEvent -> Console ()
debugEvent e = case e of
    DiagnosticMessage message -> let
        name = showName 'DiagnosticMessage
        in verbosityGate name $ do
            openTag name [("message", message)]
            closeTag name
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
    PlayedMinion (viewPlayer -> who) bmHandle -> let
        playerAttr = ("player", show who)
        cardAttr = ("handle", show bmHandle)
        in tag 'PlayedMinion [playerAttr, cardAttr]
    PlayedSpell (viewPlayer -> who) card -> let
        playerAttr = ("player", show who)
        cardAttr = ("card", handCardName' $ HandCardSpell card)
        in tag 'PlayedSpell [playerAttr, cardAttr]
    HeroTakesDamage (viewPlayer -> who) (Damage damage) -> let
        playerAttr = ("player", show who)
        damageAttr = ("damage", show damage)
        in tag 'HeroTakesDamage [playerAttr, damageAttr]
    MinionTakesDamage bm (Damage damage) -> let
        minionAttr = ("minion", showCardName $ bm^.boardMinion.minionName)
        dmgAttr = ("dmg", show damage)
        in tag 'MinionTakesDamage [minionAttr, dmgAttr]
    MinionDied bm -> let
        minionAttr = ("minion", showCardName $ bm^.boardMinion.minionName)
        in tag 'MinionDied [minionAttr]
    EnactAttack attacker defender -> let
        attackerAttr = ("attacker", show attacker)
        defenderAttr = ("defender", show defender)
        in tag 'EnactAttack [attackerAttr, defenderAttr]
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
    Silenced bm -> let
        minionAttr = ("minion", show bm)
        in tag 'Silenced [minionAttr]
    AttackFailed reason -> let
        reasonAttr = ("reason", show reason)
        in tag 'AttackFailed [reasonAttr]
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
        PromptPickMinion snapshot selection xs -> pickMinion snapshot selection xs
        PromptPickPlayer snapshot selection xs -> pickPlayer snapshot selection xs
        PromptPickCharacter snapshot selection xs -> pickCharacter snapshot selection xs
        PromptMulligan _ xs -> return xs


mkPick :: (Eq a) => (SignedInt -> Hearth Console (Maybe a)) -> GameSnapshot -> Selection -> NonEmpty a -> Console a
mkPick fromSignedInt snapshot selection candidates = case selection of
    AtRandom -> pickRandom candidates
    Targeted' -> view pendingTargets >>= \case
        [] -> escape
        pendingTarget : rest -> do
            pendingTargets .= rest
            mTarget <- runQuery snapshot $ fromSignedInt pendingTarget
            case mTarget of
                Nothing -> escape
                Just target -> case target `elem` toList candidates of
                    False -> escape
                    True -> return target
    where
        escape = $todo 'mkPick "Need to use a continuation to escape to before card is played."


pickMinion :: GameSnapshot -> Selection -> NonEmpty MinionHandle -> Console MinionHandle
pickMinion = mkPick fetchMinionHandle


pickPlayer :: GameSnapshot -> Selection -> NonEmpty PlayerHandle -> Console PlayerHandle
pickPlayer = mkPick fetchPlayerHandle


pickCharacter :: GameSnapshot -> Selection -> NonEmpty CharacterHandle -> Console CharacterHandle
pickCharacter = mkPick fetchCharacterHandle


enums :: (Enum a) => [a]
enums = enumFrom $ toEnum 0


data Conjunction = And | Or


itemize :: (Show a) => Conjunction -> [a] -> String
itemize conj = \case
    [] -> ""
    [x] -> show x
    [x, y] -> show x ++ " and " ++ show y
    [x, y, z] -> show x ++ ", " ++ show y ++ ", " ++ conjStr ++ " " ++ show z
    x : rest -> show x ++ ", " ++ itemize conj rest
    where
        conjStr = case conj of
            And -> "and"
            Or -> "or"


programHelp :: HelpDescription -> Console ()
programHelp (HelpDescription desc) = do
    runGame .= False
    liftIO $ do
        let desc' = "\n HearthShroud\n\n" ++ desc
        putStrLn desc'
        cursorUpLine $ 1 + length (lines desc')
        printBanner 75


consoleOptions :: Options Console () ()
consoleOptions = do
    addOption (kw ["-h", "--help"] `text` "Display this help text.")
        programHelp
    addOption (kw ["--verbosity"] `argText` "VERBOSITY" `text` ("One of: " ++ itemize Or (enums :: [Verbosity])))
        (logState.verbosity .=)
    addOption (kw "--seed" `argText` "INT" `text` "Sets the random number seed to INT.")
        ((gameSeed .=) . Just)


main :: IO ()
main = finally runTestGame $ do
    setSGR [SetColor Background Dull Black]
    setSGR [SetColor Foreground Dull White]


runTestGame :: IO ()
runTestGame = flip evalStateT st $ unConsole $ do
    args <- liftIO getArgs
    case runOptions consoleOptions args of
        Left (ParseFailed msg _ _) -> liftIO $ do
            putStrLn msg
            putStrLn $ getHelpDescription consoleOptions
        Right ms -> sequence_ ms >> view runGame >>= \case
            False -> return ()
            True -> do
                view gameSeed >>= \case
                    Nothing -> return ()
                    Just n -> liftIO $ setStdGen $ mkStdGen n
                _ <- runHearth (player1, player2)
                liftIO clearScreen
                window <- liftIO getWindowSize
                renewLogWindow window 0
                liftIO enterToContinue
    where
        st = ConsoleState {
            _gameSeed = Nothing,
            _runGame = True,
            _pendingTargets = [],
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
        liftIO $ do
            cursorUpLine $ 1 + length (lines promptMessage)
            printBanner 75
        putStrLn ""
        putStr "> "
        getLine
    let tokenizeResponse = words
            . concatMap (\case
                '+' -> " +"
                '-' -> " -"
                '.' -> "?"
                c -> [c] )
            . map toUpper
            . filter (not . isSpace)
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
            QuitAction -> liftM ActionPlayerConceded getActivePlayerHandle
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
    addOption (kw "" `argText` "" `text` "Autoplay.")
        autoplayAction
    addOption (kw "Q" `text` "Concede and quit.")
        quitAction
    addOption (kw "0" `text` "Ends the active player's turn.")
        endTurnAction
    addOption (kw "1" `argText` "MINION POS" `text` "Plays MINION from your hand to board POS.")
        nonParseable
    addOption (kw "1" `argText` "SPELL" `text` "Plays SPELL from your hand.")
        playCardAction
    addOption (kw "2" `argText` "ATTACKER DEFENDER" `text` "Attack DEFENDER with ATTACKER.")
        attackAction
    addOption (kw "?" `argText` "CARD" `text` "Read CARD from a player's hand.")
        readCardInHandAction
    addOption (kw "?" `text` "Display detailed help text.")
        helpAction
    addOption (kw "." `text` "An alias for (?).")
        nonParseable


enterToContinue :: IO ()
enterToContinue = do
    setSGR [SetColor Foreground Dull White]
    putStrLn "*** PRESS ENTER TO CONTINUE ***"
    _ <- getLine
    return ()


quitAction :: Hearth Console ConsoleAction
quitAction = return QuitAction


rawBanner :: [String]
rawBanner = [
      ""
    , "    _____________________"
    , "  +/7777777777|77777777772+"
    , "  |1*`  2   ( . )   /  `*1|"
    , "  |1` 2   (   ^   )   / `1|"
    , "  |1  (Hearth{@}Shroud)  1|"
    , "  |1, /    (  v  )    2 ,1|"
    , "  |1*,  /   ( ' )   2  ,*1|"
    , "  ++7777777777|7777777777++"
    , "   2+-------------------+/"
    , "" ]


sgrBanner :: [SGRString]
sgrBanner = flip map rawBanner $ concatMap $ \case
    '2' -> f '\\' Dull Green
    '/' -> f '/' Dull Green
    '1' -> f '|' Vivid Red
    '|' -> f '|' Vivid Yellow
    '7' -> f '-' Vivid Red
    '-' -> f '-' Vivid Yellow
    '_' -> f '_' Vivid Yellow
    '+' -> f '+' Vivid Yellow
    '*' -> f '*' Vivid Magenta
    '.' -> f '.' Vivid Green
    '\''-> f '\'' Vivid Green
    '(' -> f '(' Vivid Green
    ')' -> f ')' Vivid Green
    '@' -> sgr [SetColor Background Vivid Blue] ++ f '@' Dull Black ++ sgr [SetColor Background Dull Black]
    '{' -> f '(' Vivid Black
    '}' -> f ')' Vivid Black
    '^' -> f '^' Dull Cyan
    'v' -> f 'v' Dull Cyan
    c -> f c Dull White
    where
        f ch intensity color = sgrColor (intensity, color) ++ [Right ch]


printBanner :: Int -> IO ()
printBanner columnOffset = do
    setSGR [SetColor Foreground Vivid Black]
    forM_ sgrBanner $ \str -> do
        setCursorColumn columnOffset
        putSGRString $ str ++ [Right '\n']
    setSGR [SetColor Foreground Dull White]


helpAction :: Hearth Console ConsoleAction
helpAction = do
    liftIO $ do
        setCursorPosition 1 0
        clearScreen
        putStrLn formattedActionOptions
        cursorUpLine $ 1 + length (lines formattedActionOptions)
        printBanner 75
        putStrLn ""
        putStrLn "Usage:"
        putStrLn "> COMMAND+ARG1+ARG2+ARG3+..."
        putStrLn ""
        putStrLn "(+) and (-) are used to delimit arguments and affect their following arguments' signs."
        putStrLn "Positive arguments denote friendly or neutral choices. Negative arguments denote enemy choices."
        putStrLn "Players are denoted by the number 0."
        putStrLn ""
        putStrLn "- Example: Summon hand minion 4 to board position 3."
        putStrLn "> 1+4+3"
        putStrLn ""
        putStrLn "- Example: Attack with minion 1 against opponent."
        putStrLn "> 2+1-0"
        putStrLn ""
        putStrLn "- Example: Play Fireball (hand spell 3) against opposing hero."
        putStrLn "> 1+3-0"
        putStrLn ""
        putStrLn "- Example: Play Fireball (hand spell 3) against own hero."
        putStrLn "> 1+3+0"
        putStrLn ""
        enterToContinue
    return QuietRetryAction


cmpKeyword :: Keyword -> Keyword -> Ordering
cmpKeyword =  comparing $ \k -> let
    name = concat $ kwNames k
    in case name of
        "" -> -1
        [c] -> case c of
            '?' -> 50 + case kwArgText k of
                "" -> 1
                _ -> 0
            '.' -> 999
            _ -> case isDigit c of
                True -> 30 + ord c - ord '0'
                False -> case isAlpha c of
                    True -> ord c - ord 'A'
                    False -> -666
        _ -> -666


formattedActionOptions :: String
formattedActionOptions = let
    name k = case concat $ kwNames k of
        "" -> " "
        n -> case kwArgText k of
            [] -> n
            _ -> n ++ " "
    ks = sortBy cmpKeyword $ getKeywords actionOptions
    kwLen k = length $ name k ++ kwArgText k
    maxKwLen = maximum $ map kwLen ks
    format k = let
        nameArgs = name k ++ kwArgText k
        pad = replicate (maxKwLen - kwLen k) '-'
        in "-- " ++ nameArgs ++ " --" ++ pad ++ " " ++ kwText k
    in intercalate "\n" $ map format ks


class PickRandom l a | l -> a where
    pickRandom :: (MonadIO m) => l -> m a


instance PickRandom [a] (Maybe a) where
    pickRandom = liftIO . liftM listToMaybe . shuffleM


instance PickRandom (NonEmpty a) a where
    pickRandom (NonEmpty x xs) = pickRandom (x : xs) >>= \case
        Just y -> return y
        Nothing -> $logicError 'pickRandom "Can't pick from (NonEmpty a)?"


autoplayAction :: Hearth Console ConsoleAction
autoplayAction = liftIO (shuffleM activities) >>= decideAction
    where
        decideAction = \case
            [] -> endTurnAction
            m : ms -> m >>= \case
                Nothing -> decideAction ms
                Just action -> action
        activities = [
            tryPlayMinion,
            tryPlaySpell,
            tryAttackPlayerPlayer,
            tryAttackPlayerMinion,
            tryAttackMinionPlayer,
            tryAttackMinionMinion ]
        tryPlayMinion = do
            handle <- getActivePlayerHandle
            cards <- view $ getPlayer handle.playerHand.handCards
            maxPos <- view $ getPlayer handle.playerMinions.to (BoardPos . length)
            let positions = [BoardPos 0 .. maxPos]
            pos <- liftM head $ liftIO $ shuffleM positions
            allowedCards <- flip filterM (reverse cards) $ \card -> local id (playMinion handle card pos) >>= \case
                Failure -> return False
                Success -> return True
            pickRandom allowedCards >>= return . \case
                Nothing -> Nothing
                Just card -> Just $ return $ GameAction $ ActionPlayMinion card pos
        tryPlaySpell = do
            handle <- getActivePlayerHandle
            cards <- view $ getPlayer handle.playerHand.handCards
            allowedCards <- flip filterM (reverse cards) $ \card -> local id (playSpell handle card) >>= \case
                Failure -> return False
                Success -> return True
            pickRandom allowedCards >>= return . \case
                Nothing -> Nothing
                Just card -> Just $ return $ GameAction $ ActionPlaySpell card
        tryAttack predicate = do
            activeHandle <- getActivePlayerHandle
            activeMinions' <- view $ getPlayer activeHandle.playerMinions
            nonActiveHandle <- getNonActivePlayerHandle
            nonActiveMinions' <- view $ getPlayer nonActiveHandle.playerMinions
            let activeMinions = map characterHandle activeMinions'
                nonActiveMinions = map characterHandle nonActiveMinions'
                pairs = filter predicate $ (Left activeHandle, Left nonActiveHandle)
                     : [(a, na) | a <- activeMinions, na <- nonActiveMinions]
                    ++ [(Left activeHandle, na) | na <- nonActiveMinions]
                    ++ [(a, Left nonActiveHandle) | a <- activeMinions]
            allowedPairs <- flip filterM pairs $ \(activeChar, nonActiveChar) -> do
                local id $ enactAttack activeChar nonActiveChar >>= \case
                    Failure -> return False
                    Success -> return True
            pickRandom allowedPairs >>= return . \case
                Nothing -> Nothing
                Just (attacker, defender) -> Just $ return $ GameAction $ ActionAttack attacker defender
        tryAttackPlayerPlayer = tryAttack $ \case
            (Left _, Left _) -> True
            _ -> False
        tryAttackPlayerMinion = tryAttack $ \case
            (Left _, Right _) -> True
            _ -> False
        tryAttackMinionPlayer = tryAttack $ \case
            (Right _, Left _) -> True
            _ -> False
        tryAttackMinionMinion = tryAttack $ \case
            (Right _, Right _) -> True
            _ -> False


endTurnAction :: Hearth Console ConsoleAction
endTurnAction = return $ GameAction ActionEndTurn


lookupIndex :: [a] -> Int -> Maybe a
lookupIndex (x:xs) n = case n == 0 of
    True -> Just x
    False -> lookupIndex xs (n - 1)
lookupIndex [] _ = Nothing


readCardInHandAction :: SignedInt -> Hearth Console ConsoleAction
readCardInHandAction (SignedInt sign handIdx) = do
    handle <- case sign of
        Positive -> getActivePlayerHandle
        Negative -> getNonActivePlayerHandle
    cards <- view $ getPlayer handle.playerHand.handCards
    let mCard = lookupIndex cards $ length cards - handIdx
    case mCard of
        Nothing -> return ComplainRetryAction
        Just card -> do
            liftIO $ do
                let cardStr = showCard card
                setCursorPosition 2 0
                clearScreen
                putStrLn "  __________________ "
                putStrLn " /                  \\"
                putStrLn " | CARD INFORMATION |"
                putStrLn " \\__________________/"
                putStrLn ""
                forM_ (lines cardStr) $ \ln -> putStrLn $ "  " ++ ln
                cursorUpLine $ 7 + length (lines cardStr)
                printBanner 75
                putStrLn ""
                enterToContinue
                return QuietRetryAction


fetchPlayerHandle :: SignedInt -> Hearth Console (Maybe PlayerHandle)
fetchPlayerHandle (SignedInt sign idx) = case idx of
    0 -> liftM Just $ case sign of
        Positive -> getActivePlayerHandle
        Negative -> getNonActivePlayerHandle
    _ -> return Nothing


fetchMinionHandle :: SignedInt -> Hearth Console (Maybe MinionHandle)
fetchMinionHandle (SignedInt sign idx) = case idx of
    0 -> return Nothing
    _ -> do
        p <- case sign of
            Positive -> getActivePlayerHandle
            Negative -> getNonActivePlayerHandle
        ms <- view $ getPlayer p.playerMinions
        return $ lookupIndex (map _boardMinionHandle ms) $ idx - 1


fetchCharacterHandle :: SignedInt -> Hearth Console (Maybe CharacterHandle)
fetchCharacterHandle (SignedInt sign idx) = do
    p <- case sign of
        Positive -> getActivePlayerHandle
        Negative -> getNonActivePlayerHandle
    ms <- view $ getPlayer p.playerMinions
    return $ lookupIndex (Left p : map characterHandle ms) idx


attackAction :: SignedInt -> SignedInt -> Hearth Console ConsoleAction
attackAction attackerIdx defenderIdx = do
    mAttacker <- fetchCharacterHandle attackerIdx
    mDefender <- fetchCharacterHandle defenderIdx
    case mAttacker of
        Nothing -> return ComplainRetryAction
        Just attacker -> case mDefender of
            Nothing -> return ComplainRetryAction
            Just defender -> return $ GameAction $ ActionAttack attacker defender


playCardAction :: List SignedInt -> Hearth Console ConsoleAction
playCardAction = let

    goMinion boardIdx card = do
        handle <- getActivePlayerHandle
        boardLen <- view $ getPlayer handle.playerMinions.to length
        case 0 < boardIdx && boardIdx <= boardLen + 1 of
            False -> return ComplainRetryAction
            True -> return $ GameAction $ ActionPlayMinion card $ BoardPos $ boardIdx - 1

    goSpell s = return $ GameAction $ ActionPlaySpell s

    go handIdx consoleTargets continuation = do
        handle <- getActivePlayerHandle
        cards <- view $ getPlayer handle.playerHand.handCards
        let mCard = lookupIndex cards $ length cards - handIdx
        case mCard of
            Nothing -> return ComplainRetryAction
            Just card -> do
                lift $ pendingTargets .= consoleTargets
                continuation card
    in \case
        List (SignedInt Positive handIdx : SignedInt Positive boardIdx : consoleTargets) -> go handIdx consoleTargets $ goMinion boardIdx
        List (SignedInt Positive handIdx : consoleTargets) -> go handIdx consoleTargets goSpell
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

















