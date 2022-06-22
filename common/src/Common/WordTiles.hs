{-# LANGUAGE MultiWayIf #-}
module Common.WordTiles (
    LetterScore(..),
    ScoredLetter(..),
    Game(..),
    GameMessage(..),
    move,
    cliGame,
) where
import Control.Monad
import Control.Monad.Writer.Lazy (tell, execWriter)
import Data.Char (toLower)
import Data.Either (lefts, rights)
import Data.Either.Extra (maybeToEither)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Set as Set
import System.Console.ANSI
import System.Exit (exitSuccess)

data LetterScore
    = LetterScoreAllWrong
    | LetterScoreWrongPlace
    | LetterScoreAllRight
    deriving (Show, Eq, Ord)

data ScoredLetter = ScoredLetter
    { scoredLetterLetter :: Char
    , scoredLetterScore :: LetterScore
    }
    deriving (Show, Eq)

validateLetter :: Char -> Maybe Char
validateLetter c = if c <= 'Z' && c >= 'A'
    then Just c
    else Nothing

validateWord :: Set.Set String -> String -> Maybe String
validateWord wordSet wordRaw = do
    word <- mapM validateLetter wordRaw
    let lowerWord = toLower <$> word
    guard $ Set.member lowerWord wordSet
    pure word

scoreGuess :: Set.Set String -> String -> String -> Maybe [ScoredLetter]
scoreGuess wordSet answer guessRaw = do
    guess <- validateWord wordSet guessRaw
    guard $ length answer == length guess
    forM (zip3 answer guess [0..]) $ \(a, g, ix) -> do
        let answerTotalCount = length $ filter (== g) answer
            guessTilNowCount = length $ filter (== g) $ take ix guess
            futureExactMatches = length $ filter (== (g, g)) $ drop ix $
                zip answer guess
            guessesUsedCount = guessTilNowCount + futureExactMatches
            presentAtAll = g `elem` answer
            qualifies = presentAtAll && guessesUsedCount < answerTotalCount
            letterScore = if | g == a -> LetterScoreAllRight
                             | qualifies -> LetterScoreWrongPlace
                             | otherwise -> LetterScoreAllWrong
        Just $ ScoredLetter g letterScore

data Game = Game
    { gameGuesses :: [[ScoredLetter]]
    , gameWordSet :: Set.Set String
    , gameAnswer :: String
    }
    deriving (Show, Eq)

data GameMessage
    = GameMessageInvalidGuess
    | GameMessageTooManyGuesses
    | GameMessageWin
    deriving (Show, Eq)

scoreAllLetters :: Game -> [Char] -> [ScoredLetter]
scoreAllLetters game str = scoreLetter <$> str where
    scoreLetter letter = ScoredLetter letter $
        fromMaybe LetterScoreAllWrong $
        M.lookup letter overallMap
    letterAsMap (ScoredLetter letter score) = M.singleton letter score
    combineMaps = M.unionWith max
    allLetters = concat $ gameGuesses game
    individualMaps = letterAsMap <$> allLetters
    overallMap = foldl combineMaps M.empty individualMaps

move :: String -> Game -> (Game, [GameMessage])
move nextGuess game@(Game guesses wordSet answer) =
    ( game
        { gameGuesses = newGuesses
        }
    , execWriter $ do
        tell $ lefts [scoredGuess]
        forM_ newGuesses $ \guess -> do
            when (win guess) $ tell [GameMessageWin]
    ) where
    win = all ((== LetterScoreAllRight) . scoredLetterScore)
    newGuesses = guesses <> rights [scoredGuess]
    scoredGuess = do
        when (length guesses >= 6) $ Left GameMessageTooManyGuesses
        maybeToEither GameMessageInvalidGuess $ scoreGuess wordSet answer nextGuess

-- Command line version of game

printScore :: [ScoredLetter] -> IO ()
printScore scoredLetters = do
    forM_ scoredLetters $ \(ScoredLetter ltr scr) -> do
        putStr $ [ltr] <> case scr of
            LetterScoreAllRight -> "! "
            LetterScoreAllWrong -> "  "
            LetterScoreWrongPlace -> "* "
    putStrLn ""

cliGame :: String -> IO ()
cliGame answer = do
    clearScreen
    file <- readFile "/usr/share/dict/words"
    let wordList = lines file
        wordSet = Set.fromList wordList
    forever $ do
        line <- getLine
        case scoreGuess wordSet answer line of
            Nothing -> putStrLn "Invalid guess! Is it a real word in all caps?"
            Just result -> do
                printScore result
                when (all (== LetterScoreAllRight) $ scoredLetterScore <$> result) $ do
                    putStrLn "Congrats! You win!"
                    exitSuccess
