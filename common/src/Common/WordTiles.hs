{-# LANGUAGE MultiWayIf #-}
module Common.WordTiles where
import Control.Monad
import Data.Char (toLower)
import qualified Data.Set as Set
import System.Console.ANSI
import System.Exit (exitSuccess)

data LetterScore
    = LetterScoreAllWrong
    | LetterScoreWrongPlace
    | LetterScoreAllRight
    deriving (Show, Eq)

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

score :: Set.Set String -> String -> String -> Maybe [ScoredLetter]
score wordSet answerRaw guessRaw = do
    answer <- validateWord wordSet answerRaw
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

-- Command line version of game

printScore :: [ScoredLetter] -> IO ()
printScore scoredLetters = do
    forM_ scoredLetters $ \(ScoredLetter ltr scr) -> do
        putStr $ [ltr] <> case scr of
            LetterScoreAllRight -> "! "
            LetterScoreAllWrong -> "  "
            LetterScoreWrongPlace -> "* "
    putStrLn ""

game :: String -> IO ()
game answer = do
    clearScreen
    file <- readFile "/usr/share/dict/words"
    let wordList = lines file
        wordSet = Set.fromList wordList
    forever $ do
        line <- getLine
        case score wordSet answer line of
            Nothing -> putStrLn "Invalid guess! Is it a real word in all caps?"
            Just result -> do
                printScore result
                when (all (== LetterScoreAllRight) $ scoredLetterScore <$> result) $ do
                    putStrLn "Congrats! You win!"
                    exitSuccess
