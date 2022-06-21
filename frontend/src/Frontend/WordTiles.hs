{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE DataKinds         #-}
module Frontend.WordTiles where
import           Data.FileEmbed
import           Data.ByteString.Char8 (unpack)
import qualified Data.Text as T
import qualified Data.Set as Set
import           Reflex.Class
import           Reflex.Dom
import           Control.Monad (forM_)
import           Control.Monad.Fix (MonadFix)
import           Common.WordTiles

gameRow
  :: ( DomBuilder t m
     )
  => [ScoredLetter]
  -> m ()
gameRow letters = el "div" $ do
    forM_ letters $ \(ScoredLetter letter score) -> do
        let cls = case score of
                LetterScoreAllWrong -> "allwrong"
                LetterScoreWrongPlace -> "wrongplace"
                LetterScoreAllRight -> "allright"
        elAttr "span" ("class" =: cls) $ el "b" $
            text $ T.pack $ letter:[]

gameDisplay
  :: ( DomBuilder t m
     , PostBuild t m
     )
  => Dynamic t (Game, [GameMessage])
  -> m ()
gameDisplay dGame = dyn_ $ ffor dGame $ \(game, messages) -> do
    forM_ (gameGuesses game) $ \guess -> gameRow guess
    forM_ messages $ \message -> el "p" $ text $ T.pack $ show message

wordSet :: Int -> Set.Set String
wordSet len = Set.fromList wordList where
    file = $(embedFile "/usr/share/dict/words")
    wordList = filter ((== len) . length) $ lines $ unpack file

{-
displayLetters
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , HasDocument m
     )
  => m ()
displayLetters = do
    doc <- askDocument
    let newLetter = fmap show <$> domEvent Keydown doc
    letters <- foldDyn (<>) "" newLetter
    pure ()
-}

app
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , HasDocument m
     )
  => m ()
app = do
    let
        start = Game [] (wordSet 5) "GLOAT"
        moveAll word (gm, _) = move word gm
    rec
        game <- foldDyn moveAll (start, []) newWord
        gameDisplay game
        newWord <- fmap (fmap T.unpack) $ el "div" $ do
            inputText <- fmap (fmap T.toUpper . value) $ inputElement $ def
            (submit, _) <- el' "button" $ text "Submit"
            let click = domEvent Click submit
            pure $ current inputText <@ click
        displayLetters
    pure ()
