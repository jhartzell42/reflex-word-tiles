{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE DataKinds           #-}
module Frontend.WordTiles where
import           Data.FileEmbed
import           Data.ByteString.Char8 (unpack)
import qualified Data.Text as T
import qualified Data.Set as Set
import           Reflex.Class
import           Reflex.Dom
import           Control.Monad (forM_, forM, void)
import           Control.Monad.Fix (MonadFix)
import           Common.WordTiles

gameRow
  :: ( DomBuilder t m
     )
  => [ScoredLetter]
  -> m (Event t Char)
gameRow letters = el "div" $ do
    letterEvents <- forM letters $ \(ScoredLetter letter score) -> do
        let cls = case score of
                LetterScoreAllWrong -> "allwrong"
                LetterScoreWrongPlace -> "wrongplace"
                LetterScoreAllRight -> "allright"
        (e, _) <- elAttr' "span" ("class" =: cls) $ el "b" $
            text $ T.pack $ letter:[]
        pure $ letter <$ domEvent Click e
    pure $ leftmost letterEvents

gameDisplay
  :: ( DomBuilder t m
     , PostBuild t m
     )
  => Dynamic t (Game, [GameMessage])
  -> m ()
gameDisplay dGame = dyn_ $ ffor dGame $ \(game, messages) -> do
    forM_ (gameGuesses game) $ \guess -> void $ gameRow guess
    forM_ messages $ \message -> el "p" $ text $ T.pack $ show message

wordSet :: Int -> Set.Set String
wordSet len = Set.fromList wordList where
    file = $(embedFile "/usr/share/dict/words")
    wordList = filter ((== len) . length) $ lines $ unpack file

app
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
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
            inputTextElement <- inputElement def
            let inputText = fmap T.toUpper $ value $ inputTextElement
            (submit, _) <- el' "button" $ text "Submit"
            let enter = keypress Enter inputTextElement
                click = domEvent Click submit
            pure $ current inputText <@ leftmost [click, enter]
    pure ()
