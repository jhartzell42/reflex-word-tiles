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
import           Reflex.Dom
import           Control.Monad (forM_)
import           Control.Monad.Fix (MonadFix)
import           Common.WordTiles

gameDisplay
  :: ( DomBuilder t m
     , PostBuild t m
     )
  => Dynamic t (Game, [GameMessage])
  -> m ()
gameDisplay dGame = dyn_ $ renderGame <$> dGame where
    renderGame (game, messages) = do
        forM_ (gameGuesses game) $ \guess -> do
            el "div" $ do
                forM_ guess $ \(ScoredLetter letter score) -> do
                    let color = case score of
                            LetterScoreAllWrong -> "gray"
                            LetterScoreWrongPlace -> "#cc0"
                            LetterScoreAllRight -> "green"
                        style =
                            "background-color: " <> color <> "; " <>
                            "color: white; " <>
                            "height: 80px; width: 80px;" <>
                            "display: inline-flex;" <>
                            "font-size: 30pt;" <>
                            "align-items: center;" <>
                            "justify-content: center;" <>
                            "border-width: 3px; font-family: sans-serif;"
                    elAttr "span" ("style" =: style) $ el "b" $
                        text $ T.pack $ letter:[]
        forM_ messages $ \message -> do
            el "p" $ text $ T.pack $ show message

wordSet :: Set.Set String
wordSet = Set.fromList wordList where
    file = $(embedFile "/usr/share/dict/words")
    wordList = lines $ unpack file

app
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     )
  => m ()
app = do
    let
        start = Game [] wordSet "PIETY"
        moveAll word (gm, _) = move word gm
    rec
        game <- foldDyn moveAll (start, []) newWord
        gameDisplay game
        newWord <- fmap (fmap T.unpack) $ el "div" $ do
            inputText <- fmap value $ inputElement $ def
            (submit, _) <- el' "button" $ text "Submit"
            let click = domEvent Click submit
            pure $ current inputText <@ click
    pure ()
