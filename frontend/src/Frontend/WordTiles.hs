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

individualTile
  :: ( DomBuilder t m
     , PostBuild t m
     )
  => Dynamic t ScoredLetter
  -> m ()
individualTile dScoredLetter = do
    let convertScore score = case score of
            LetterScoreAllWrong -> "allwrong"
            LetterScoreWrongPlace -> "wrongplace"
            LetterScoreAllRight -> "allright"
        dClass = convertScore . scoredLetterScore <$> dScoredLetter
        dLetter = T.pack . (:[]) . scoredLetterLetter <$> dScoredLetter
    elDynAttr "span" (("class" =:) <$> dClass) $ dynText dLetter

gameRow
  :: ( DomBuilder t m
     , PostBuild t m
     )
  => Int
  -> Dynamic t [ScoredLetter]
  -> m ()
gameRow wordLength dWord = do
    el "div" $ forM_ [0..wordLength] $ \i -> do
        individualTile $ (!! i) <$> dWord

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
                    let cls = case score of
                            LetterScoreAllWrong -> "allwrong"
                            LetterScoreWrongPlace -> "wrongplace"
                            LetterScoreAllRight -> "allright"
                    elAttr "span" ("class" =: cls) $ el "b" $
                        text $ T.pack $ letter:[]
        forM_ messages $ \message -> do
            el "p" $ text $ T.pack $ show message

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
        start = Game [] (wordSet 5) "PIETY"
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
