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
import           Data.Char (isUpper)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Set as Set
import           Reflex.Class
import           Reflex.Dom
import           Control.Monad (forM_, forM, void)
import           Control.Monad.Fix (MonadFix)
import           Common.WordTiles
import           GHCJS.DOM.EventM
import           GHCJS.DOM.GlobalEventHandlers (keyDown)

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

onScreenKeyboard
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     )
  => Dynamic t Game
  -> m (Event t Char)
onScreenKeyboard dGame = elAttr "div" ("class" =: "onscreenkbd") $ do
    eeCh <- dyn $ ffor dGame $ \game -> do
        e1 <- gameRow $ scoreAllLetters game "QWERTYUIOP"
        e2 <- gameRow $ scoreAllLetters game "ASDFGHJKL"
        e3 <- elAttr "div" ("class" =: "lastrow") $ do
            (enterEl, _) <- elAttr' "span" ("class" =: "allwrong enter") $
                el "b" $
                text "[ENTER]"
            let e3a = '\n' <$ domEvent Click enterEl
            e3b <- gameRow $ scoreAllLetters game "ZXCVBNM"
            (backspace, _) <- elAttr' "span" ("class" =: "allwrong bksp") $
                el "b" $
                text "<-"
            let e3c = '\b' <$ domEvent Click backspace
            pure $ leftmost [e3a, e3b, e3c]
        pure $ leftmost [e1, e2, e3]
    switchHold never eeCh

wordInput
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , Prerender t m
     )
  => Dynamic t Game
  -> m (Event t T.Text)
wordInput game = el "div" $ mdo
    -- Text entered so far
    inputText <- holdDyn "" eSetValue
    elAttr "div" ("class" =: "inputtext") $
        dynText inputText

    -- Key press event from keyboard, on-screen or physical
    key <- do
        key1 <- onScreenKeyboard game
        wKey2 <- fmap (switch . current) $ prerender (pure never) $ do
            doc <- askDocument
            wrapDomEvent doc (`on` keyDown) getKeyEvent
        let key2 = mapMaybe convertKey wKey2
        pure $ leftmost [key1, key2]

    let enterPressed = void $ ffilter (== '\n') key
        eEnterNewValue = "" <$ enterPressed

        backspacePressed = void $ ffilter (== '\b') key
        eBackspaceNewValue = dropLast <$>
                current inputText <@ backspacePressed where
            dropLast = fromMaybe "" . fmap fst . T.unsnoc

        letterPressed = ffilter isUpper key
        eLetterNewValue = T.snoc <$> current inputText <@> letterPressed

        eSetValue = leftmost
            [ eEnterNewValue
            , eLetterNewValue
            , eBackspaceNewValue
            ]

    pure $ current inputText <@ enterPressed

app
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , Prerender t m
     )
  => m ()
app = do
    let
        start = Game [] (wordSet 5) "AWFUL"
        moveAll word (gm, _) = move word gm
    rec
        game <- foldDyn moveAll (start, []) eNewWord
        gameDisplay game
        eNewWord <- fmap (fmap T.unpack) $ wordInput $ fst <$> game
    pure ()

convertKey :: Word -> Maybe Char
convertKey key = case keyCodeLookup $ fromIntegral key of
    Backspace -> Just '\b'
    Enter -> Just '\n'
    KeyA -> Just 'A'
    KeyB -> Just 'B'
    KeyC -> Just 'C'
    KeyD -> Just 'D'
    KeyE -> Just 'E'
    KeyF -> Just 'F'
    KeyG -> Just 'G'
    KeyH -> Just 'H'
    KeyI -> Just 'I'
    KeyJ -> Just 'J'
    KeyK -> Just 'K'
    KeyL -> Just 'L'
    KeyM -> Just 'M'
    KeyN -> Just 'N'
    KeyO -> Just 'O'
    KeyP -> Just 'P'
    KeyQ -> Just 'Q'
    KeyR -> Just 'R'
    KeyS -> Just 'S'
    KeyT -> Just 'T'
    KeyU -> Just 'U'
    KeyV -> Just 'V'
    KeyW -> Just 'W'
    KeyX -> Just 'X'
    KeyY -> Just 'Y'
    KeyZ -> Just 'Z'
    _ -> Nothing
