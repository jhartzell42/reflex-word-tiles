{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
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
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Set as Set
import           Reflex.Class
import           Reflex.Dom
import           Control.Monad (forM_, forM, when, void)
import           Control.Monad.Fix (MonadFix)
import           Common.WordTiles
import           GHCJS.DOM.EventM
import           GHCJS.DOM.GlobalEventHandlers (keyDown)

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
        dGame <- foldDyn moveAll (start, []) eNewWord
        gameDisplay dGame
        dGameOver <- holdUniqDyn $ ffor dGame $ \(game, msgs) ->
            GameMessageTooManyGuesses `elem` msgs ||
            GameMessageWin `elem` msgs ||
            length (gameGuesses game) >= 6
        eeNewWord <- dyn $ ffor dGameOver $ \gameOver -> do
            if gameOver
                then pure never
                else fmap (fmap T.unpack) $ wordInput $ fst <$> dGame
        eNewWord <- switchHold never eeNewWord
    pure ()

wordSet :: Int -> Set.Set String
wordSet len = Set.fromList wordList where
    file = $(embedFile "/usr/share/dict/words")
    wordList = filter ((== len) . length) $ lines $ unpack file

-- Display

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
    when (GameMessageWin `elem` messages) $ do
        el "p" $ text "You won!"
    when (length (gameGuesses game) >= 6) $
        el "p" $ text "That was your last guess!"
    when (GameMessageInvalidGuess `elem` messages) $ do
        el "p" $ text "Invalid guess... try again!"

-- Input

data InputKey
    = InputKeyLetter Char
    | InputKeyBackspace
    | InputKeyEnter
    deriving Eq

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
    eKey <- fmap leftmost $ sequence
        [ onScreenKeyboard game
        , getKeyboardKey
        ]

    let enterPressed = void $ ffilter (== InputKeyEnter) eKey
        eEnterNewValue = "" <$ enterPressed

        backspacePressed = void $ ffilter (== InputKeyBackspace) eKey
        eBackspaceNewValue = dropLast <$>
                current inputText <@ backspacePressed where
            dropLast = fromMaybe "" . fmap fst . T.unsnoc

        letterPressed = fforMaybe eKey $ \case
            InputKeyLetter letter -> Just letter
            _ -> Nothing
        eLetterNewValue = T.snoc <$> current inputText <@> letterPressed

        eSetValue = leftmost
            [ eEnterNewValue
            , eLetterNewValue
            , eBackspaceNewValue
            ]

    pure $ current inputText <@ enterPressed

onScreenKeyboard
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     )
  => Dynamic t Game
  -> m (Event t InputKey)
onScreenKeyboard dGame = elAttr "div" ("class" =: "onscreenkbd") $ do
    eeCh <- dyn $ ffor dGame $ \game -> do
        let keyboardRow letters = do
            eCh <- gameRow $ scoreAllLetters game letters
            pure $ fmap InputKeyLetter eCh
        e1 <- keyboardRow "QWERTYUIOP"
        e2 <- keyboardRow "ASDFGHJKL"
        e3 <- elAttr "div" ("class" =: "lastrow") $ do
            (enterEl, _) <- elAttr' "span" ("class" =: "allwrong enter") $
                el "b" $
                text "[ENTER]"
            let e3a = InputKeyEnter <$ domEvent Click enterEl
            e3b <- keyboardRow "ZXCVBNM"
            (backspace, _) <- elAttr' "span" ("class" =: "allwrong bksp") $
                el "b" $
                text "<-"
            let e3c = InputKeyBackspace <$ domEvent Click backspace
            pure $ leftmost [e3a, e3b, e3c]
        pure $ leftmost [e1, e2, e3]
    switchHold never eeCh

getGlobalKeyDownEvent
  :: ( DomBuilder t m
     , Prerender t m
     )
  => m (Event t Word)
getGlobalKeyDownEvent = fmap (switch . current) $ prerender (pure never) $ do
        doc <- askDocument
        wrapDomEvent doc (`on` keyDown) getKeyEvent

getKeyboardKey
  :: ( DomBuilder t m
     , Prerender t m
     )
  => m (Event t InputKey)
getKeyboardKey = do
    eRaw <- getGlobalKeyDownEvent
    pure $ fforMaybe eRaw $ \raw -> case keyCodeLookup $ fromIntegral raw of
        Backspace -> Just InputKeyBackspace
        Enter -> Just InputKeyEnter
        KeyA -> Just $ InputKeyLetter 'A'
        KeyB -> Just $ InputKeyLetter 'B'
        KeyC -> Just $ InputKeyLetter 'C'
        KeyD -> Just $ InputKeyLetter 'D'
        KeyE -> Just $ InputKeyLetter 'E'
        KeyF -> Just $ InputKeyLetter 'F'
        KeyG -> Just $ InputKeyLetter 'G'
        KeyH -> Just $ InputKeyLetter 'H'
        KeyI -> Just $ InputKeyLetter 'I'
        KeyJ -> Just $ InputKeyLetter 'J'
        KeyK -> Just $ InputKeyLetter 'K'
        KeyL -> Just $ InputKeyLetter 'L'
        KeyM -> Just $ InputKeyLetter 'M'
        KeyN -> Just $ InputKeyLetter 'N'
        KeyO -> Just $ InputKeyLetter 'O'
        KeyP -> Just $ InputKeyLetter 'P'
        KeyQ -> Just $ InputKeyLetter 'Q'
        KeyR -> Just $ InputKeyLetter 'R'
        KeyS -> Just $ InputKeyLetter 'S'
        KeyT -> Just $ InputKeyLetter 'T'
        KeyU -> Just $ InputKeyLetter 'U'
        KeyV -> Just $ InputKeyLetter 'V'
        KeyW -> Just $ InputKeyLetter 'W'
        KeyX -> Just $ InputKeyLetter 'X'
        KeyY -> Just $ InputKeyLetter 'Y'
        KeyZ -> Just $ InputKeyLetter 'Z'
        _ -> Nothing
