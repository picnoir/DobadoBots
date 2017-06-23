{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module DobadoBots.Graphics.Editor(
  drawEditor,
  renderCode,
  handleEditorEvents,
  appendEventEditor,
  prettyPrintAst,
  generateEditorTextures
) where

import           Prelude hiding (Left, Right)
import           Control.Monad               (when, unless)
import           Control.Monad.State         (StateT(..), get,
                                              modify, liftIO)
import           Data.Char                   (toUpper)                                              
import qualified Data.Text as T              (Text(..), unpack,
                                              intercalate, concat,
                                              splitAt, split,
                                              singleton, drop,
                                              length, pack, lines, unlines)
import           Data.Maybe                  (listToMaybe, catMaybes)
import           Data.Either.Extra           (isLeft, isRight, fromLeft')
import           SDL.Input.Keyboard.Codes
import qualified SDL                         (Renderer(..), Point(..), 
                                             V2(..), Rectangle(..),
                                             V4(..), Texture(..),
                                             Event(..), EventPayload(..),
                                             KeyboardEventData(..), InputMotion(..),
                                             Keysym(..), KeyModifier(..),
                                             Keycode(..),
                                             fillRect, rendererDrawColor,
                                             copy)
import           SDL                         (($=))
import Foreign.C.Types                       (CInt(..)) 
import qualified SDL.Raw as Raw              (Color(..))
import           Text.PrettyPrint            (Doc(..))
import           Text.Parsec                 (ParseError, sourceColumn, sourceLine,
                                              errorPos)

import DobadoBots.GameEngine.Data            (GameState(..), GamePhase(..))
import DobadoBots.Interpreter.Data           (Cond(..))
import DobadoBots.Interpreter.PrettyPrinter  (prettyPrint)
import DobadoBots.Graphics.Data              (RendererState(..), EditorState(..),
                                              EditorEvent(..))
import DobadoBots.Graphics.Utils             (loadFontBlended)

generateEditorTextures :: SDL.Renderer -> RendererState -> IO RendererState
generateEditorTextures r rst = do
  codeTex <- renderCode r . T.lines . text $ editor rst 
  if isLeft $ currentParseResult rst
    then do
      errorTex <- generateSyntaxErrorTex r . fromLeft' $ currentParseResult rst 
      return $ RendererState
                 (robotTexture rst)
                 (editorCursor rst)
                 codeTex
                 (running rst)
                 (editing rst)
                 (buttons rst)
                 errorTex
                 (parseErrorCursor rst)
                 (editor rst)
                 (currentParseResult rst)
  else
    return $ RendererState
                 (robotTexture rst)
                 (editorCursor rst)
                 codeTex
                 (running rst)
                 (editing rst)
                 (buttons rst)
                 (parseErrorMess rst)  
                 (parseErrorCursor rst)
                 (editor rst)
                 (currentParseResult rst)

    


offset :: CInt
offset = 14

drawEditor :: SDL.Renderer -> GameState -> RendererState -> IO ()
drawEditor r st rst = do
  SDL.rendererDrawColor r $= SDL.V4 0 0 0 0
  SDL.fillRect r . Just $ SDL.Rectangle (SDL.P $ SDL.V2 640 0) (SDL.V2 300 480)
  if isLeft $ currentParseResult rst
  then SDL.rendererDrawColor r $= SDL.V4 255 0 0 0
  else SDL.rendererDrawColor r $= SDL.V4 0 255 0 0
  when (isLeft $ currentParseResult rst) $ do
    SDL.fillRect r . Just . errorCursorPosition . fromLeft' $ currentParseResult rst
    SDL.copy r (fst errorCursor) Nothing (Just . errorCursorPosition . fromLeft' $ currentParseResult rst) 
  SDL.fillRect r . Just $ SDL.Rectangle (SDL.P $ SDL.V2 0 480) (SDL.V2 940 20)
  unless (null (parseErrorMess rst) || isRight (currentParseResult rst)) $ SDL.copy r (fst . head $ parseErrorMess rst) Nothing (Just $ SDL.Rectangle (SDL.P $ SDL.V2 10 481) (snd . head $ parseErrorMess rst))
  displayCode r st rst
  case phase st of
    Running -> SDL.copy r (fst $ running rst) Nothing (Just $ SDL.Rectangle (SDL.P $ SDL.V2 640 400)(snd $ running rst))
    Editing -> SDL.copy r (fst $ editing rst) Nothing (Just $ SDL.Rectangle (SDL.P $ SDL.V2 640 400)(snd $ editing rst))
    _ -> return ()
  where
    errorCursor = parseErrorCursor rst
    errorCursorPosition :: ParseError -> SDL.Rectangle CInt
    errorCursorPosition err = SDL.Rectangle  (SDL.P $ SDL.V2 (640 + (errorColumn err * 6 )) (errorLine err * offset)) (snd $ parseErrorCursor rst)
    errorColumn :: ParseError -> CInt
    errorColumn err = fromIntegral $ sourceColumn $ errorPos err 
    errorLine :: ParseError -> CInt
    errorLine err = fromIntegral $ sourceLine $ errorPos err

displayCode :: SDL.Renderer -> GameState -> RendererState -> IO ()
displayCode r st rst = do
                    runStateT (renderLines r $ codeTextures rst) 1 
                    when (phase st == Editing) $ SDL.copy r
                                                          (fst $ editorCursor rst) 
                                                          Nothing 
                                                          (Just $ SDL.Rectangle cursorPos (snd $ editorCursor rst))
                    return ()
  where
    cursorPos = SDL.P $ SDL.V2 (640 + (ccC * 6 )) (clC * offset) 
    clC = fromIntegral $ cl + 1
    ccC = fromIntegral $ cc + 1
    (EditorState t cc cl) = editor rst

generateSyntaxErrorTex :: SDL.Renderer -> ParseError -> IO [(SDL.Texture, SDL.V2 CInt)]
generateSyntaxErrorTex r p = mapM (loadFontBlended r "data/fonts/VT323-Regular.ttf" 14 (Raw.Color 255 255 255 0))  [errTxt]
  where 
    errTxt     = unwords $ drop 1 errTxtList
    errTxtList = lines $ show p
renderCode :: SDL.Renderer -> [T.Text] -> IO [(SDL.Texture, SDL.V2 CInt)]
renderCode r = mapM (renderLine . T.unpack)
  where 
    handleEmptyLines t
      | t == "" = " "
      | otherwise = t
    renderLine l = loadFontBlended r
                                "data/fonts/VT323-Regular.ttf"
                                14
                                (Raw.Color 0 255 0 0)
                                (handleEmptyLines l)

prettyPrintAst :: Cond -> [T.Text]
prettyPrintAst ast = T.lines $ prettyPrint ast

renderLines :: SDL.Renderer -> [(SDL.Texture, SDL.V2 CInt)] -> StateT CInt IO ()
renderLines r strs = do
    mapM_ renderLine strs
    return ()
  where 
    renderLine :: (SDL.Texture, SDL.V2 CInt) -> StateT CInt IO ()
    renderLine (tex, size) = do
          lineNb <- get
          modify (+1)
          let pos = SDL.P $ SDL.V2 650 (lineNb * offset)
          liftIO $ SDL.copy r tex Nothing (Just $ SDL.Rectangle pos size)

handleEditorEvents :: [SDL.Event] -> Maybe EditorEvent
handleEditorEvents evts = listToMaybe . catMaybes $ (sdlEventTransco <$> filteredKeyboardPressEvents)
  where 
        filteredKeyboardPressEvents = filter filterKeyboardPressEvents 
                                                (ked <$> filteredKeyboardEventPayloads) 
        filterKeyboardPressEvents e = case SDL.keyboardEventKeyMotion e of
                                          SDL.Pressed -> True
                                          _           -> False
        ked  e                      = case e of
                                        SDL.KeyboardEvent d -> d
                                        _                   -> error 
                                              "Problem while filtering keyboard events."
        filteredKeyboardEventPayloads = filter filterEventsPayload eventsPayloads 
        filterEventsPayload evt = case evt of
                                    SDL.KeyboardEvent _ -> True 
                                    _                   -> False 
        eventsPayloads          = map SDL.eventPayload evts

sdlEventTransco :: SDL.KeyboardEventData -> Maybe EditorEvent
sdlEventTransco (SDL.KeyboardEventData _ _ _ keySym) = case keySym of
  (SDL.Keysym _ KeycodeA _)           -> handleCharMods 'a'
  (SDL.Keysym _ KeycodeB _)           -> handleCharMods 'b'
  (SDL.Keysym _ KeycodeC _)           -> handleCharMods 'c'
  (SDL.Keysym _ KeycodeD _)           -> handleCharMods 'd'
  (SDL.Keysym _ KeycodeE _)           -> handleCharMods 'e'
  (SDL.Keysym _ KeycodeF _)           -> handleCharMods 'f'
  (SDL.Keysym _ KeycodeG _)           -> handleCharMods 'g'
  (SDL.Keysym _ KeycodeH _)           -> handleCharMods 'h'
  (SDL.Keysym _ KeycodeI _)           -> handleCharMods 'i'
  (SDL.Keysym _ KeycodeJ _)           -> handleCharMods 'j'
  (SDL.Keysym _ KeycodeK _)           -> handleCharMods 'k'
  (SDL.Keysym _ KeycodeL _)           -> handleCharMods 'l'
  (SDL.Keysym _ KeycodeM _)           -> handleCharMods 'm'
  (SDL.Keysym _ KeycodeN _)           -> handleCharMods 'n'
  (SDL.Keysym _ KeycodeO _)           -> handleCharMods 'o'
  (SDL.Keysym _ KeycodeP _)           -> handleCharMods 'p'
  (SDL.Keysym _ KeycodeQ _)           -> handleCharMods 'q'
  (SDL.Keysym _ KeycodeR _)           -> handleCharMods 'r'
  (SDL.Keysym _ KeycodeS _)           -> handleCharMods 's'
  (SDL.Keysym _ KeycodeT _)           -> handleCharMods 't'
  (SDL.Keysym _ KeycodeU _)           -> handleCharMods 'u'
  (SDL.Keysym _ KeycodeV _)           -> handleCharMods 'v'
  (SDL.Keysym _ KeycodeW _)           -> handleCharMods 'w'
  (SDL.Keysym _ KeycodeX _)           -> handleCharMods 'x'
  (SDL.Keysym _ KeycodeY _)           -> handleCharMods 'y'
  (SDL.Keysym _ KeycodeZ _)           -> handleCharMods 'z'
  (SDL.Keysym _ Keycode1 _)           -> Just $ AppendChar '1'
  (SDL.Keysym _ Keycode2 _)           -> Just $ AppendChar '2'
  (SDL.Keysym _ Keycode3 _)           -> Just $ AppendChar '3'
  (SDL.Keysym _ Keycode4 _)           -> Just $ AppendChar '4'
  (SDL.Keysym _ Keycode5 _)           -> Just $ AppendChar '5'
  (SDL.Keysym _ Keycode6 _)           -> Just $ AppendChar '6'
  (SDL.Keysym _ Keycode7 _)           -> Just $ AppendChar '7'
  (SDL.Keysym _ Keycode8 _)           -> Just $ AppendChar '8'
  (SDL.Keysym _ Keycode9 _)           -> Just $ AppendChar '9'
  (SDL.Keysym _ Keycode0 _)           -> Just $ AppendChar '0'
  (SDL.Keysym _ KeycodeReturn _)      -> Just NewLine
  (SDL.Keysym _ KeycodeBackspace _)   -> Just BackSpace
  (SDL.Keysym _ KeycodeDelete _)      -> Just Delete
  (SDL.Keysym _ KeycodeUp _)          -> Just Up
  (SDL.Keysym _ KeycodeDown _)        -> Just Down
  (SDL.Keysym _ KeycodeLeft _)        -> Just Left
  (SDL.Keysym _ KeycodeRight _)       -> Just Right
  (SDL.Keysym _ KeycodeSpace _)       -> Just Space
  _                                   -> Nothing
  where 
        handleCharMods c = if isUpper 
                           then Just $ AppendChar $ toUpper c
                           else Just $ AppendChar c
        code           = SDL.keysymKeycode keySym
        mods           = SDL.keysymModifier keySym
        isUpper = SDL.keyModifierLeftShift mods || SDL.keyModifierRightShift mods 

appendEventEditor :: EditorEvent -> EditorState -> EditorState
appendEventEditor (AppendChar c) est@(EditorState t cc cl) = EditorState (insertCharEditor c est) (cc + 1) cl
appendEventEditor NewLine est@(EditorState t cc cl)        = EditorState (insertCharEditor '\n' est) 0 (cl+1)
appendEventEditor Space est                                = appendEventEditor (AppendChar ' ') est
appendEventEditor Delete est@(EditorState t cc cl)         = EditorState (removeChar est) cc cl
appendEventEditor BackSpace est@(EditorState t cc cl)
  | cc == 0 && cl > 0 = EditorState ( sucklessUnlines . removeLine cl $ appendLineToPreviousLine cl splittedLines)  0 (cl - 1)
  | cc > 0 = EditorState (removeChar $ EditorState t (cc - 1) cl) (max (cc-1) 0) cl
  | otherwise = est
  where
    sucklessUnlines = T.intercalate (T.singleton '\n')
    splittedLines = T.split (=='\n') t
    customUnlines = T.intercalate (T.singleton '\n')
appendEventEditor Left      (EditorState t cc cl)          = EditorState t (max (cc - 1) 0) cl
appendEventEditor Right     (EditorState t cc cl)          = EditorState t (min (cc + 1) lineLength) cl
  where
    lineLength = T.length $ T.split (=='\n') t !! cl
appendEventEditor Up    est@(EditorState t cc cl)
  | cl == 0   = est
  | otherwise = EditorState t (min cc upperLineLength) (max 0 (cl - 1))
  where
    upperLineLength = T.length $ T.split (=='\n') t !! (cl - 1)
appendEventEditor Down  est@(EditorState t cc cl)
  | cl == endLine = est
  | otherwise     = EditorState t (min cc downLineLength) (min endLine (cl + 1))
  where
    endLine = length (T.split (=='\n') t) - 1
    downLineLength = T.length $ T.split (=='\n') t !! (cl + 1)

insertCharEditor :: Char -> EditorState -> T.Text
insertCharEditor c (EditorState t cc cl) 
  | t == ""   = T.singleton c
  | otherwise = T.intercalate (T.singleton '\n') newLines 
  where
    newLines     = insertAt cl alteredLine editorLines
    alteredLine  = T.concat [fst splittedLine, T.singleton c, snd splittedLine]
    splittedLine = T.splitAt cc insertLine
    insertLine   = editorLines !! cl 
    editorLines  = T.split (=='\n') t

removeChar :: EditorState -> T.Text 
removeChar (EditorState t cc cl) 
  | T.length t < 2 = ""
  | otherwise      = T.intercalate (T.singleton '\n') newLines
  where
    newLines     = insertAt cl alteredLine editorLines
    alteredLine  = T.concat [fst splittedLine, T.drop 1 $ snd splittedLine]
    splittedLine = T.splitAt cc deleteLine
    deleteLine   = editorLines !! cl
    editorLines  = T.split (=='\n') t

insertAt :: Int -> T.Text -> [T.Text] -> [T.Text] 
insertAt i y xs
  | length xs > 1 = as ++ (y:bs)
  | otherwise = xs
  where (as,tr:bs) = splitAt i xs

removeLine :: Int -> [T.Text] -> [T.Text]
removeLine pos xs = take pos xs ++ drop (pos + 1) xs

appendLineToPreviousLine :: Int -> [T.Text] -> [T.Text]
appendLineToPreviousLine pos xs = insertAt (pos - 1) (T.concat (previousLine : [line])) xs 
  where line :: T.Text
        line = xs !! pos 
        previousLine :: T.Text
        previousLine = xs !! (pos - 1)
