{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module DobadoBots.Graphics.Editor(
  drawEditor,
  renderCode,
  handleEditorEvents,
  appendEventEditor,
  prettyPrintAst
) where

import           Prelude hiding (Left, Right)
import           Control.Monad.State         (StateT(..), get,
                                              modify, liftIO)
import           Data.Char                   (toUpper)                                              
import qualified Data.Text as T              (Text(..), unpack,
                                              intercalate, concat,
                                              splitAt, lines,
                                              singleton, drop,
                                              length, pack)
import           Data.Maybe                  (listToMaybe, catMaybes)
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

import DobadoBots.GameEngine.Data            (GameState(..), GamePhase(..))
import DobadoBots.Interpreter.Data           (Cond(..))
import DobadoBots.Interpreter.PrettyPrinter  (prettyPrint)
import DobadoBots.Graphics.Data              (RendererState(..), EditorState(..),
                                              EditorEvent(..))
import DobadoBots.Graphics.Utils             (loadFontBlended)

offset :: CInt
offset = 15

drawEditor :: SDL.Renderer -> GameState -> RendererState -> IO ()
drawEditor r st rst = do
  SDL.rendererDrawColor r $= SDL.V4 0 0 0 0
  SDL.fillRect r . Just $ SDL.Rectangle (SDL.P $ SDL.V2 540 0) (SDL.V2 300 480)
  displayCode r st rst
  case phase st of
    Running -> SDL.copy r (fst $ running rst) Nothing (Just $ SDL.Rectangle (SDL.P $ SDL.V2 640 400)(snd $ running rst))
    Editing -> SDL.copy r (fst $ editing rst) Nothing (Just $ SDL.Rectangle (SDL.P $ SDL.V2 640 400)(snd $ editing rst))
    _ -> return ()

displayCode :: SDL.Renderer -> GameState -> RendererState -> IO ()
displayCode r st rst = do
                    runStateT (renderLines r $ codeTextures rst) 1 
                    return ()

renderCode :: SDL.Renderer -> [T.Text] -> IO [(SDL.Texture, SDL.V2 CInt)]
renderCode r = mapM (renderLine . T.unpack)
  where 
    handleEmptyLines t
      | t == "" = " "
      | otherwise = t
    renderLine l = loadFontBlended r
                                "data/fonts/Inconsolata-Regular.ttf"
                                15
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
          let pos = SDL.P $ SDL.V2 550 (lineNb * offset)
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
  | cc > 0 = EditorState (removeChar $ EditorState t (cc - 1) cl) (max (cc-1) 0) cl
  | otherwise = est
appendEventEditor Left      (EditorState t cc cl)          = EditorState t (max (cc - 1) 0) cl
appendEventEditor Right     (EditorState t cc cl)          = EditorState t (min (cc + 1) lineLength) cl
  where
    lineLength = T.length $ T.lines t !! cl
appendEventEditor Up    est@(EditorState t cc cl)
  | cl == 0   = est
  | otherwise = EditorState t (min cc upperLineLength) (max 0 (cl - 1))
  where
    upperLineLength = T.length $ T.lines t !! (cl - 1)
appendEventEditor Down  est@(EditorState t cc cl)
  | cl == endLine = est
  | otherwise     = EditorState t (min cc downLineLength) (min endLine (cl + 1))
  where
    endLine = length (T.lines t) - 1
    downLineLength = T.length $ T.lines t !! (cl + 1)

insertCharEditor :: Char -> EditorState -> T.Text
insertCharEditor c (EditorState t cc cl) 
  | t == ""   = T.singleton c
  | otherwise = T.intercalate (T.singleton '\n') newLines 
  where
    newLines     = insertAt cl alteredLine editorLines
    alteredLine  = T.concat [fst splittedLine, T.singleton c, snd splittedLine]
    splittedLine = T.splitAt cc insertLine
    insertLine   = editorLines !! cl 
    editorLines  = T.lines t

removeChar :: EditorState -> T.Text 
removeChar (EditorState t cc cl) 
  | T.length t < 2 = ""
  | otherwise      = T.intercalate (T.singleton '\n') newLines
  where
    newLines     = insertAt cl alteredLine editorLines
    alteredLine  = T.concat [fst splittedLine, T.drop 1 $ snd splittedLine]
    splittedLine = T.splitAt cc deleteLine
    deleteLine   = editorLines !! cl
    editorLines  = T.lines t

insertAt :: Int -> T.Text -> [T.Text] -> [T.Text] 
insertAt i y xs
  | length xs > 1 = as ++ (y:bs)
  | otherwise = xs
  where (as,tr:bs) = splitAt i xs
