{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module DobadoBots.Graphics.Editor(
  drawEditor,
  renderCode,
  handleEditorEvents
) where

import           Control.Monad.State         (StateT(..), get,
                                              modify, liftIO)
import           Data.Text                   (Text(..), unpack)
import qualified SDL                         (Renderer(..), Point(..), 
                                             V2(..), Rectangle(..),
                                             V4(..), Texture(..),
                                             Event(..), EventPayload(..),
                                             KeyboardEventData(..), InputMotion(..),
                                             Keysym(..), KeyModifier(..),
                                             Keycode(..),
                                             fillRect, rendererDrawColor,
                                             copy)
import           SDL                         (($=), pattern KeycodeA)
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

renderCode :: SDL.Renderer -> Cond -> IO [(SDL.Texture, SDL.V2 CInt)]
renderCode r ast = mapM renderLine strs
  where renderLine = loadFontBlended r
                                "data/fonts/Inconsolata-Regular.ttf"
                                15
                                (Raw.Color 0 255 0 0)
        strs = lines . unpack $ prettyPrint ast

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

handleEditorEvents :: [SDL.Event] -> EditorEvent
handleEditorEvents evts = undefined
  where 
        filteredKeyboardPressEvents = filter filterKeyboardPressEvents (auie <$> filteredKeyboardEventPayloads) 
        filterKeyboardPressEvents e = case SDL.keyboardEventKeyMotion e of
                                          SDL.Pressed -> True
                                          _           -> False
        auie e                      = case e of
                                        SDL.KeyboardEvent d -> d
                                        _                   -> error "NOT GOOOD NINJA"
        filteredKeyboardEventPayloads = filter filterEventsPayload eventsPayloads 
        filterEventsPayload evt = case evt of
                                    SDL.KeyboardEvent _ -> True 
                                    _                   -> False 
        eventsPayloads          = map SDL.eventPayload evts

sdlEventTransco :: SDL.KeyboardEventData -> EditorEvent
sdlEventTransco (SDL.KeyboardEventData _ _ _ keySym) = case keySym of
  (SDL.Keysym _ keycodeA (SDL.KeyModifier False False _ _ _ _ _ _ _ _ _)) -> AppendChar 'a'
  where code = SDL.keysymKeycode keySym
        mods = SDL.keysymModifier keySym
