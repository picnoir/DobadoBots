module DobadoBots.Graphics.Buttons (
  createButtons,
  displayButtons
) where

import DobadoBots.Graphics.Data  (Buttons(..), Button(..), ButtonEvent(..),
                                  RendererState(..))
import DobadoBots.Graphics.Utils (getBmpTex)

import qualified SDL             (Renderer(..), V2(..), Point(..),
                                  Rectangle(..), copy, Event)
import Foreign.C.Types (CInt) 

createButtons :: SDL.Renderer -> IO Buttons
createButtons r = do
  startButtonTex <- getBmpTex "data/img/start.bmp" r
  startButtonTexHover <- getBmpTex "data/img/start-hover.bmp" r
  let startButton = Button startButtonTex startButtonTexHover startButtonPos False True StartEvent
  return $ Buttons startButton
  where
    startButtonPos = SDL.P $ SDL.V2 530 400

displayButtons :: SDL.Renderer -> Buttons -> IO ()
displayButtons r b = displayButton r $ startButton b

displayButton :: SDL.Renderer -> Button -> IO ()
displayButton r b = SDL.copy r (fst tex) Nothing (Just $ SDL.Rectangle (buttonPos b) (snd tex))
  where tex = if isHover b
              then buttonTexHover b
              else buttonTex b
  
handleMouseEvents :: [SDL.Event] -> RendererState -> ButtonEvent
handleMouseEvents = undefined
