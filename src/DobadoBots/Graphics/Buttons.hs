module DobadoBots.Graphics.Buttons (
  createButtons,
  displayButtons,
  handleMouseEvents
) where

import DobadoBots.Graphics.Data  (Buttons(..), Button(..), ButtonEvent(..),
                                  RendererState(..))
import DobadoBots.Graphics.Utils (getBmpTex)

import           Data.Monoid     (Last(..))
import           Data.Data       (toConstr)
import           Data.List       (find)
import qualified SDL             (Renderer(..), V2(..), Point(..),
                                  Rectangle(..),  EventPayload(..),
                                  EventPayload(..), copy, Event(..),
                                  MouseButtonEventData(..), MouseMotionEventData(..))
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
  
handleMouseEvents :: [SDL.Event] -> RendererState -> (Maybe RendererState, Maybe ButtonEvent)
handleMouseEvents evts rst = (nrst, bevt)
  where
    nrst = handleMouseMoveEvents rst moveEvent
    bevt = handleMouseClickEvents rst clickEvent
    (Last moveEvent, Last clickEvent) = 
      foldMap getEvent $ map SDL.eventPayload evts
    getEvent evt = case evt of
      SDL.MouseMotionEvent e -> (Last $ Just e, mempty)
      SDL.MouseButtonEvent e -> (mempty, Last $ Just e)
      _ -> mempty

handleMouseClickEvents :: RendererState -> Maybe SDL.MouseButtonEventData -> Maybe ButtonEvent
handleMouseClickEvents rst evt = Nothing

handleMouseMoveEvents :: RendererState -> Maybe SDL.MouseMotionEventData -> Maybe RendererState
handleMouseMoveEvents rst evt = Nothing
