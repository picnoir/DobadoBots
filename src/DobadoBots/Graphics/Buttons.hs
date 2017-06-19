module DobadoBots.Graphics.Buttons (
  createButtons,
  displayButtons,
  handleMouseEvents
) where

import DobadoBots.Graphics.Data  (Buttons(..), Button(..), ButtonEvent(..),
                                  RendererState(..), toList)
import DobadoBots.Graphics.Utils (getBmpTex, isInRectangle)

import           Data.Monoid     (Last(..))
import           Data.Maybe      (listToMaybe, catMaybes)
import           Data.Data       (toConstr)
import           Data.List       (find)
import           Control.Monad   (when)
import qualified SDL             (Renderer(..), V2(..), Point(..),
                                  Rectangle(..),  EventPayload(..),
                                  EventPayload(..), copy, Event(..),
                                  MouseButtonEventData(..), MouseMotionEventData(..),
                                  InputMotion(..))
import Foreign.C.Types (CInt) 

createButtons :: SDL.Renderer -> IO Buttons
createButtons r = do
  startButtonTex <- getBmpTex "data/img/start.bmp" r
  editButtonTex  <- getBmpTex "data/img/edit.bmp"  r
  startButtonTexHover <- getBmpTex "data/img/start-hover.bmp" r
  let startButton = Button startButtonTex startButtonTexHover controlButtonPos False True StartEvent
  let editButton  = Button editButtonTex  editButtonTex       controlButtonPos False False EditEvent
  return $ Buttons startButton editButton
  where
    controlButtonPos = SDL.P $ SDL.V2 550 413

displayButtons :: SDL.Renderer -> Buttons -> IO [()]
displayButtons r b = mapM (displayButton r) [startButton b, editButton b]

displayButton :: SDL.Renderer -> Button -> IO ()
displayButton r b = when (isActive b) $ SDL.copy r (fst tex) Nothing (Just $ SDL.Rectangle (buttonPos b) (snd tex))
  where tex = if isHover b
              then buttonTexHover b
              else buttonTex b
  
handleMouseEvents :: [SDL.Event] -> RendererState -> (Maybe RendererState, Maybe ButtonEvent)
handleMouseEvents evts rst = (nrst, bevt)
  where
    nrst = case bevt of
             (Just event) -> Just $ activateRunningButtons rst event
             _            -> Nothing
    bevt = handleMouseClickEvents rst clickEvent
    (Last moveEvent, Last clickEvent) = 
      foldMap getEvent $ map SDL.eventPayload evts
    getEvent evt = case evt of
      SDL.MouseMotionEvent e -> (Last $ Just e, mempty)
      SDL.MouseButtonEvent e
        | SDL.mouseButtonEventMotion e == SDL.Pressed -> (mempty, Last $ Just e)
      _ -> mempty

handleMouseClickEvents :: RendererState -> Maybe SDL.MouseButtonEventData -> Maybe ButtonEvent
handleMouseClickEvents rst Nothing    = Nothing
handleMouseClickEvents rst (Just evt) = listToMaybe . catMaybes . fmap getAction $ getActiveButtons rst
  where 
        getAction b = if isInRectangle (getRect b) mousePoint
                      then Just $ event b
                      else Nothing
        getRect b = SDL.Rectangle (buttonPos b) (snd $ buttonTex b)
        mousePoint = SDL.mouseButtonEventPos evt

getActiveButtons :: RendererState -> [Button]
getActiveButtons rst = filter isActive (toList $ buttons rst)

activateRunningButtons :: RendererState -> ButtonEvent -> RendererState
activateRunningButtons rst p = RendererState
                                (robotTexture rst)
                                (editorCursor rst)
                                (codeTextures rst)
                                (running rst)
                                (editing rst)
                                (Buttons nStartButton nEditButton)
                                (editor rst)
  where oldStartButton = startButton $ buttons rst
        oldEditButton  = editButton  $ buttons rst
        nStartButton   = setButtonActivity oldStartButton startActive
        nEditButton    = setButtonActivity oldEditButton  (not startActive)
        startActive    = case p of
                          StartEvent -> False
                          EditEvent  -> True

setButtonActivity :: Button -> Bool -> Button 
setButtonActivity b active = Button (buttonTex b)
                              (buttonTexHover b)
                              (buttonPos b)
                              (isHover b)
                              active
                              (event b)
