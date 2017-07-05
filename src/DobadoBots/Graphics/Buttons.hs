module DobadoBots.Graphics.Buttons (
  createButtons,
  displayButtons,
  handleMouseEvents
) where

import DobadoBots.Graphics.Data  (Buttons(..), Button(..), ButtonEvent(..),
                                  RendererState(..), toList)
import DobadoBots.Graphics.Utils (getBmpTex, isInRectangle, changeLevel)

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
  startButtonTex      <- getBmpTex "data/img/start.bmp"       r
  editButtonTex       <- getBmpTex "data/img/edit.bmp"        r
  playButtonTex       <- getBmpTex "data/img/play.bmp"        r
  startButtonTexHover <- getBmpTex "data/img/start-hover.bmp" r
  leftButtonTex       <- getBmpTex "data/img/leftArrow.bmp"   r
  rightButtonTex      <- getBmpTex "data/img/rightArrow.bmp"  r
  let startButton  = Button startButtonTex startButtonTexHover controlButtonPos False False StartEvent
  let editButton   = Button editButtonTex  editButtonTex       controlButtonPos False False EditEvent
  let playButton   = Button playButtonTex  playButtonTex       (SDL.P $ SDL.V2 700 380) False True PlayEvent
  let leftButton   = Button leftButtonTex  leftButtonTex       (SDL.P $ SDL.V2 200 350) False False LeftEvent
  let rightButton  = Button rightButtonTex rightButtonTex      (SDL.P $ SDL.V2 350 350) False False RightEvent
  let choseLButton = Button playButtonTex playButtonTex        (SDL.P $ SDL.V2 275 400) False False ChoseLevelEvent
  return $ Buttons startButton editButton playButton leftButton rightButton choseLButton
  where
    controlButtonPos = SDL.P $ SDL.V2 550 413

displayButtons :: SDL.Renderer -> Buttons -> IO [()]
displayButtons r b = mapM (displayButton r) (toList b) 

displayButton :: SDL.Renderer -> Button -> IO ()
displayButton r b = when (isActive b) $ SDL.copy r (fst tex) Nothing (Just $ SDL.Rectangle (buttonPos b) (snd tex))
  where tex = if isHover b
              then buttonTexHover b
              else buttonTex b
  
handleMouseEvents :: [SDL.Event] -> RendererState -> (Maybe RendererState, Maybe ButtonEvent)
handleMouseEvents evts rst = (nrst, bevt)
  where
    nrst = case bevt of
             (Just event) -> Just $ modifyButtonsStates rst event
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

modifyButtonsStates :: RendererState -> ButtonEvent -> RendererState
modifyButtonsStates rst p = RendererState
                                (levels rst)
                                nCurrentSelectedLvl
                                (robotTexture rst)
                                (editorCursor rst)
                                (codeTextures rst)
                                (running rst)
                                (editing rst)
                                (Buttons nStartButton nEditButton nPlayButton nLevelSelectionL nLevelSelectionR nSelectLevel)
                                (parseErrorMess rst)
                                (parseErrorCursor rst)
                                (splashScreen rst)
                                (editor rst)
                                (currentParseResult rst)
  where oldStartButton     = startButton $ buttons rst
        oldEditButton      = editButton  $ buttons rst
        oldPlayButton      = playButton  $ buttons rst
        oldLevelSelectionL = levelSelectionL $ buttons rst
        oldLevelSelectionR = levelSelectionR $ buttons rst
        oldSelectLevel     = selectLevel $ buttons rst
        nStartButton       = setButtonActivity oldStartButton startActive
        nEditButton        = setButtonActivity oldEditButton  editActive
        nPlayButton        = setButtonActivity oldPlayButton  playActive
        nLevelSelectionL   = setButtonActivity oldLevelSelectionL lvlSelectionActive
        nLevelSelectionR   = setButtonActivity oldLevelSelectionR lvlSelectionActive
        nSelectLevel       = setButtonActivity oldSelectLevel lvlSelectionActive
        nCurrentSelectedLvl= case p of
                                LeftEvent      -> changeLevel (-1) rst
                                RightEvent     -> changeLevel 1 rst
                                _              -> currentSelectedLvl rst
        startActive        = case p of
                                StartEvent      -> False
                                EditEvent       -> True
                                PlayEvent       -> False
                                ChoseLevelEvent -> True
                                LeftEvent       -> False
                                RightEvent      -> False
        editActive         = case p of
                                StartEvent      -> True
                                EditEvent       -> False
                                PlayEvent       -> False
                                ChoseLevelEvent -> False
                                LeftEvent       -> False
                                RightEvent      -> False
        playActive         = case p of
                                StartEvent      -> False
                                EditEvent       -> False
                                PlayEvent       -> False
                                ChoseLevelEvent -> False
                                LeftEvent       -> False
                                RightEvent      -> False
        lvlSelectionActive = case p of
                                StartEvent      -> False
                                EditEvent       -> False
                                PlayEvent       -> True
                                ChoseLevelEvent -> False
                                LeftEvent       -> True
                                RightEvent      -> True

setButtonActivity :: Button -> Bool -> Button 
setButtonActivity b active = Button (buttonTex b)
                              (buttonTexHover b)
                              (buttonPos b)
                              (isHover b)
                              active
                              (event b)
