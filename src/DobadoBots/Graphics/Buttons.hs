module DobadoBots.Graphics.Buttons (
  createButtons,
  displayButtons
) where

import DobadoBots.Graphics.Data  (Buttons(..), Button(..))
import DobadoBots.Graphics.Utils (getBmpTex)

import qualified SDL             (Renderer(..), V2(..), Point(..),
                                  Rectangle(..), copy)
import Foreign.C.Types (CInt) 

createButtons :: SDL.Renderer -> IO Buttons
createButtons r = do
  startButtonTex <- getBmpTex "data/img/start.bmp" r
  let startButton = Button startButtonTex startButtonPos
  return $ Buttons startButton
  where
    startButtonPos = SDL.P $ SDL.V2 0 0 

displayButtons :: SDL.Renderer -> Buttons -> IO ()
displayButtons r b = do
  let sb = startButton b
  SDL.copy r (fst $ buttonTex sb) Nothing (Just $ SDL.Rectangle (buttonPos sb) (snd $ buttonTex sb))
  
  
