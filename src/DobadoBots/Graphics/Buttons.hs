module DobadoBots.Graphics.Buttons (
  createButtons
) where

import DobadoBots.Graphics.Data  (Buttons(..))
import DobadoBots.Graphics.Utils (getBmpTex)

import qualified SDL             (Renderer(..))

createButtons :: SDL.Renderer -> IO Buttons
createButtons r = Buttons <$> getBmpTex "data/img/start.bmp" r 
