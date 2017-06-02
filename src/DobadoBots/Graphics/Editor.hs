module DobadoBots.Graphics.Editor(
  drawEditor
) where

import qualified SDL (Renderer(..), Point(..), 
                      V2(..), Rectangle(..),
                      V4(..),
                      fillRect, rendererDrawColor)
import           SDL (($=))

import DobadoBots.GameEngine.Data (GameState(..))

drawEditor :: SDL.Renderer -> GameState -> IO ()
drawEditor r st = do
  SDL.rendererDrawColor r $= SDL.V4 0 0 0 0
  SDL.fillRect r $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 540 0) (SDL.V2 300 480)
