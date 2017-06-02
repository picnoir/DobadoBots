{-# LANGUAGE OverloadedStrings #-}

module DobadoBots.Graphics.Editor(
  drawEditor,
  renderCode
) where

import           Data.Text (Text(..))
import qualified SDL       (Renderer(..), Point(..), 
                            V2(..), Rectangle(..),
                            V4(..), Texture(..),
                            fillRect, rendererDrawColor)
import           SDL       (($=))
import Foreign.C.Types     (CInt(..)) 

import DobadoBots.GameEngine.Data   (GameState(..))
import DobadoBots.Graphics.Utils (loadFont)

drawEditor :: SDL.Renderer -> GameState -> IO ()
drawEditor r st = do
  SDL.rendererDrawColor r $= SDL.V4 0 0 0 0
  SDL.fillRect r . Just $ SDL.Rectangle (SDL.P $ SDL.V2 540 0) (SDL.V2 300 480)
  displayCode r st

displayCode :: SDL.Renderer -> GameState -> IO ()
displayCode r st = undefined

renderCode :: SDL.Renderer -> GameState -> (SDL.Texture, SDL.V2 CInt)
renderCode r t = undefined
