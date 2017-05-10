{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (unless)
import Data.Text (Text(..))
import Foreign.C.Types (CInt)
import qualified SDL
import SDL (($=))
import SDL.Vect (V2(..), V4(..))

main :: IO ()
main = do
  (renderer, window) <- createMainWindow "DobadoBots" 
  mainLoop renderer 
  closeMainWindow renderer window

mainLoop :: SDL.Renderer -> IO ()
mainLoop renderer = do 
  events <- SDL.pollEvents
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
  SDL.clear renderer
  SDL.present renderer
  unless quit $ mainLoop renderer

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

createMainWindow :: Text -> IO ((SDL.Renderer, SDL.Window))
createMainWindow winName = do
  SDL.initialize [SDL.InitVideo]
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  window <- SDL.createWindow winName SDL.defaultWindow {SDL.windowInitialSize = V2 screenWidth screenHeight}
  SDL.showWindow window
  renderer <-
    SDL.createRenderer
      window
      (-1)
      SDL.RendererConfig
        { SDL.rendererType = SDL.AcceleratedVSyncRenderer
        , SDL.rendererTargetTexture = False }
  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
  return (renderer, window)

closeMainWindow :: SDL.Renderer -> SDL.Window -> IO ()
closeMainWindow renderer window = do
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
