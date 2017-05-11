module DobadoBots.Graphics.Window (
  createMainWindow
, closeMainWindow
) where
import Control.Monad (unless)
import Data.Text (Text(..))
import qualified SDL
import SDL (($=))
import SDL.Vect (V2(..), V4(..))
import qualified Linear.V2 as L (V2(..))
import Foreign.C.Types (CInt(..))

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

handleEvents :: IO ([SDL.Event])
handleEvents = SDL.pollEvents

