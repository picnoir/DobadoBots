module DobadoBots.Graphics.Window (
  createMainWindow
, closeMainWindow
, mainGraphicsLoop
) where
import Control.Monad (unless)
import Data.Text (Text(..))
import qualified Data.Vector.Storable as V (Vector(..), fromList, map) 
import qualified SDL
import SDL (($=))
import SDL.Vect (V2(..), V4(..))
import qualified Linear.V2 as L (V2(..))
import Foreign.C.Types (CInt(..))
import DobadoBots.GameEngine.Data (GameEngine(..), Objective(..), Obstacle(..), Object(..))

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

mainGraphicsLoop :: SDL.Renderer -> GameEngine -> IO ()
mainGraphicsLoop renderer gameState = do 
  --let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
  drawArena renderer gameState 
  SDL.clear renderer
  SDL.present renderer

getObstaclesRects :: [Obstacle] -> V.Vector (SDL.Rectangle CInt)
getObstaclesRects obs = V.fromList $ rectangles 
  where
    rectangles   = map rectangle obs
    rectangle ob = SDL.Rectangle (pointRect ob) (sizeRect ob)
    pointRect ob = SDL.P $ linearToSDLV2 $ position ob
    sizeRect ob  = linearToSDLV2 $ size ob

getObjectiveRect :: Objective -> SDL.Rectangle CInt
getObjectiveRect obj  = SDL.Rectangle (pointRect obj) (sizeRect obj)
  where pointRect obj = SDL.P $ linearToSDLV2 $ position obj
        sizeRect obj  = linearToSDLV2 $ size obj
  

drawArena :: SDL.Renderer -> GameEngine -> IO ()
drawArena renderer gameState = do
  let obsR = getObstaclesRects $ obstacles gameState
  let objR = getObjectiveRect $ objective gameState
  SDL.drawRects renderer obsR

linearToSDLV2 :: L.V2 Float -> SDL.V2 CInt
linearToSDLV2 (L.V2 x y) = SDL.V2 (toCint x) (toCint y)  
  where toCint x = CInt $ floor x
