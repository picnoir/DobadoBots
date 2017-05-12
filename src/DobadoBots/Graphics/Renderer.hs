module DobadoBots.Graphics.Renderer (
  mainGraphicsLoop
, Textures(..)
, loadTextures
) where

import GHC.Float (float2Double)
import qualified SDL (Renderer, rendererDrawColor, clear,
  present, fillRects, fillRect, Rectangle(..), Point(..), V2(..), V4(..), Texture(..),
  loadBMP, createTextureFromSurface, freeSurface, copyEx)
import SDL (($=))
import DobadoBots.GameEngine.Data (GameEngine(..), Objective(..), Obstacle(..), Object(..), Robot(..))
import qualified Linear.V2 as L (V2(..))
import Foreign.C.Types (CInt(..), CDouble(..))
import qualified Data.Vector.Storable as V (Vector(..), fromList, map) 

data Textures = Textures {
  robotTexture :: SDL.Texture
}

mainGraphicsLoop :: SDL.Renderer -> GameEngine -> Textures -> IO ()
mainGraphicsLoop renderer gameState tex = do 
  SDL.rendererDrawColor renderer $= SDL.V4 14 36 57 maxBound
  SDL.clear renderer
  drawArena renderer gameState 
  drawRobots renderer (robotTexture tex) $ robots gameState 
  SDL.present renderer

drawArena :: SDL.Renderer -> GameEngine -> IO ()
drawArena renderer gameState = do
  let obsR = getObstaclesRects $ obstacles gameState
  let objR = getObjectiveRect $ objective gameState
  SDL.rendererDrawColor renderer $= SDL.V4 42 84 126 maxBound
  SDL.fillRects renderer obsR
  SDL.rendererDrawColor renderer $= SDL.V4 255 88 0 maxBound
  SDL.fillRect renderer $ Just objR
  

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
  
drawRobot :: SDL.Renderer -> SDL.Texture -> Robot -> IO ()
drawRobot renderer tex robot = SDL.copyEx renderer tex Nothing (Just dest) angle Nothing (SDL.V2 False False)
  where angle :: CDouble
        angle = CDouble $ float2Double $ rotation robot  
        dest :: SDL.Rectangle CInt
        dest = SDL.Rectangle p s
        p :: SDL.Point SDL.V2 CInt
        p = SDL.P $ linearToSDLV2 $ position robot
        s :: SDL.V2 CInt
        s = linearToSDLV2 $ size robot

drawRobots :: SDL.Renderer -> SDL.Texture -> [Robot] -> IO()
drawRobots r t rbts = mapM_ (drawRobot r t) rbts

linearToSDLV2 :: L.V2 Float -> SDL.V2 CInt
linearToSDLV2 (L.V2 x y) = SDL.V2 (toCint x) (toCint y)  
  where toCint x = CInt $ floor x

loadTextures :: FilePath -> SDL.Renderer -> IO (Textures)
loadTextures robotImg renderer = do
  robot <- rTex robotImg renderer 
  return $ Textures robot 
  where rTex path rend = do
          surf <- SDL.loadBMP robotImg
          tex <- SDL.createTextureFromSurface rend surf
          SDL.freeSurface surf 
          return tex
