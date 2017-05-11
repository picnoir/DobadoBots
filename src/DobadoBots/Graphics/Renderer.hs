module DobadoBots.Graphics.Renderer (
  mainGraphicsLoop
) where

import qualified SDL (Renderer, rendererDrawColor, clear,
  present, drawRects, Rectangle(..), Point(..), V2(..), V4(..))
import SDL (($=))
import DobadoBots.GameEngine.Data (GameEngine(..), Objective(..), Obstacle(..), Object(..))
import qualified Linear.V2 as L (V2(..))
import Foreign.C.Types (CInt(..))
import qualified Data.Vector.Storable as V (Vector(..), fromList, map) 

mainGraphicsLoop :: SDL.Renderer -> GameEngine -> IO ()
mainGraphicsLoop renderer gameState = do 
  --let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
  SDL.rendererDrawColor renderer $= SDL.V4 maxBound maxBound maxBound maxBound
  drawArena renderer gameState 
  SDL.clear renderer
  SDL.present renderer

drawArena :: SDL.Renderer -> GameEngine -> IO ()
drawArena renderer gameState = do
  let obsR = getObstaclesRects $ obstacles gameState
  let objR = getObjectiveRect $ objective gameState
  SDL.drawRects renderer obsR


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
  

linearToSDLV2 :: L.V2 Float -> SDL.V2 CInt
linearToSDLV2 (L.V2 x y) = SDL.V2 (toCint x) (toCint y)  
  where toCint x = CInt $ floor x
