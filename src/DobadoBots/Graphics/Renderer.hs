module DobadoBots.Graphics.Renderer (
  mainGraphicsLoop
, Textures(..)
, loadTextures
) where

import DobadoBots.GameEngine.Data (GameState(..), Objective(..), Obstacle(..),
  Object(..), Robot(..), Collider(..), getCenter)

import GHC.Float (float2Double)
import qualified SDL (Renderer, rendererDrawColor, clear,
                      present, fillRects, fillRect, 
                      Rectangle(..), Point(..), V2(..), 
                      V4(..), Texture(..),
                      loadBMP, createTextureFromSurface, 
                      freeSurface, copyEx, drawLine, copy, surfaceDimensions)
import qualified SDL.Raw as Raw (Color(..))
import qualified SDL.TTF as TTF (withInit, wasInit, openFont, renderUTF8Solid,
                                 closeFont)
import SDL (($=))
import qualified Linear.V2 as L (V2(..))
import qualified Linear.Metric as LM (distance)
import Foreign.C.Types (CInt(..), CDouble(..))
import           Control.Monad (unless) 
import qualified Data.Vector.Storable as V (Vector(..), fromList, map) 
import qualified Data.Sequence as S (Seq)
import qualified Data.HashMap.Strict as HM (lookup)

newtype Textures = Textures {
  robotTexture :: SDL.Texture
}

mainGraphicsLoop :: SDL.Renderer -> GameState -> Textures -> IO ()
mainGraphicsLoop renderer gameState tex = do 
  SDL.rendererDrawColor renderer $= SDL.V4 14 36 57 maxBound
  SDL.clear renderer
  drawArena renderer gameState 
  drawLines renderer gameState
  drawRobots renderer (robotTexture tex) $ robots gameState 
  mapM (drawRobotDist renderer gameState ) $ robots gameState
  SDL.present renderer

drawRobotDist :: SDL.Renderer -> GameState -> Robot -> IO ()
drawRobotDist r st rb = do
  let middle = SDL.P $ linearToSDLV2 $ (posTarget + posRob)/ 2
  (fontTex, size) <- loadFont r "data/fonts/OSP-DIN.ttf" 20 (Raw.Color 255 255 255 0) $ show $ LM.distance posRob posTarget
  let loc = SDL.Rectangle middle size
  SDL.copy r fontTex Nothing (Just loc)
  where posRob = position $ object rb
        posTarget = case lookup of
                     (Just val) -> snd val
                     Nothing    -> posRob
        lookup = HM.lookup (robotId rb) $ collisions st


drawLines :: SDL.Renderer -> GameState -> IO ()
drawLines r s = do
  drawObjectiveLine r (robots s) (objective s)
  SDL.rendererDrawColor r $= SDL.V4 255 0 0 maxBound
  drawRobotsFrontLine r (robots s) s

drawRobotsFrontLine :: SDL.Renderer -> S.Seq Robot -> GameState -> IO ()
drawRobotsFrontLine r rbs st = mapM_ drawRbFrontLine rbs
  where
    drawRbFrontLine :: Robot -> IO ()
    drawRbFrontLine rb = SDL.drawLine r (pRobot rb) (pFront rb)
    pRobot rb = SDL.P . linearToSDLV2 . getCenter $ object rb
    pFront rb = case nearestInt $ robotId rb of
        Nothing -> pRobot rb
        (Just int) -> SDL.P . linearToSDLV2 . snd $ int
    nearestInt rbId = HM.lookup rbId $ collisions st

drawObjectiveLine :: SDL.Renderer -> S.Seq Robot -> Objective -> IO ()
drawObjectiveLine r rbs o = mapM_ drawRbLine rbs
  where
    drawRbLine rb = SDL.drawLine r (pRobot rb) pObjective
    pRobot rb     = SDL.P . linearToSDLV2 . getCenter $ object rb
    pObjective    = SDL.P . linearToSDLV2 $ getCenter o

drawArena :: SDL.Renderer -> GameState -> IO ()
drawArena renderer gameState = do
  let obsR = getObstaclesRects $ obstacles gameState
  let objR = getObjectiveRect $ objective gameState
  SDL.rendererDrawColor renderer $= SDL.V4 42 84 126 maxBound
  SDL.fillRects renderer obsR
  SDL.rendererDrawColor renderer $= SDL.V4 255 88 0 maxBound
  SDL.fillRect renderer $ Just objR

getObstaclesRects :: [Obstacle] -> V.Vector (SDL.Rectangle CInt)
getObstaclesRects obs = V.fromList rectangles 
  where
    rectangles   = map rectangle obs
    rectangle ob = SDL.Rectangle (pointRect ob) (sizeRect ob)
    pointRect ob = SDL.P . linearToSDLV2 $ position ob
    sizeRect ob  = linearToSDLV2 $ size ob

getObjectiveRect :: Objective -> SDL.Rectangle CInt
getObjectiveRect obj  = SDL.Rectangle (pointRect obj) (sizeRect obj)
  where pointRect obj = SDL.P . linearToSDLV2 $ position obj
        sizeRect obj  = linearToSDLV2 $ size obj
  
drawRobot :: SDL.Renderer -> SDL.Texture -> Robot -> IO ()
drawRobot renderer tex robot = SDL.copyEx renderer tex Nothing (Just dest) angle Nothing (SDL.V2 False False)
  where 
        angle = CDouble . float2Double . rotation $ object robot
        dest  = SDL.Rectangle p s
        p     = SDL.P . linearToSDLV2 . position $ object robot
        s     = linearToSDLV2 . size $ object robot

drawRobots :: SDL.Renderer -> SDL.Texture -> S.Seq Robot -> IO()
drawRobots r t = mapM_ (drawRobot r t)

linearToSDLV2 :: L.V2 Float -> SDL.V2 CInt
linearToSDLV2 (L.V2 x y) = SDL.V2 (toCint x) (toCint y)  

toCint :: Float -> CInt
toCint x = CInt $ floor x

loadTextures :: FilePath -> SDL.Renderer -> IO Textures
loadTextures robotImg renderer = do
  robot <- rTex robotImg renderer 
  return $ Textures robot 
  where rTex path rend = do
          surf <- SDL.loadBMP robotImg
          tex  <- SDL.createTextureFromSurface rend surf
          SDL.freeSurface surf 
          return tex


loadFont :: SDL.Renderer -> String -> Int -> Raw.Color -> String -> IO (SDL.Texture, SDL.V2 CInt)
loadFont r fontFile size color text = TTF.withInit $ do
    inited <- TTF.wasInit
    unless inited $ error "[Error] Cannot initialise font system." 
    font <- TTF.openFont fontFile size
    textSurface <- TTF.renderUTF8Solid font text color
    textTexture <- SDL.createTextureFromSurface r textSurface
    size <- SDL.surfaceDimensions textSurface
    SDL.freeSurface textSurface
    TTF.closeFont font
    return (textTexture, size)
