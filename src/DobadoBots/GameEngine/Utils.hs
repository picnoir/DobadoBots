module DobadoBots.GameEngine.Utils(
  getXV2,
  getYV2,
  minTuple,
  minTupleArray,
  v2toSGVect,
  sGVectToV2,
  degreeToRadian, 
  radianToDegree,
  point2ToV2,
  objectToShape,
  setPhase
) where

import Linear.V2               (V2(..))
import Data.SG.Geometry.TwoDim (Point2'(..))
import qualified Data.SG.Shape as GS (Shape'(..))
import qualified Data.SG.Geometry.TwoDim as G2 (Point2'(..))

import DobadoBots.GameEngine.Data (Object(..), GamePhase(..),
                                   GameState(..))

getXV2 :: V2 a -> a
getXV2 (V2 x _) = x

getYV2 :: V2 a -> a
getYV2 (V2 _ y) = y

minTuple :: (Ord a) => (a,a) -> a
minTuple (x,y)
  | x > y     = y
  | otherwise = x

minTupleArray :: (Ord a) => [(a,a)] -> Maybe a
minTupleArray xs  
    | not (null xs) = Just <$> minimum $ fmap minTuple xs
    | otherwise     = Nothing 

v2toSGVect :: V2 a -> (a,a)
v2toSGVect (V2 x y) = (x,y)

sGVectToV2 :: (a,a) -> V2 a
sGVectToV2 (x,y) = V2 x y

degreeToRadian :: Float -> Float
degreeToRadian d = (d * pi) / 180 

radianToDegree :: Float -> Float
radianToDegree d = (d*180) / pi

point2ToV2 :: Point2' a -> V2 a
point2ToV2 (Point2 (x,y)) = V2 x y

objectToShape :: Object -> GS.Shape' Float
objectToShape o = GS.Rectangle centerRectPoint . v2toSGVect $ size o / 2
  where centerRectPoint = G2.Point2 . v2toSGVect $ size o / 2 + position o

setPhase :: GamePhase -> GameState -> GameState
setPhase newPhase st = GameState
                         (obstacles st)
                         (arenaSize st)
                         (objective st)
                         (startingPoints st)
                         (robots st)
                         newPhase
                         (collisions st)
                         (ast st)
