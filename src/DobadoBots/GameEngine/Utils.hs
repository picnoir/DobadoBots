module DobadoBots.GameEngine.Utils(
  getXV2,
  getYV2,
  minTuple,
  minTupleArray,
  v2toSGVect,
  sGVectToV2,
  degreeToRadian, 
  point2ToV2 
) where

import Linear.V2 (V2(..))
import Data.SG.Geometry.TwoDim (Point2'(..))

getXV2 :: V2 a -> a
getXV2 (V2 x _) = x

getYV2 :: V2 a -> a
getYV2 (V2 _ y) = y

minTuple :: (Ord a) => (a,a) -> a
minTuple (x,y)
  | x > y = y
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

point2ToV2 :: Point2' a -> V2 a
point2ToV2 (Point2 (x,y)) = V2 x y
