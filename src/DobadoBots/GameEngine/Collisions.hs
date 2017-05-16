module DobadoBots.GameEngine.Collisions (
 nearestIntersection
,nearestIntersectionDistance
,nearestDistance
) where

import           Linear.V2                     (V2(..))
import qualified Data.Foldable           as F  (toList)
import qualified Data.SG.Geometry        as GG (alongLine)
import qualified Data.SG.Shape           as GS (Shape'(..), intersectLineShape)
import           Data.List                     (minimumBy)
import           Data.Function                 (on)
import qualified Data.SG.Geometry.TwoDim as G2 (Line2'(..), Rel2', Line2', Point2'(..), makeRel2)
import           Data.Maybe                    (catMaybes, mapMaybe, maybeToList, listToMaybe, isJust, fromJust)

import DobadoBots.GameEngine.Data (Robot(..), GameState(..),
                                   Collider(..), Obstacle(..),
                                   Objective(..), Object(..))
import DobadoBots.GameEngine.Utils (getXV2, getYV2, minTupleArray, v2toSGVect, point2ToV2, degreeToRadian)
  
nearestIntersection :: Robot -> GameState -> (Collider, V2 Float)
nearestIntersection r st
  | isJust nearestCol = (fst $ fromJust nearestCol, getV2IntersecPoint)
  | otherwise         = (Wall, polarToCartesian r 20)
  where
    getV2IntersecPoint      = point2ToV2 . GG.alongLine (snd $ fromJust nearestCol) $ getRobotFrontLine r
    polarToCartesian r dist = V2 (dist * cos  (angleRob r))  (dist * sin  (angleRob r))
    angleRob r              = 90 + rotation r
    nearestCol              = nearestIntersectionDistance r st

nearestIntersectionDistance :: Robot -> GameState -> Maybe (Collider, Float)
nearestIntersectionDistance r st = case minCollider of
      (col, Just dist) -> Just (col,dist)
      (_, Nothing)      -> Nothing
  where nearObs            = (Obstacle , minTupleArray . obstacleIntersections r $ obstacles st)
        nearRob            = (Robot, minTupleArray . robotIntersections r . F.toList $ robots st)
        nearObj            = (Objective, minTupleArray . objectiveIntersections r $ objective st)
        nearWall           = (Wall, Just $ arenaIntersection r st)
        colVector          = filter distanceFilter [nearObs, nearRob, nearObj]
        distanceFilter v   = (isJust $ snd v) && ((>0) . fromJust $ snd v)
        minCollider        = case colVector of
                                (x:xs) -> minimumBy (compare `on` snd) colVector
                                []     -> (Wall, Nothing)

returnNearestObstacleIntersection :: Robot -> [Obstacle] -> Maybe (V2 Float)
returnNearestObstacleIntersection r obs = getNearestCoordinates . nearestDistance $ obstacleIntersections r obs
  where 
        getNearestCoordinates Nothing = Nothing
        getNearestCoordinates (Just ray) 
          | ray < 0           = Nothing
          | otherwise         = Just . point2ToV2 . GG.alongLine ray $ getRobotFrontLine r 
        angle                 = 45 - rotation r

nearestDistance :: [(Float,Float)] -> Maybe Float
nearestDistance = minTupleArray 

obstacleIntersections :: Robot -> [Obstacle] -> [(Float,Float)] 
obstacleIntersections r = mapMaybe $ returnObjectIntersection r 

objectiveIntersections :: Robot -> Objective -> [(Float,Float)]
objectiveIntersections r obj = catMaybes [returnObjectIntersection r obj] 

arenaIntersection :: Robot -> GameState -> Float
arenaIntersection = undefined

robotIntersections :: Robot -> [Robot] -> [(Float,Float)]
robotIntersections r rbs = mapMaybe (returnObjectIntersection r) otherRobots
  where otherRobots = filter (/= r) rbs 

returnObjectIntersection :: Robot -> Object -> Maybe (Float,Float)
returnObjectIntersection robot obj = GS.intersectLineShape (getRobotFrontLine robot) shape
  where shape = GS.Rectangle centerRectPoint . v2toSGVect $ size obj / 2
        centerRectPoint = G2.Point2 . v2toSGVect $ size obj /2 + position obj 

getRobotFrontLine :: Robot -> G2.Line2' Float
getRobotFrontLine robot = line 
  where line        = G2.Line2 (G2.Point2 (xRobot,yRobot)) $ G2.makeRel2 (xFrontRobot,yFrontRobot)
        xRobot      = getXV2 centerRobot
        yRobot      = getYV2 centerRobot
        centerRobot = position robot + size robot / 2
        xFrontRobot = cos . degreeToRadian $ rotation robot
        yFrontRobot = sin . degreeToRadian $ rotation robot

