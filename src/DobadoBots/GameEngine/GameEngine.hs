module DobadoBots.GameEngine.GameEngine (
  gameEngineTick
, returnNearestObstacleIntersection
) where

import qualified Data.Sequence as S (update, index)
import Linear.V2 (V2(..))
import qualified Linear.Metric as LM (distance)
import qualified Data.SG.Geometry.TwoDim as G2 (Line2'(..), Rel2', Line2', Point2'(..), makeRel2)
import qualified Data.SG.Geometry as GG (alongLine)
import Data.Maybe (catMaybes)
import qualified Data.SG.Shape as GS (Shape'(..), intersectLineShape)
import Debug.Trace

import DobadoBots.Interpreter.Data (Cond(..), ActionToken(..))
import DobadoBots.GameEngine.Data (GameEngine(..), Object(..), Robot, Obstacle(..))

gameEngineTick :: GameEngine -> Cond -> GameEngine
gameEngineTick st (Token t) = applyAction t st
gameEngineTick st _ = undefined

-- TODO: look at lenses, there is a way
-- to get rid of the first line using those.
applyAction :: ActionToken -> GameEngine -> GameEngine
applyAction MoveForward = moveRobots
applyAction _ = error "DAFUK IZ DAT TOKEN?"

moveRobots :: GameEngine -> GameEngine 
moveRobots st = GameEngine
                  (obstacles st)
                  (objective st)
                  (startingPoints st)
                  (S.update 0 (moveRobot $ S.index (robots st) 0) (robots st))

-- TODO: look at lenses, there is a way
-- to get rid of the first line using those.
moveRobot :: Robot -> Robot
moveRobot r = Object newPos (size r) (rotation r) rVel 
  where newPos = (position r) + deltaPos
        deltaPos = V2 (rVel * (cos angle)) (rVel * (sin angle)) 
        rVel = velocity r
        angle = degreeToRadian $ rotation r

degreeToRadian :: Float -> Float
degreeToRadian d = d / 180 * pi

returnNearestObstacleIntersection :: Robot -> [Obstacle] -> Maybe (V2 Float)
returnNearestObstacleIntersection r obs = getNearestCoordinates $ nearestDistance (intersections r obs)
  where 
        getNearestCoordinates :: Maybe Float -> Maybe (V2 Float)
        getNearestCoordinates Nothing = Nothing
        getNearestCoordinates (Just ray) 
          | ray < 0 = Nothing
          | otherwise = Just $ (point2ToV2 $ (GG.alongLine ray (getRobotFrontLine r) )) 
        nearestDistance :: [(Float, Float)] -> Maybe Float
        nearestDistance xs
          | length xs < 1 = Nothing
          | otherwise =  Just $ (minimum$ map minTuple xs)
        intersections :: Robot -> [Obstacle] -> [(Float, Float)]
        intersections r' obs'= catMaybes $ map (returnObstacleIntersection r') obs'
        angle = 45 - rotation r

returnObstacleIntersection :: Robot -> Obstacle -> Maybe (Float,Float)
returnObstacleIntersection robot obstacle = GS.intersectLineShape (getRobotFrontLine robot) shape
  where shape = GS.Rectangle centerRectPoint (v2toSGVect $ (size obstacle)/2)
        centerRectPoint = G2.Point2 $ v2toSGVect $ ((size obstacle)/2) + position obstacle 

getRobotFrontLine :: Robot -> G2.Line2' Float
getRobotFrontLine robot = line 
  where line = G2.Line2 (G2.Point2 (xRobot,yRobot)) (G2.makeRel2 (xFrontRobot,yFrontRobot))
        xRobot = getXV2 $ centerRobot
        yRobot = getYV2 $ centerRobot
        centerRobot = (position robot) + ((size robot) / 2)
        xFrontRobot = cos $ degreeToRadian $ rotation robot
        yFrontRobot = sin $ degreeToRadian $ rotation robot

getXV2 :: V2 a -> a
getXV2 (V2 x _) = x

minTuple :: (Ord a) => (a,a) -> a
minTuple (x,y)
  | x > y = y
  | otherwise = y

getYV2 :: V2 a -> a
getYV2 (V2 _ y) = y

v2toSGVect :: V2 a -> (a,a)
v2toSGVect (V2 x y) = (x,y)

sGVectToV2 :: (a,a) -> V2 a
sGVectToV2 (x,y) = V2 x y

point2ToV2 :: G2.Point2' a -> V2 a
point2ToV2 (G2.Point2 (x,y)) = V2 x y
