module DobadoBots.GameEngine.Collisions (
 nearestIntersection
,nearestIntersectionDistance
,nearestDistance
,robotObjectiveIntersection
) where

import           Linear.V2                     (V2(..))
import qualified Data.Foldable           as F  (toList)
import qualified Data.SG.Geometry        as GG (alongLine)
import qualified Data.SG.Shape           as GS (Shape'(..), intersectLineShape, overlap)
import           Data.List                     (minimumBy)
import           Data.Function                 (on)
import qualified Data.SG.Geometry.TwoDim as G2 (Line2'(..), Rel2', Line2', 
                                                Point2'(..), makeRel2, 
                                                intersectLines2)
import           Data.Maybe                    (catMaybes, mapMaybe, maybeToList,
                                                listToMaybe, isJust, fromJust)
import           Data.HashMap.Strict     as HM (lookup)

import           Control.Monad                 (liftM2)

import DobadoBots.GameEngine.Data              (Robot(..), GameState(..),
                                                Collider(..), Obstacle(..),
                                                Objective(..), Object(..), Collision(..),
                                                RobotId(..))
import DobadoBots.GameEngine.Utils             (getXV2, getYV2, minTupleArray,
                                                v2toSGVect, point2ToV2, degreeToRadian,
                                                objectToShape)

robotObjectiveIntersection :: RobotId -> GameState -> Bool
robotObjectiveIntersection rId st = isJust $ GS.overlap rb obj
  where
    rb  = objectToShape . object . fromJust . HM.lookup rId $ robots st
    obj = objectToShape $ objective st

nearestIntersection :: Robot -> GameState -> Collision 
nearestIntersection r st
  | isJust nearestCol = (fst $ fromJust nearestCol, getV2IntersecPoint)
  | otherwise         = error "no colliding point!"
  where
    getV2IntersecPoint      = point2ToV2 . GG.alongLine (snd $ fromJust nearestCol) $ getRobotFrontLine r
    nearestCol              = nearestIntersectionDistance r st

nearestIntersectionDistance :: Robot -> GameState -> Maybe (Collider, Float)
nearestIntersectionDistance r st = minCollider 
  where nearObs            = liftM2 (,) (Just Obstacle) . minTupleArray . obstacleIntersections r $ obstacles st
        nearRob            = liftM2 (,) (Just Robot) . minTupleArray . robotIntersections r . F.toList $ robots st
        nearObj            = liftM2 (,) (Just Objective) . minTupleArray . objectiveIntersections r $ objective st
        nearWall           = Just (Wall, arenaIntersection r st)
        collidersList      = catMaybes [nearObs, nearRob, nearObj, nearWall]
        colVector          = filter ((>=0) . snd) collidersList
        minCollider        = case colVector of
                                (x:xs) -> Just $ minimumBy (compare `on` snd) colVector
                                []     -> Nothing

nearestDistance :: [(Float,Float)] -> Maybe Float
nearestDistance = minTupleArray 

obstacleIntersections :: Robot -> [Obstacle] -> [(Float,Float)] 
obstacleIntersections r = mapMaybe $ returnObjectIntersection r 

objectiveIntersections :: Robot -> Objective -> [(Float,Float)]
objectiveIntersections r obj = catMaybes [returnObjectIntersection r obj] 

arenaIntersection :: Robot -> GameState -> Float
arenaIntersection r st = case length filteredIntersections of
                              0 -> 0 
                              _ -> minimum filteredIntersections
  where 
        filteredIntersections = filter (>= 0) wallsIntersections
        wallsIntersections    = fst <$> catMaybes (G2.intersectLines2 (getRobotFrontLine r) <$> walls)
        walls                 = [leftWall,rightWall,topWall,bottomWall]
        leftWall              = G2.Line2 (G2.Point2 (0,0)) (G2.makeRel2 (0,aHeight))
        rightWall             = G2.Line2 (G2.Point2 (aWidth,0)) (G2.makeRel2 (0,aHeight))
        topWall               = G2.Line2 (G2.Point2 (0,0)) (G2.makeRel2 (aWidth,0))
        bottomWall            = G2.Line2 (G2.Point2 (0,aHeight)) (G2.makeRel2 (aWidth,0))
        (V2 aWidth aHeight)   = arenaSize st

robotIntersections :: Robot -> [Robot] -> [(Float,Float)]
robotIntersections r rbs = mapMaybe (returnObjectIntersection r) otherRobots
  where otherRobots = object <$>  filter (/= r) rbs 

returnObjectIntersection :: Robot -> Object -> Maybe (Float,Float)
returnObjectIntersection robot obj = GS.intersectLineShape (getRobotFrontLine robot) shape
  where shape           = objectToShape obj

getRobotFrontLine :: Robot -> G2.Line2' Float
getRobotFrontLine robot = line 
  where line        = G2.Line2 (G2.Point2 (xRobot,yRobot)) $ G2.makeRel2 (xFrontRobot,yFrontRobot)
        xRobot      = getXV2 centerRobot
        yRobot      = getYV2 centerRobot
        centerRobot = position (object robot) + size (object robot )/ 2
        vRobot      = velocity $ object robot
        rot         = rotation $ object robot
        xFrontRobot = vRobot * (cos . degreeToRadian $ rot)
        yFrontRobot = vRobot * (sin . degreeToRadian $ rot)

