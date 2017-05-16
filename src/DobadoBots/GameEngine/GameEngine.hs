module DobadoBots.GameEngine.GameEngine (
  gameEngineTick
, returnNearestIntersection
) where

import qualified Data.Sequence           as S  (update, index)
import qualified Data.Foldable           as F  (toList)
import           Data.Function                 (on)
import           Data.List                     (minimumBy)
import           Linear.V2                     (V2(..))
import qualified Linear.Metric           as LM (distance)
import qualified Data.SG.Geometry.TwoDim as G2 (Line2'(..), Rel2', Line2', Point2'(..), makeRel2)
import qualified Data.SG.Geometry        as GG (alongLine)
import           Data.Maybe                    (catMaybes, mapMaybe, maybeToList, listToMaybe, isJust, fromJust)
import qualified Data.SG.Shape           as GS (Shape'(..), intersectLineShape)

import DobadoBots.Interpreter.Data (Cond(..), ActionToken(..))
import DobadoBots.GameEngine.Data  (GameEngine(..), Object(..), Robot, Obstacle(..), Objective(..))

data Collider = Obstacle | Objective | Wall | Robot deriving (Show)

gameEngineTick :: GameEngine -> Cond -> GameEngine
gameEngineTick st (Token t) = applyAction t st
gameEngineTick st _ = undefined

returnNearestIntersection :: Robot -> GameEngine -> (Collider, V2 Float)
returnNearestIntersection r st
  | isJust nearestCol = (fst $ fromJust nearestCol, getV2IntersecPoint)
  | otherwise         = (Wall, polarToCartesian r 20)
  where
    getV2IntersecPoint      = point2ToV2 . GG.alongLine (snd $ fromJust nearestCol) $ getRobotFrontLine r
    polarToCartesian r dist = V2 (dist * cos  (angleRob r))  (dist * sin  (angleRob r))
    angleRob r              = 90 + rotation r
    nearestCol              = returnNearestIntersectionDistance r st

returnNearestIntersectionDistance :: Robot -> GameEngine -> Maybe (Collider, Float)
returnNearestIntersectionDistance r st = case minCollider of
      (col, Just dist) -> Just (col,dist)
      (_, Nothing)      -> Nothing
  where nearObs            = (Obstacle , minTupleArray . obstacleIntersections r $ obstacles st)
        nearRob            = (Robot, minTupleArray . robotIntersections r . F.toList $ robots st)
        nearObj            = (Objective, minTupleArray . objectiveIntersections r $ objective st)
        colVector          = filter (isJust . snd ) [nearObs, nearRob, nearObj]
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
obstacleIntersections r = mapMaybe $ returnObstacleIntersection r 

objectiveIntersections :: Robot -> Objective -> [(Float,Float)]
objectiveIntersections r obj = catMaybes [returnObstacleIntersection r obj] 

robotIntersections :: Robot -> [Robot] -> [(Float,Float)]
robotIntersections r rbs= obstacleIntersections r otherRobots
  where otherRobots = filter (/= r) rbs 

returnObstacleIntersection :: Robot -> Object -> Maybe (Float,Float)
returnObstacleIntersection robot obj = GS.intersectLineShape (getRobotFrontLine robot) shape
  where shape = GS.Rectangle centerRectPoint . v2toSGVect $ size obj / 2
        centerRectPoint = G2.Point2 . v2toSGVect $ size obj /2 + position obj 

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
                  (moveRobot (obstacles st) <$> robots st)

-- TODO: look at lenses, there is a way
-- to get rid of the first line using those.
moveRobot  :: [Obstacle] -> Robot -> Robot
moveRobot obs r = Object newPos (size r) (rotation r) rVel 
  where newPos     = position r + deltaPos
        deltaPos   = V2 (rVel * cos angle) $ rVel * sin angle
        rVel       = minimum $ velocity r : maybeToList nearestD
        angle      = degreeToRadian $ rotation r
        nearestD   = rmBotWidth <$> nearestDistance (obstacleIntersections r obs)
        rmBotWidth = subtract $ (/2) $ getYV2 $ size r

degreeToRadian :: Float -> Float
degreeToRadian d = d / 180 * pi

getRobotFrontLine :: Robot -> G2.Line2' Float
getRobotFrontLine robot = line 
  where line        = G2.Line2 (G2.Point2 (xRobot,yRobot)) $ G2.makeRel2 (xFrontRobot,yFrontRobot)
        xRobot      = getXV2 centerRobot
        yRobot      = getYV2 centerRobot
        centerRobot = position robot + size robot / 2
        xFrontRobot = cos $ degreeToRadian $ rotation robot
        yFrontRobot = sin $ degreeToRadian $ rotation robot

getXV2 :: V2 a -> a
getXV2 (V2 x _) = x

getYV2 :: V2 a -> a
getYV2 (V2 _ y) = y

minTuple :: (Ord a) => (a,a) -> a
minTuple (x,y)
  | x > y = y
  | otherwise = y

minTupleArray :: (Ord a) => [(a,a)] -> Maybe a
minTupleArray xs  
    | not (null xs) = Just <$> minimum $ fmap minTuple xs
    | otherwise     = Nothing

v2toSGVect :: V2 a -> (a,a)
v2toSGVect (V2 x y) = (x,y)

sGVectToV2 :: (a,a) -> V2 a
sGVectToV2 (x,y) = V2 x y

point2ToV2 :: G2.Point2' a -> V2 a
point2ToV2 (G2.Point2 (x,y)) = V2 x y
