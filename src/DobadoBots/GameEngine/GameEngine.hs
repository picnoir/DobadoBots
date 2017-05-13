module DobadoBots.GameEngine.GameEngine (
  gameEngineTick
, returnNearestObstacleIntersection
) where

import qualified Data.Sequence as S (update, index)
import Linear.V2 (V2(..))
import qualified Data.SG.Geometry.TwoDim as G2 (Line2'(..), Rel2', Line2', Point2'(..), makeRel2)
import qualified Data.SG.Shape as GS (Shape'(..), intersectLineShape)

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
returnNearestObstacleIntersection = undefined

returnObstacleIntersection :: Robot -> Obstacle -> Maybe (V2 Float)
returnObstacleIntersection robot obstacle = sGVectToV2 <$> GS.intersectLineShape line shape
  where line = G2.Line2 (G2.Point2 (xRobot,yRobot)) (G2.makeRel2 (xFrontRobot,yFrontRobot))
        shape = GS.Rectangle centerRectPoint (v2toSGVect $ size obstacle)
        centerRectPoint = G2.Point2 $ v2toSGVect $ ((size obstacle)/2) + position obstacle 
        xRobot = getXV2 $ position robot
        yRobot = getYV2 $ position robot 
        xFrontRobot = 10 * (cos $ degreeToRadian $ rotation robot)
        yFrontRobot = 10 * (sin $ degreeToRadian $ rotation robot)

getXV2 :: V2 a -> a
getXV2 (V2 x _) = x

getYV2 :: V2 a -> a
getYV2 (V2 _ y) = y

v2toSGVect :: V2 a -> (a,a)
v2toSGVect (V2 x y) = (x,y)

sGVectToV2 :: (a,a) -> V2 a
sGVectToV2 (x,y) = V2 x y
