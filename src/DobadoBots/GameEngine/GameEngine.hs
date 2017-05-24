{-# LANGUAGE OverloadedStrings #-}
module DobadoBots.GameEngine.GameEngine (
  gameEngineTick,
  generateGameState
) where

import qualified Data.Sequence           as S  (update, index, singleton)
import           Linear.V2                     (V2(..))
import qualified Linear.Metric           as LM (distance)
import           Data.Maybe                    (maybeToList, fromJust)
import qualified Data.HashMap.Strict     as HM (insert, HashMap, empty, fromList, lookup)

import DobadoBots.Interpreter.Data             (Cond(..), ActionToken(..))
import DobadoBots.GameEngine.Data              (GameState(..), Object(..), Robot(..),
                                                RobotId(..),Obstacle(..), Objective(..),
                                                Collision(..), Level(..))
import DobadoBots.GameEngine.Collisions        (nearestIntersection, nearestIntersectionDistance,
                                                nearestDistance)
import DobadoBots.GameEngine.Utils             (getYV2, minTuple, degreeToRadian)
import DobadoBots.Interpreter.Interpreter      (interpretScript) 

generateGameState :: Level -> GameState
generateGameState l = GameState
                            (lObstacles l)
                            (lArenaSize l)
                            (lObjective l)
                            (lStartingPoints l)
                            (HM.fromList[("UniqRobot",Robot' "UniqRobot" (head $ lStartingPoints l))])
                            HM.empty

gameEngineTick :: GameState -> Cond -> GameState 
gameEngineTick st ast       = applyAction actionToken "UniqRobot" nst 
    where nst = computeCollisions st
          actionToken       = interpretScript ast "UniqRobot" nst 

-- TODO: look at lenses, there is a way
-- to get rid of the first line using those.
applyAction :: ActionToken -> RobotId -> GameState -> GameState 
applyAction MoveForward rId = moveRobots 
applyAction TurnLeft rId    = rotateRobot (-1) rId 
applyAction TurnRight rId   = rotateRobot 1 rId
applyAction _ _             = error "DAFUK IZ DAT TOKEN?"

rotateRobot :: Float -> RobotId -> GameState -> GameState
rotateRobot angle rId st = GameState 
                            (obstacles st)
                            (arenaSize st)
                            (objective st)
                            (startingPoints st)
                            (HM.insert rId newRobot $ robots st)
                            (collisions st)
  where robot = fromJust . HM.lookup rId $ robots st
        newRobot = Robot' (robotId robot) (Object
                                      (position $ object robot)
                                      (size $ object robot)
                                      (angle + (rotation $ object robot))
                                      (velocity $ object robot))

moveRobots :: GameState -> GameState 
moveRobots st = GameState
                  (obstacles st)
                  (arenaSize st)
                  (objective st)
                  (startingPoints st)
                  (moveRobot st <$> robots st)
                  (collisions st)

-- TODO: look at lenses, there is a way
-- to get rid of the first line using those.
moveRobot  :: GameState -> Robot -> Robot
moveRobot st r = Robot' (robotId r) $ Object newPos (size $ object r) (rotation $ object r) rVel 
  where newPos     = position (object r) + deltaPos
        deltaPos   = V2 (rVel * cos angle) $ rVel * sin angle
        rVel       = minimum $ velocity (object r) : maybeToList nearestD 
        angle      = degreeToRadian . rotation $ object r
        nearestD   = max 0 . rmBotWidth . snd <$> nearestIntersectionDistance r st
        rmBotWidth = subtract . (/2) . getYV2 . size $ object r

computeCollisions :: GameState -> GameState
computeCollisions st = GameState
                          (obstacles st)
                          (arenaSize st)
                          (objective st)
                          (startingPoints st)
                          (robots st)
                          newCols
  where newCols        = foldr computeCols (collisions st) (robots st)
        computeCols rb = HM.insert (robotId rb) (rbCol rb)
        rbCol rb       = nearestIntersection rb st
