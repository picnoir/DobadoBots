{-# LANGUAGE OverloadedStrings #-}
module DobadoBots.GameEngine.GameEngine (
  gameEngineTick,
  generateGameState
) where

import qualified Data.Sequence           as S  (update, index, singleton)
import           Linear.V2                     (V2(..))
import qualified Linear.Metric           as LM (distance)
import           Data.Maybe                    (maybeToList)
import qualified Data.HashMap.Strict     as HM (insert, HashMap, empty)

import DobadoBots.Interpreter.Data             (Cond(..), ActionToken(..))
import DobadoBots.GameEngine.Data              (GameState(..), Object(..), Robot(..),
                                                RobotId(..),Obstacle(..), Objective(..),
                                                Collision(..), Level(..))
import DobadoBots.GameEngine.Collisions        (nearestIntersection, nearestIntersectionDistance,
                                                nearestDistance)
import DobadoBots.GameEngine.Utils             (getYV2, minTuple, degreeToRadian)


generateGameState :: Level -> GameState
generateGameState l = GameState
                            (lObstacles l)
                            (lArenaSize l)
                            (lObjective l)
                            (lStartingPoints l)
                            (S.singleton $ Robot' "UniqRobot" (head $ lStartingPoints l))
                            HM.empty

gameEngineTick :: GameState -> Cond -> GameState 
gameEngineTick st (Token t) = applyAction t nst 
    where nst = computeCollisions st
gameEngineTick st _         = undefined

-- TODO: look at lenses, there is a way
-- to get rid of the first line using those.
applyAction :: ActionToken -> GameState -> GameState 
applyAction MoveForward = moveRobots
applyAction _           = error "DAFUK IZ DAT TOKEN?"

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
