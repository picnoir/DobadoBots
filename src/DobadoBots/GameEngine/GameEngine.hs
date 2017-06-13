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
import qualified Data.SG.Geometry.TwoDim as G2 (Rel2'(..), makeRel2, toAngle) 

import DobadoBots.Interpreter.Data             (Cond(..), ActionToken(..))
import DobadoBots.GameEngine.Data              (GameState(..), Object(..), Robot(..),
                                                RobotId(..),Obstacle(..), Objective(..),
                                                Collision(..), Level(..), GamePhase(..))
import DobadoBots.GameEngine.Collisions        (nearestIntersection, nearestIntersectionDistance,
                                                nearestDistance, robotObjectiveIntersection)
import DobadoBots.GameEngine.Utils             (getXV2, getYV2, minTuple, degreeToRadian, radianToDegree)
import DobadoBots.Interpreter.Interpreter      (interpretScript) 

generateGameState :: Level -> GameState
generateGameState l = GameState
                            (lObstacles l)
                            (lArenaSize l)
                            (lObjective l)
                            (lStartingPoints l)
                            (HM.fromList[("UniqRobot",Robot' "UniqRobot" (head $ lStartingPoints l))])
                            Editing
                            HM.empty

gameEngineTick :: GameState -> Cond -> GameState 
gameEngineTick st ast = if not $ robotObjectiveIntersection "UniqRobot" st
                        then applyAction actionToken "UniqRobot" nst 
                        else setGameStateWin st 
    where nst         = computeCollisions st
          actionToken = interpretScript ast "UniqRobot" nst

-- TODO: look at lenses, there is a way
-- to get rid of the first line using those.
applyAction :: ActionToken -> RobotId -> GameState -> GameState 
applyAction MoveForward rId st   = moveRobots st
applyAction TurnLeft rId st      = rotateRobot (-1) rId True st
applyAction TurnRight rId st     = rotateRobot 1 rId True st
applyAction FaceObjective rId st = rotateRobot objRot rId False st
  where objRot  = radianToDegree $ G2.toAngle diffRel
        diffRel = G2.makeRel2 (x,y)
        x       = getXV2 diffVec
        y       = getYV2 diffVec
        diffVec = objPos - robPos
        objPos  = position $ objective st
        robPos  = position . object . fromJust . HM.lookup rId $ robots st
applyAction _ _ _             = error "DAFUK IZ DAT TOKEN?"

rotateRobot :: Float -> RobotId -> Bool -> GameState -> GameState
rotateRobot angle rId isRel st = GameState 
                                  (obstacles st)
                                  (arenaSize st)
                                  (objective st)
                                  (startingPoints st)
                                  (HM.insert rId newRobot $ robots st)
                                  (phase st)
                                  (collisions st)
  where robot    = fromJust . HM.lookup rId $ robots st
        nAngle   = if isRel
                   then angle + rotation (object robot)
                   else angle
        newRobot = Robot' (robotId robot) (Object
                                    (position $ object robot)
                                    (size $ object robot)
                                    nAngle
                                    (velocity $ object robot))

moveRobots :: GameState -> GameState 
moveRobots st = GameState
                  (obstacles st)
                  (arenaSize st)
                  (objective st)
                  (startingPoints st)
                  (moveRobot st <$> robots st)
                  (phase st)
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
                          (phase st)
                          newCols
  where newCols        = foldr computeCols (collisions st) (robots st)
        computeCols rb = HM.insert (robotId rb) (rbCol rb)
        rbCol rb       = nearestIntersection rb st

setGameStateWin :: GameState -> GameState
setGameStateWin st = GameState
                       (obstacles st)
                       (arenaSize st)
                       (objective st)
                       (startingPoints st)
                       (robots st)
                       Win
                       (collisions st)
