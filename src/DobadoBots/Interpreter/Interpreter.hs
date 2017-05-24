module DobadoBots.Interpreter.Interpreter (
  interpretScript
) where

import DobadoBots.Interpreter.Data         (Cond(..), ActionToken(..),
                                            SensorToken(..), LogicExpr(..),
                                            CmpInteger(..))
import DobadoBots.GameEngine.Data          (GameState(..), RobotId(..),
                                            Collider(..), Collision(..),
                                            Robot(..), Object(..))

import           Data.Maybe                (fromJust)
import qualified Data.HashMap.Strict as HM (lookup)
import qualified Data.Sequence       as S  (filter)
import           Data.Foldable             (toList)
import           Linear.V2           as LV2(V2(..))
import qualified Linear.Metric       as LM (distance)

interpretScript :: Cond -> RobotId -> GameState -> ActionToken 
interpretScript (Token t) rbId st = t
interpretScript (Cond lExpr ifCond elseCond) rId st = 
  if evaluateLogicExpr lExpr collision robot  
    then interpretScript ifCond rId st
    else interpretScript elseCond rId st
  where collision = fromJust . HM.lookup rId $ collisions st
        robot :: Robot
        robot = fromJust $ HM.lookup rId $ robots st

evaluateLogicExpr :: LogicExpr -> Collision -> Robot -> Bool
evaluateLogicExpr (CmpCollider LaserScan exCol) (col, _) rb = exCol == col
evaluateLogicExpr (CmpLogicInt (Sup LaserDistance testDistance)) (_, colCoordinates) rb = testDistance > distance rb colCoordinates
evaluateLogicExpr (CmpLogicInt (Eq LaserDistance testDistance)) (_, colCoordinates) rb = testDistance == distance rb colCoordinates
evaluateLogicExpr (CmpLogicInt (Inf LaserDistance testDistance)) (_, colCoordinates) rb = testDistance < distance rb colCoordinates

distance :: Robot -> V2 Float -> Integer
distance rb colPos = floor $ LM.distance colPos (position $ object rb)
