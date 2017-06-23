module DobadoBots.Interpreter.Interpreter (
  interpretScript
) where

import DobadoBots.Interpreter.Data         (Cond(..), ActionToken(..),
                                            SensorToken(..), LogicExpr(..),
                                            CmpInteger(..), Collider(..)) 
import DobadoBots.GameEngine.Data          (GameState(..), RobotId(..),
                                            Collision(..),
                                            Robot(..), Object(..))
import DobadoBots.GameEngine.Collisions    (robotColliderDistance)

import           Data.Maybe                (fromJust)
import qualified Data.HashMap.Strict as HM (lookup)
import qualified Data.Sequence       as S  (filter)
import           Data.Foldable             (toList)
import qualified Data.Vector         as V  (Vector(..))
import           Linear.V2           as LV2(V2(..))

interpretScript :: Cond -> RobotId -> GameState -> ActionToken
interpretScript (Token t) rbId st = t
interpretScript (Cond lExpr ifCond elseCond) rId st = 
  if logicExprEvaluated
    then  interpretScript ifCond rId st
    else interpretScript elseCond rId st
  where collision = fromJust . HM.lookup rId $ collisions st
        robot     = fromJust $ HM.lookup rId $ robots st
        logicExprEvaluated = evaluateLogicExpr lExpr collision robot st

evaluateLogicExpr :: LogicExpr -> Collision -> Robot -> GameState -> Bool
evaluateLogicExpr (CmpCollider LaserScan exCol) (col, _) rb st = exCol == col
evaluateLogicExpr (CmpLogicInt cmp) (_, colCoordinates) rb st = testDistance cmp rb colCoordinates st

testDistance :: CmpInteger SensorToken -> Robot -> V2 Float -> GameState -> Bool
testDistance (Sup token distTest) robot colCoord st = distTest > robotColliderDistance robot cmpCoord 
  where cmpCoord = case token of LaserDistance -> colCoord
                                 ObjectiveDistance -> position $ objective st
testDistance (Inf token distTest) robot colCoord st = distTest < robotColliderDistance robot cmpCoord 
  where cmpCoord = case token of LaserDistance -> colCoord
                                 ObjectiveDistance -> position $ objective st
testDistance (Eq token distTest) robot colCoord st = distTest == robotColliderDistance robot cmpCoord 
  where cmpCoord = case token of LaserDistance -> colCoord
                                 ObjectiveDistance -> position $ objective st
