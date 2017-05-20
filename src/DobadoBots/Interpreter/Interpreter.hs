module DobadoBots.Interpreter.Interpreter (

) where

import DobadoBots.Interpreter.Data         (Cond(..), ActionToken(..),
                                            SensorToken(..), LogicExpr(..))
import DobadoBots.GameEngine.Data          (GameState(..), Robot(..), Collider(..))

import           Data.Maybe             (fromJust)
import qualified Data.HashMap.Strict as HM (lookup)

interpretScript :: Cond -> GameState -> Robot -> ActionToken 
interpretScript (Token actionToken) st rb = actionToken
interpretScript (Cond logicExpr sbThen sbElse) st rb = 
  if evalLogicExpr logicExpr
    then interpretScript sbThen st rb
    else interpretScript sbElse st rb
  where evalLogicExpr (Sup sensor val) = getSensorVal sensor st rb > val 
        evalLogicExpr (Inf sensor val) = getSensorVal sensor st rb < val
        evalLogicExpr (Eq  sensor val) = getSensorVal sensor st rb == val

getSensorVal :: SensorToken -> GameState -> Robot -> Collider
getSensorVal LaserScan st rb = fromJust $ fst <$> getCollisions st 
  where getCollisions rb = HM.lookup (robotId rb) $ collisions st
  
