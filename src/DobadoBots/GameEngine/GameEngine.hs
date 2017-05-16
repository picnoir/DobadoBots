module DobadoBots.GameEngine.GameEngine (
  gameEngineTick
) where

import qualified Data.Sequence           as S  (update, index)
import           Linear.V2                     (V2(..))
import qualified Linear.Metric           as LM (distance)
import           Data.Maybe                    (maybeToList)

import DobadoBots.Interpreter.Data (Cond(..), ActionToken(..))
import DobadoBots.GameEngine.Data  (GameState(..), Object(..), Robot, Obstacle(..), Objective(..))
import DobadoBots.GameEngine.Collisions (nearestIntersectionDistance, nearestDistance)
import DobadoBots.GameEngine.Utils      (getYV2, minTuple, degreeToRadian)


gameEngineTick :: GameState -> Cond -> GameState 
gameEngineTick st (Token t) = applyAction t st
gameEngineTick st _ = undefined

-- TODO: look at lenses, there is a way
-- to get rid of the first line using those.
applyAction :: ActionToken -> GameState -> GameState 
applyAction MoveForward = moveRobots
applyAction _ = error "DAFUK IZ DAT TOKEN?"

moveRobots :: GameState -> GameState 
moveRobots st = GameState
                  (obstacles st)
                  (objective st)
                  (startingPoints st)
                  (moveRobot st <$> robots st)

-- TODO: look at lenses, there is a way
-- to get rid of the first line using those.
moveRobot  :: GameState -> Robot -> Robot
moveRobot st r = Object newPos (size r) (rotation r) rVel 
  where newPos     = position r + deltaPos
        deltaPos   = V2 (rVel * cos angle) $ rVel * sin angle
        rVel       = minimum $ velocity r : maybeToList nearestD
        angle      = degreeToRadian $ rotation r
        nearestD   = (rmBotWidth . snd) <$> nearestIntersectionDistance r st
        rmBotWidth = subtract . (/2) . getYV2 $ size r

