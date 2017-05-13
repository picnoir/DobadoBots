module DobadoBots.GameEngine.GameEngine (
  gameEngineTick
) where

import qualified Data.Sequence as S (update, index)
import Linear.V2 (V2(..))
import Debug.Trace

import DobadoBots.Interpreter.Data (Cond(..), ActionToken(..))
import DobadoBots.GameEngine.Data (GameEngine(..), Object(..), Robot)

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
moveRobot r = trace (show deltaPos) $ Object newPos (size r) (rotation r) rVel 
  where newPos = (position r) + deltaPos
        deltaPos = V2 (rVel * (cos angle)) (rVel * (sin angle)) 
        rVel = velocity r
        angle = degreeToRadian $ rotation r

degreeToRadian :: Float -> Float
degreeToRadian d = d / 180 * pi
