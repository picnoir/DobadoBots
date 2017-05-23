module DobadoBots.Interpreter.Interpreter (
  interpretScript
) where

import DobadoBots.Interpreter.Data         (Cond(..), ActionToken(..),
                                            SensorToken(..), LogicExpr(..),
                                            CmpInteger(..))
import DobadoBots.GameEngine.Data          (GameState(..), RobotId(..), Collider(..))

import           Data.Maybe                (fromJust)
import qualified Data.HashMap.Strict as HM (lookup)

interpretScript :: GameState -> RobotId -> Cond -> ActionToken 
interpretScript = undefined
