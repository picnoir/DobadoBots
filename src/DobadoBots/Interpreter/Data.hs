module DobadoBots.Interpreter.Data
( ActionToken (..)
, SensorToken (..)
, Cond        (..)
)
where

data ActionToken = MoveForward 
                 | TurnLeft
                 | TurnRight
                 | FaceObjective
                 | ChangeObjective
                 deriving (Show, Eq)

data SensorToken = LaserDistance
                 | LaserScan
                 | ObjectiveDistance
                 deriving (Show, Eq)

data Cond = Token ActionToken | Cond { sensor   :: SensorToken
                                     , ifValid   :: Cond
                                     , ifInvalid :: Cond
                                     } deriving (Show, Eq)
