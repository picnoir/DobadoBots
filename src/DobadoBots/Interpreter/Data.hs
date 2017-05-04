module DobadoBots.Interpreter.Data
( ActionToken (..)
, SensorToken (..)
, Cond        (..)
, LogicExpr   (..)
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

data LogicExpr a = Sup a Integer
                 | Inf a Integer
                 | Eq a Integer
                 deriving (Show, Eq)

data Cond = Token ActionToken | Cond { sensor    :: LogicExpr SensorToken
                                     , ifValid   :: Cond
                                     , ifInvalid :: Cond
                                     } deriving (Show, Eq)
