module DobadoBots.Interpreter.Data
( ActionToken   ( ..)
, SensorToken   ( ..)
, Cond          ( ..)
, LogicExpr     ( ..)
, CmpInteger    ( ..)
, Collider      ( ..)
)
where

import Data.HashMap.Strict        (HashMap)

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

data LogicExpr = CmpCollider SensorToken Collider | CmpLogicInt (CmpInteger SensorToken) deriving (Show, Eq)

data CmpInteger a = Sup a Integer
                  | Inf a Integer
                  | Eq a Integer
                 deriving (Show, Eq)

data Cond = Token ActionToken | Cond { sensor    :: LogicExpr 
                                     , ifValid   :: Cond
                                     , ifInvalid :: Cond
                                     } deriving (Show, Eq)

data Collider = Obstacle | Objective | Wall | Robot deriving (Show, Eq)
