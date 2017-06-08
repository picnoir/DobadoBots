module DobadoBots.Interpreter.Data
( ActionToken   ( ..)
, SensorToken   ( ..)
, Cond          ( ..)
, LogicExpr     ( ..)
, CmpInteger    ( ..)
, CondEvaluated ( ..)
, fillCondEval
)
where

import DobadoBots.GameEngine.Data (Collider(..))

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

data CondEvaluated = EvaluatedToken (ActionToken, Bool)
                   | CondEvaluated LogicExpr (CondEvaluated, Bool) (CondEvaluated, Bool)

fillCondEval :: Bool -> Cond -> CondEvaluated
fillCondEval b (Token action) = EvaluatedToken (action, b)
fillCondEval b (Cond sens valCond invalCond) = CondEvaluated sens (fillCondEval b valCond, b) (fillCondEval b valCond, b)
