module DobadoBots.Interpreter.Data
( ActionToken      (..)
, SensorToken (..) 
, Cond        (..)
)
where

data ActionToken = ActionToken deriving (Show)

data SensorToken = SensorToken deriving (Show)

data Cond = Cond { sensor         :: SensorToken
                ,  ifValid        :: ActionToken
                ,  ifInvalid      :: ActionToken
                } deriving (Show)
