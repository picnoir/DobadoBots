module DobadoBots.Interpreter.Data
( ActionToken      (..)
, SensorToken (..) 
, Cond        (..)
)
where

data ActionToken = ActionToken deriving (Show, Eq)

data SensorToken = SensorToken deriving (Show, Eq)

data Cond = Token ActionToken | Cond { sensor         :: SensorToken
                                    , ifValid        :: Cond 
                                    , ifInvalid      :: Cond 
                                    } deriving (Show, Eq)

