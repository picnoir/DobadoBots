module DobadoBots.GameEngine.Data (
  Size(..)
, GameEngine(..)
, Object(..)
, Obstacle(..)
, Objective(..)
, StartingPoint(..)
) where
import Linear.V2

type Size = (Float, Float)

data GameEngine = GameEngine {obstacles      :: [Obstacle]
                            , objective      :: Objective
                            , startingPoints :: [StartingPoint]
                            , robots         :: [Robot]

data Object = Object {position :: V2 Float
                    , size     :: Size
                    , rotation :: Float
                    , velocity :: V2 Float} deriving (Show)

type Obstacle = Object

type Objective = Object

type StartingPoint = Object

type Robot = Object 
