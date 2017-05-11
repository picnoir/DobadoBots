module DobadoBots.GameEngine.Data (
  Size(..)
, Position(..)
, Velocity(..)
, GameEngine(..)
, Object(..)
, Obstacle(..)
, Objective(..)
, StartingPoint(..)
) where
import Linear.V2

type Size = V2 Float 

type Position = V2 Float

type Velocity = V2 Float

data GameEngine = GameEngine {obstacles      :: [Obstacle]
                            , objective      :: Objective
                            , startingPoints :: [StartingPoint]
                            , robots         :: [Robot]} deriving (Show, Eq)

data Object = Object {position :: Position
                    , size     :: Size
                    , rotation :: Float
                    , velocity :: Velocity} deriving (Show, Eq)

type Obstacle = Object

type Objective = Object

type StartingPoint = Object

type Robot = Object 
