{-# LANGUAGE OverloadedStrings #-}

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

import Linear.V2 (V2(..))
import Data.Aeson (FromJSON(..), withObject, (.:)) 
import qualified Data.Aeson.Types as AT (Parser, Object(..), Value(..)) 
import qualified Data.HashMap.Lazy as HS (lookup)

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

instance FromJSON GameEngine where
  parseJSON = withObject "GameEngine" $ \v -> GameEngine
    <$> v .: "obstacles"
    <*> v .: "objective"
    <*> v .: "startingpoints"
    <*> v .: "startingpoints"

instance FromJSON Object where
    parseJSON = withObject  "Object" $ \v -> Object
      <$> parsePosition (HS.lookup "position" v) 
      <*> parseSize (HS.lookup "size" v) 
      <*> v .: "rotation"
      <*> parseVelocity (HS.lookup "velocity" v) 

parsePosition :: Maybe (AT.Value) -> AT.Parser (V2 Float)
parsePosition (Just (AT.Object v)) = V2 <$> v .: "x" <*> v .: "y"
parsePosition _ = fail "No position object."

parseSize :: Maybe (AT.Value) -> AT.Parser (V2 Float)
parseSize (Just(AT.Object v)) = V2 <$> v .: "width" <*> v .: "height"
parseSize _ = fail "No size object."

parseVelocity :: Maybe (AT.Value)-> AT.Parser (V2 Float)
parseVelocity = parsePosition
