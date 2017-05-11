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
import qualified Data.Aeson.Types as AT (Parser, Object) 

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
    <*> v .: "objectives"
    <*> v .: "startingPoints"
    <*> pure []

instance FromJSON Object where
    parseJSON = withObject  "Object" $ \v -> Object
      <$> parsePosition v
      <*> parseSize v
      <*> v .: "rotation"
      <*> parseVelocity v

parsePosition :: AT.Object -> AT.Parser (V2 Float)
parsePosition v = V2 <$> v .: "x" <*> v .: "y"

parseSize :: AT.Object -> AT.Parser (V2 Float)
parseSize v = V2 <$> v .: "width" <*> v .: "heigt"

parseVelocity :: AT.Object -> AT.Parser (V2 Float)
parseVelocity = parsePosition
