module DobadoBots.Interpreter.Parser(
  parseScript 
) where

import DobadoBots.Interpreter.Data (Cond(..), ActionToken(..),
                                    SensorToken(..), LogicExpr(..),
                                    CmpInteger(..))
import DobadoBots.GameEngine.Data  (Collider(..))

import Data.Text (unpack, Text(..))
import Text.ParserCombinators.Parsec (CharParser, ParseError, (<|>),
                                      string, spaces, parse, many1,
                                      digit, newline, char, try)


scriptFile :: CharParser () Cond
scriptFile = Token <$> actionParser
         <|> conditionParser

actionParser :: CharParser () ActionToken
actionParser = MoveForward     <$ string "moveForward"
          <|>  try (TurnLeft   <$ string "turnLeft")
          <|>  try (TurnRight  <$ string "turnRight")
          <|>  FaceObjective   <$ string "faceObjective"
          <|>  ChangeObjective <$ string "changeObjective"

sensorParser :: CharParser () SensorToken
sensorParser = try (LaserDistance <$ string "laserDistance")
           <|> ObjectiveDistance  <$ string "objectiveDistance"
           <|> try (LaserScan     <$ string "laserScan")

cmpIntegerParser :: CharParser () (CmpInteger SensorToken)
cmpIntegerParser = op '=' Eq
               <|> op '<' Inf
               <|> op '>' Sup
    where
      op c cons = try (cons <$>
                    (spaces *> sensorParser)
                  <*
                    (spaces *> char c) 
                  <*> 
                    (spaces *> (read <$> many1 digit)))

colliderParser :: CharParser () Collider
colliderParser = try (Obstacle  <$ string "obstacle")
             <|> try (Objective <$ string "objective")
             <|> try (Wall      <$ string "wall")
             <|> try (Robot     <$ string "robot")

cmpCollider :: CharParser () LogicExpr
cmpCollider = CmpCollider <$> 
                (spaces *> sensorParser)
              <*>
                (spaces *> string "==" *> spaces *> colliderParser)

logicExprParser :: CharParser () LogicExpr
logicExprParser = try cmpCollider
              <|> try (CmpLogicInt <$> cmpIntegerParser)

conditionParser :: CharParser () Cond
conditionParser = Cond <$>
                    ((spaces *> string "IF") *> logicExprParser <* newline)
                  <*>
                    (spaces *> parseNested <* newline)
                  <*>
                    ((spaces *> string "ELSE") *> (newline *> (spaces *> parseNested)))
                  where
                    parseNested = Token <$> actionParser
                              <|> conditionParser 
parseScript :: Text -> Either ParseError Cond 
parseScript script = parse scriptFile "Error while parsing script: " $ unpack script 
