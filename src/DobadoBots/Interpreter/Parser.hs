module DobadoBots.Interpreter.Parser(
  parseScript 
) where

import DobadoBots.Interpreter.Data (Cond(..), ActionToken(..),
                                    SensorToken(..), LogicExpr(..))

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
           <|> ObjectiveDistance <$ string "objectiveDistance"
           <|> try (LaserScan <$ string "laserScan")

logicExprParser :: CharParser () (LogicExpr SensorToken)
logicExprParser = op '=' Eq
              <|> op '<' Inf
              <|> op '>' Sup
    where
      op c cons = try (cons <$>
                    (spaces *> sensorParser)
                  <*
                    (spaces *> char c) 
                  <*> 
                    (spaces *> (read <$> many1 digit)))


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
