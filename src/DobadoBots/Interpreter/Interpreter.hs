module DobadoBots.Interpreter.Interpreter(
  parseScript 
) where

import DobadoBots.Interpreter.Data

import Data.Text
import Text.ParserCombinators.Parsec

scriptFile :: CharParser () Cond
scriptFile = Token <$> actionParser
         <|> conditionParser

actionParser:: CharParser () ActionToken
actionParser = MoveForward     <$ string "moveForward"
          <|>  try (TurnLeft   <$ string "turnLeft")
          <|>  try (TurnRight  <$ string "turnRight")
          <|>  FaceObjective   <$ string "faceObjective"
          <|>  ChangeObjective <$ string "changeObjective"

sensorParser :: CharParser () SensorToken
sensorParser = LaserDistance <$ string "laserDistance" 

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
                    (string "IF" *> logicExprParser <* newline)
                  <*>
                    (spaces *> (Token <$> actionParser) <* newline)
                  <*>
                    (string "ELSE" *> (newline *> (spaces *> (Token <$> actionParser))))


parseScript :: Text -> Either ParseError Cond 
parseScript script = parse scriptFile "Error while parsing script: " $ unpack script 
