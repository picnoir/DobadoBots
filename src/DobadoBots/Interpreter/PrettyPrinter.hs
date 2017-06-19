module DobadoBots.Interpreter.PrettyPrinter(
 prettyPrint 
) where

import Data.Text                   (Text(..), pack)
import Text.PrettyPrint            (Doc(..), text, (<>), ($$),
                                    (<+>), nest, render)

import DobadoBots.Interpreter.Data (Cond(..), LogicExpr(..),
                                    CmpInteger(..), SensorToken(..),
                                    ActionToken(..))
import DobadoBots.GameEngine.Data  (Collider(..))
prettyPrint :: Cond -> Text 
prettyPrint = pack . render . generateDoc 

generateDoc :: Cond -> Doc
generateDoc (Token MoveForward)         = text "moveForward"
generateDoc (Token TurnLeft)            = text "turnLeft"
generateDoc (Token TurnRight)           = text "turnRight"
generateDoc (Token FaceObjective)       = text "faceObjective"
generateDoc (Token ChangeObjective)     = text "changeObjective"
generateDoc (Cond sensor valid invalid) = text "IF" <+> logicExprToDoc sensor $$ nest 3 (generateDoc valid) $$ text "ELSE" $$ nest 3 (generateDoc invalid)

logicExprToDoc :: LogicExpr -> Doc
logicExprToDoc (CmpCollider sensor collider) = prettyPrintSensorToken sensor <+> text "=" <+> prettyPrintCollider collider
logicExprToDoc (CmpLogicInt cmp) = cmpIntegerToDoc cmp

cmpIntegerToDoc :: CmpInteger SensorToken -> Doc
cmpIntegerToDoc (Sup sensor i) = prettyPrintSensorToken sensor <+> text "<" <+> text (show i)
cmpIntegerToDoc (Eq sensor i)  = prettyPrintSensorToken sensor <+> text "= " <+> text (show i)
cmpIntegerToDoc (Inf sensor i) = prettyPrintSensorToken sensor <+> text ">" <+> text (show i)

prettyPrintSensorToken :: SensorToken -> Doc
prettyPrintSensorToken LaserScan         = text "laserScan"
prettyPrintSensorToken LaserDistance     = text "laserDistance"
prettyPrintSensorToken ObjectiveDistance = text "objectiveDistance"

prettyPrintCollider :: Collider -> Doc
prettyPrintCollider Obstacle  = text "obstacle"
prettyPrintCollider Objective = text "objective"
prettyPrintCollider Wall      = text "wall"
prettyPrintCollider Robot     = text "robot"

