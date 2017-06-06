module DobadoBots.Interpreter.PrettyPrinter(
 prettyPrint 
) where

import Data.Text                   (Text(..), pack)
import Text.PrettyPrint            (Doc(..), text, (<>), ($$),
                                    (<+>), nest, render)

import DobadoBots.Interpreter.Data (Cond(..), LogicExpr(..),
                                    CmpInteger(..), SensorToken(..))

prettyPrint :: Cond -> Text 
prettyPrint = pack . render . generateDoc 

generateDoc :: Cond -> Doc
generateDoc (Token token) = text $ show token
generateDoc (Cond sensor valid invalid) = text "If" <+> logicExprToDoc sensor $$ (nest 3 $ generateDoc valid) $$ text "Else" $$ (nest 3 $ generateDoc invalid )

logicExprToDoc :: LogicExpr -> Doc
logicExprToDoc (CmpCollider sensor collider) = (text $ show sensor) <+> text "=" <+> (text $ show collider)
logicExprToDoc (CmpLogicInt (cmp)) = cmpIntegerToDoc cmp

cmpIntegerToDoc :: CmpInteger SensorToken -> Doc
cmpIntegerToDoc (Sup sensor i) = (text $ show sensor) <+> text ">" <+> (text $ show i)
cmpIntegerToDoc (Eq sensor i) = (text $ show sensor) <+> text "=" <+> (text $ show i)
cmpIntegerToDoc (Inf sensor i) = (text $ show sensor) <+> text "<" <+> (text $ show i)

