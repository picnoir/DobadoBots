{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module DobadoBots.InterpreterSpec where

import DobadoBots

import Data.FileEmbed
import Data.Text.Encoding
import Test.Hspec
import Test.Hspec.Expectations.Contrib (isLeft) 

spec :: Spec
spec = describe "parseScript" $ do
         it "should parse move forward" $ 
           parseScript "moveForward" `shouldBe` Right (Token MoveForward)
         it "should parse turn left" $ 
           parseScript "turnLeft" `shouldBe` Right (Token TurnLeft)
         it "should parse turn right" $ 
           parseScript "turnRight" `shouldBe` Right (Token TurnRight)
         it "should parse face objective" $ 
           parseScript "faceObjective" `shouldBe` Right (Token FaceObjective)
         it "should parse change objective" $ 
           parseScript "changeObjective" `shouldBe` Right (Token ChangeObjective)
         it "should parse simple cond =" $ 
           parseScript (decodeUtf8 $(embedFile "test/fixtures/script/simplecond.script")) `shouldBe` Right (Cond (CmpLogicInt $ Eq LaserDistance 1)(Token TurnLeft) (Token MoveForward))
         it "should parse simple cond >" $
           parseScript (decodeUtf8 $(embedFile "test/fixtures/script/simplecond2.script")) `shouldBe` Right (Cond (CmpLogicInt $ Sup LaserScan 1)(Token TurnLeft) (Token MoveForward))
         it "should parse simple cond <" $ 
           parseScript (decodeUtf8 $(embedFile "test/fixtures/script/simplecond3.script")) `shouldBe` Right (Cond (CmpLogicInt $ Inf LaserDistance 1)(Token TurnLeft) (Token MoveForward))
         it "should parse collider " $
           parseScript (decodeUtf8 $(embedFile "test/fixtures/script/simplecondcol.script")) `shouldBe` Right (Cond (CmpCollider LaserScan Wall) (Token TurnLeft) (Token MoveForward))
         it "should parse weirdly indented files" $ 
           parseScript (decodeUtf8 $(embedFile "test/fixtures/script/simpleBadIndented.script")) `shouldBe` Right (Cond (CmpLogicInt $ Inf LaserDistance 1)(Token TurnLeft) (Token MoveForward))
         it "should parse tab indented files" $ 
           parseScript (decodeUtf8 $(embedFile "test/fixtures/script/simpleTabIndented.script")) `shouldBe` Right (Cond (CmpLogicInt $ Inf LaserDistance 1)(Token TurnLeft) (Token MoveForward))
         it "should parse nested if cond" $ 
           parseScript (decodeUtf8 $(embedFile "test/fixtures/script/nestedcond.script")) `shouldBe` Right (Cond (CmpLogicInt $ Sup LaserDistance 10) (Cond (CmpLogicInt $ Sup LaserDistance 20) (Token TurnLeft) (Token TurnRight)) (Token MoveForward))
         it "should parse nested else cond" $ 
           parseScript (decodeUtf8 $(embedFile "test/fixtures/script/nestedcond2.script")) `shouldBe` Right (Cond (CmpLogicInt $ Sup LaserDistance 10)  (Token TurnLeft) (Cond (CmpLogicInt $ Sup LaserDistance 20) (Token TurnLeft) (Token TurnRight)))
         it "should parse deeply nested else cond" $ 
           parseScript (decodeUtf8 $(embedFile "test/fixtures/script/deepNestedCond.script")) `shouldBe` Right (Cond (CmpLogicInt $ Sup LaserDistance 10)  (Token TurnLeft) (Cond (CmpLogicInt $ Sup LaserDistance 20) (Cond (CmpLogicInt $ Inf ObjectiveDistance 3) (Token MoveForward) (Token TurnLeft)) (Token TurnRight)))
         it "should fail when no operator in condition"$ 
           parseScript (decodeUtf8 $(embedFile "test/fixtures/script/simpleconderr1.script")) `shouldSatisfy` isLeft 
         it "should fail when no success action"$ 
           parseScript (decodeUtf8 $(embedFile "test/fixtures/script/simpleconderr2.script")) `shouldSatisfy` isLeft 
         it "should fail when no else clause"$ 
           parseScript (decodeUtf8 $(embedFile "test/fixtures/script/simpleconderr3.script")) `shouldSatisfy` isLeft 
