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
         it "should parse move forward" $ do
           parseScript "moveForward" `shouldBe` Right (Token MoveForward)
         it "should parse turn left" $ do
           parseScript "turnLeft" `shouldBe` Right (Token TurnLeft)
         it "should parse turn right" $ do
           parseScript "turnRight" `shouldBe` Right (Token TurnRight)
         it "should parse face objective" $ do
           parseScript "faceObjective" `shouldBe` Right (Token FaceObjective)
         it "should parse change objective" $ do
           parseScript "changeObjective" `shouldBe` Right (Token ChangeObjective)
         it "should parse simple cond =" $ do
           parseScript (decodeUtf8 $(embedFile "test/fixtures/script/simplecond.script")) `shouldBe` Right (Cond (Eq LaserDistance 1)(Token TurnLeft) (Token MoveForward))
         it "should parse simple cond >" $ do
           parseScript (decodeUtf8 $(embedFile "test/fixtures/script/simplecond2.script")) `shouldBe` Right (Cond (Sup LaserScan 1)(Token TurnLeft) (Token MoveForward))
         it "should parse simple cond <" $ do
           parseScript (decodeUtf8 $(embedFile "test/fixtures/script/simplecond3.script")) `shouldBe` Right (Cond (Inf LaserDistance 1)(Token TurnLeft) (Token MoveForward))
         it "should parse weirdly indented files" $ do
           parseScript (decodeUtf8 $(embedFile "test/fixtures/script/simpleBadIndented.script")) `shouldBe` Right (Cond (Inf LaserDistance 1)(Token TurnLeft) (Token MoveForward))
         it "should parse tab indented files" $ do
           parseScript (decodeUtf8 $(embedFile "test/fixtures/script/simpleTabIndented.script")) `shouldBe` Right (Cond (Inf LaserDistance 1)(Token TurnLeft) (Token MoveForward))
         it "should parse nested if cond" $ do
           parseScript (decodeUtf8 $(embedFile "test/fixtures/script/nestedcond.script")) `shouldBe` Right (Cond (Sup LaserDistance 10) (Cond (Sup LaserDistance 20) (Token TurnLeft) (Token TurnRight)) (Token MoveForward))
         it "should parse nested else cond" $ do
           parseScript (decodeUtf8 $(embedFile "test/fixtures/script/nestedcond2.script")) `shouldBe` Right (Cond (Sup LaserDistance 10)  (Token TurnLeft) (Cond (Sup LaserDistance 20) (Token TurnLeft) (Token TurnRight)))
         it "should parse deeply nested else cond" $ do
           parseScript (decodeUtf8 $(embedFile "test/fixtures/script/deepNestedCond.script")) `shouldBe` Right (Cond (Sup LaserDistance 10)  (Token TurnLeft) (Cond (Sup LaserDistance 20) (Cond (Inf ObjectiveDistance 3) (Token MoveForward) (Token TurnLeft)) (Token TurnRight)))
         it "should fail when no operator in condition"$ do
           parseScript (decodeUtf8 $(embedFile "test/fixtures/script/simpleconderr1.script")) `shouldSatisfy` isLeft 
         it "should fail when no success action"$ do
           parseScript (decodeUtf8 $(embedFile "test/fixtures/script/simpleconderr2.script")) `shouldSatisfy` isLeft 
         it "should fail when no else clause"$ do
           parseScript (decodeUtf8 $(embedFile "test/fixtures/script/simpleconderr3.script")) `shouldSatisfy` isLeft 
