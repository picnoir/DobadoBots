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
           parseScript (decodeUtf8 $(embedFile "test/fixtures/simplecond.script")) `shouldBe` Right (Cond (Eq LaserDistance 1)(Token TurnLeft) (Token MoveForward))
         it "should parse simple cond >" $ do
           parseScript (decodeUtf8 $(embedFile "test/fixtures/simplecond2.script")) `shouldBe` Right (Cond (Sup LaserDistance 1)(Token TurnLeft) (Token MoveForward))
         it "should parse simple cond <" $ do
           parseScript (decodeUtf8 $(embedFile "test/fixtures/simplecond3.script")) `shouldBe` Right (Cond (Inf LaserDistance 1)(Token TurnLeft) (Token MoveForward))
         it "should parse weirdly indented files" $ do
           parseScript (decodeUtf8 $(embedFile "test/fixtures/simpleBadIndented.script")) `shouldBe` Right (Cond (Inf LaserDistance 1)(Token TurnLeft) (Token MoveForward))
         it "should parse tab indented files" $ do
           parseScript (decodeUtf8 $(embedFile "test/fixtures/simpleTabIndented.script")) `shouldBe` Right (Cond (Inf LaserDistance 1)(Token TurnLeft) (Token MoveForward))
         it "should parse nested cond" $ do
           parseScript (decodeUtf8 $(embedFile "test/fixtures/nestedcond.script")) `shouldBe` Right (Cond (Sup LaserDistance 10) (Cond (Sup LaserDistance 10) (Token TurnLeft) (Token TurnRight)) (Token MoveForward))
         it "should fail when no operator in condition"$ do
           parseScript (decodeUtf8 $(embedFile "test/fixtures/simpleconderr1.script")) `shouldSatisfy` isLeft 
         it "should fail when no success action"$ do
           parseScript (decodeUtf8 $(embedFile "test/fixtures/simpleconderr2.script")) `shouldSatisfy` isLeft 
         it "should fail when no else clause"$ do
           parseScript (decodeUtf8 $(embedFile "test/fixtures/simpleconderr3.script")) `shouldSatisfy` isLeft 
