{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module DobadoBots.InterpreterSpec where

import DobadoBots

import Data.FileEmbed
import Data.Text.Encoding
import Test.Hspec


spec :: Spec
spec = describe "parseScript" $ do
         it "should parse move forward" $ do
           parseScript "moveForward" `shouldBe` Token MoveForward
         it "should parse turn left" $ do
           parseScript "turnLeft" `shouldBe` Token TurnLeft
         it "should parse turn right" $ do
           parseScript "turnRight" `shouldBe` Token TurnRight
         it "should parse face objective" $ do
           parseScript "faceObjective" `shouldBe` Token FaceObjective
         it "should parse change objective" $ do
           parseScript "changeObjective" `shouldBe` Token ChangeObjective
         it "should parse simple cond" $ do
           parseScript  (decodeUtf8 $(embedFile "test/fixtures/simplecond.script")) `shouldBe` Cond LaserDistance (Token TurnLeft) (Token MoveForward)
