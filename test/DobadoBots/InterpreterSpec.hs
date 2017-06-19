{-# LANGUAGE OverloadedStrings #-}

module DobadoBots.InterpreterSpec (spec) where

import DobadoBots

import Data.FileEmbed
import Data.Text.Encoding
import Test.Hspec
import Test.Hspec.Expectations.Contrib (isLeft) 
import Linear.V2
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as S

spec :: Spec
spec = describe "interpretScript" $ do
        it "should move the robot forward" $
          interpretScript (Cond (CmpLogicInt $ Eq LaserDistance 1) (Token TurnLeft) (Token MoveForward)) "UniqRobot" gameStateNotAgainstObstacle `shouldBe` MoveForward
        it "should turn the robot left" $
          interpretScript (Cond (CmpLogicInt $ Inf LaserDistance 5) (Token TurnLeft) (Token MoveForward)) "UniqRobot" gameStateAgainstObstacle `shouldBe` TurnLeft 
        it "should check front kind of obstacle correctly" $
          interpretScript (Cond (CmpCollider LaserScan Objective) (Token MoveForward) (Token TurnLeft)) "UniqRobot" gameStateNotAgainstObstacle `shouldBe` TurnLeft
        it "should handle objective distance" $
          interpretScript (Cond (CmpLogicInt $ Inf ObjectiveDistance 1) (Token MoveForward) (Token TurnLeft)) "UniqRobot" gameStateNotAgainstObstacle `shouldBe` MoveForward









gameStateAgainstObstacle :: GameState
gameStateAgainstObstacle = GameState {obstacles = [Object {position = V2 200.0 250.0, size = V2 200.0 20.0, rotation = 0.0, defaultVelocity = 0.0, currentVelocity = 0.0},Object {position = V2 200.0 280.0, size = V2 200.0 20.0, rotation = 0.0, defaultVelocity = 0.0, currentVelocity = 0.0}], arenaSize = V2 640.0 480.0, objective = Object {position = V2 450.0 50.0, size = V2 10.0 12.0, rotation = 0.0,defaultVelocity = 0.0, currentVelocity = 0.0}, startingPoints = [Object {position = V2 300.0 450.0, size = V2 20.0 20.0, rotation = -90.0, defaultVelocity = 1.0, currentVelocity = 1.0}], robots = HM.fromList [("UniqRobot",Robot' {robotId = "UniqRobot", object = Object {position = V2 300.0 300.0, size = V2 20.0 20.0, rotation = -90.0, defaultVelocity = 1.0, currentVelocity = 1.0}})], phase=Running, collisions = HM.fromList [("UniqRobot",(Obstacle,V2 310.0 300.0))]}
  
gameStateNotAgainstObstacle :: GameState
gameStateNotAgainstObstacle =  GameState {obstacles = [Object {position = V2 200.0 250.0, size = V2 200.0 20.0, rotation = 0.0, defaultVelocity = 0.0, currentVelocity = 0.0},Object {position = V2 200.0 280.0, size = V2 200.0 20.0, rotation = 0.0, defaultVelocity = 0.0, currentVelocity = 0.0}], arenaSize = V2 640.0 480.0, objective = Object {position = V2 450.0 50.0, size = V2 10.0 12.0, rotation = 0.0, defaultVelocity = 0.0, currentVelocity = 0.0}, startingPoints = [Object {position = V2 300.0 450.0, size= V2 20.0 20.0, rotation = -90.0, defaultVelocity = 1.0, currentVelocity = 1.0}], robots = HM.fromList [("UniqRobot", Robot'{robotId = "UniqRobot", object = Object {position = V2 300.0 381.0, size = V2 20.0 20.0, rotation = -90.0, defaultVelocity = 1.0, currentVelocity = 1.0}})], phase=Running,collisions = HM.fromList [("UniqRobot",(Obstacle,V2 310.0 300.0))]}
