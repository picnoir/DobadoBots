{-# LANGUAGE OverloadedStrings #-}

module DobadoBots.InterpreterSpec where

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
          interpretScript gameStateNotAgainstObstacle "UniqRobot" (Cond (CmpLogicInt $ Eq LaserDistance 1) (Token TurnLeft) (Token MoveForward)) `shouldBe` MoveForward
        it "should turn the robot left" $
          interpretScript gameStateAgainstObstacle "UniqRobot" (Cond (CmpLogicInt $ Eq LaserDistance 1) (Token TurnLeft) (Token MoveForward)) `shouldBe` TurnLeft 
        it "should check front kind of obstacle correctly" $
          interpretScript gameStateNotAgainstObstacle "UniqRobot" (Cond (CmpCollider LaserScan Objective) (Token MoveForward) (Token TurnLeft)) `shouldBe` TurnLeft
        it "should handle objective distance" $
          interpretScript gameStateNotAgainstObstacle "UniqRobot" (Cond (CmpLogicInt $ Sup ObjectiveDistance 20) (Token MoveForward) (Token TurnLeft)) `shouldBe` MoveForward









gameStateAgainstObstacle :: GameState
gameStateAgainstObstacle = GameState {obstacles = [Object {position = V2 200.0 250.0, size = V2 200.0 20.0, rotation = 0.0, velocity = 0.0},Object {position = V2 200.0 280.0, size = V2 200.0 20.0, rotation = 0.0, velocity = 0.0}], arenaSize = V2 640.0 480.0, objective = Object {position = V2 450.0 50.0, size = V2 10.0 12.0, rotation = 0.0, velocity = 0.0}, startingPoints = [Object {position = V2 300.0 450.0, size = V2 20.0 20.0, rotation = -90.0, velocity = 1.0}], robots = S.fromList [Robot' {robotId = "UniqRobot", object = Object {position = V2 300.0 300.0, size = V2 20.0 20.0, rotation = -90.0, velocity = 0.0}}], collisions = HM.fromList [("UniqRobot",(Obstacle,V2 310.0 300.0))]}
  
gameStateNotAgainstObstacle :: GameState
gameStateNotAgainstObstacle =  GameState {obstacles = [Object {position = V2 200.0 250.0, size = V2 200.0 20.0, rotation = 0.0, velocity = 0.0},Object {position = V2 200.0 280.0, size = V2 200.0 20.0, rotation = 0.0, velocity = 0.0}], arenaSize = V2 640.0 480.0, objective = Object {position = V2 450.0 50.0, size = V2 10.0 12.0, rotation = 0.0, velocity = 0.0}, startingPoints = [Object {position = V2 300.0 450.0, size= V2 20.0 20.0, rotation = -90.0, velocity = 1.0}], robots = S.fromList [Robot'{robotId = "UniqRobot", object = Object {position = V2 300.0 381.0, size = V2 20.0 20.0, rotation = -90.0, velocity = 1.0}}], collisions = HM.fromList [("UniqRobot",(Obstacle,V2 310.0 300.0))]}
