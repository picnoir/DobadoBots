{-# LANGUAGE TemplateHaskell #-}

module DobadoBots.GameEngineSpec where

import DobadoBots (loadLevel, Object(..), Level(..), Size(..))

import Test.Hspec (describe, it, Spec(..), shouldBe)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Sequence as S (fromList)
import Data.FileEmbed (embedFile)
import Linear.V2 (V2(..))

spec :: Spec
spec = describe "parseArena" $ 
        it "should parse simple JSON" $ 
          loadLevel (decodeUtf8 $(embedFile "test/fixtures/levels/level1.json")) `shouldBe` Right(
           Level
              [Object (V2 12.3 13.2) (V2 10.0 12.0) 0 0 ]
              (V2 640 480)
              (Object (V2 50 50) (V2 10 12) 0.0 0 )
              [Object (V2 0 0) (V2 10 12) 0 0 ]
            )
            
