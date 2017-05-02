{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module DobadoBots.InterpreterSpec where

import qualified DobadoBots

import Test.Hspec

spec :: Spec
spec = describe "DobadoBots.parseScript" $ do
         it "should exists" $ do
           DobadoBots.parseScript "ActionToken" `shouldBe` DobadoBots.Token DobadoBots.ActionToken
