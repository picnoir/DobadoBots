{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (unless)
import Data.Text (Text(..))
import DobadoBots (createMainWindow, closeMainWindow, mainGraphicsLoop)

main :: IO ()
main = do
  (renderer, window) <- createMainWindow "DobadoBots" 
  mainGraphicsLoop renderer 
  closeMainWindow renderer window

