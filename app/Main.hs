{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (unless)
import Data.Text (Text(..))
import DobadoBots (createMainWindow, closeMainWindow, mainGraphicsLoop, GameEngine(..))

-- TODO write new event loop.
engineState :: GameEngine
engineState = undefined

main :: IO ()
main = do
  (renderer, window) <- createMainWindow "DobadoBots" 
  mainGraphicsLoop renderer engineState
  closeMainWindow renderer window

