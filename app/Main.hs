{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (unless)
import Data.Text (Text(..))
import qualified Data.Text.IO as TIO (readFile)
import DobadoBots (createMainWindow, closeMainWindow, mainGraphicsLoop, GameEngine(..), loadLevel)
import qualified SDL (EventPayload(..), eventPayload, pollEvents, Renderer)

main :: IO ()
main = do
  levelStr <- TIO.readFile "data/levels/level1.json"
  engineState <- getState $ loadLevel levelStr
  (renderer, window) <- createMainWindow "DobadoBots" 
  mainLoop renderer engineState
  closeMainWindow renderer window
  where
    getState (Right state) = return state
    getState (Left err) = fail err

mainLoop :: SDL.Renderer -> GameEngine -> IO ()
mainLoop r st = do
  evts <- SDL.pollEvents
  mainGraphicsLoop r st
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload evts
  unless quit $ mainLoop r st
