{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (unless)
import Data.Text (Text(..))
import qualified Data.Text.IO as TIO (readFile)
import DobadoBots (createMainWindow, closeMainWindow, mainGraphicsLoop, GameEngine(..),
  loadLevel, loadTextures, Textures)
import qualified SDL (EventPayload(..), eventPayload, pollEvents, Renderer)

main :: IO ()
main = do
  levelStr <- TIO.readFile "data/levels/level1.json"
  engineState <- getState $ loadLevel levelStr
  (renderer, window) <- createMainWindow "DobadoBots" 
  textures <- loadTextures "data/img/robot.bmp" renderer 
  mainLoop renderer engineState textures
  closeMainWindow renderer window
  where
    getState (Right state) = return state
    getState (Left err) = fail err

mainLoop :: SDL.Renderer -> GameEngine -> Textures -> IO ()
mainLoop r st tex = do
  evts <- SDL.pollEvents
  mainGraphicsLoop r st tex
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload evts
  unless quit $ mainLoop r st tex
