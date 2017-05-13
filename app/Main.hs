{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (unless)
import Data.Text (Text(..))
import qualified Data.Text.IO as TIO (readFile)
import DobadoBots (createMainWindow, closeMainWindow, mainGraphicsLoop, GameEngine(..),
  loadLevel, loadTextures, Textures, gameEngineTick, parseScript, Cond(..))
import qualified SDL (EventPayload(..), eventPayload, pollEvents, Renderer)

main :: IO ()
main = do
  levelStr <- TIO.readFile "data/levels/level1.json"
  scriptStr <- TIO.readFile "data/script.script"
  ast <- getAst $ parseScript scriptStr
  engineState <- getState $ loadLevel levelStr
  (renderer, window) <- createMainWindow "DobadoBots" 
  textures <- loadTextures "data/img/robot.bmp" renderer 
  mainLoop renderer ast engineState textures
  closeMainWindow renderer window
  where
    getState (Right state) = return state
    getState (Left err)    = fail err
    getAst   (Right ast)   = return ast
    getAst   (Left err)    = fail $ show err

mainLoop :: SDL.Renderer -> Cond -> GameEngine -> Textures -> IO ()
mainLoop r ast st tex = do
  evts <- SDL.pollEvents
  let nst = gameEngineTick st ast
  mainGraphicsLoop r nst tex
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload evts
  unless quit $ mainLoop r ast nst tex
