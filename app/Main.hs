{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (unless, when)
import Data.Text (Text(..))
import qualified Data.Text.IO as TIO (readFile)
import DobadoBots (createMainWindow, closeMainWindow,
                   mainGraphicsLoop, GameState(..),
                   loadLevel, createRendererState, RendererState,
                   gameEngineTick, parseScript, Cond(..),
                   generateGameState, GamePhase(..))
import qualified SDL (EventPayload(..), eventPayload, pollEvents, Renderer)

main :: IO ()
main = do
  levelStr <- TIO.readFile "data/levels/level1.json"
  level <- getState $ loadLevel levelStr
  let engineState = generateGameState level
  scriptStr <- TIO.readFile "data/script.script"
  ast <- getAst $ parseScript scriptStr
  (renderer, window) <- createMainWindow "DobadoBots" 
  textures <- createRendererState "data/img/robot.bmp" renderer engineState 
  mainLoop renderer ast engineState textures
  closeMainWindow renderer window
  where
    getState (Right state) = return state
    getState (Left err)    = fail err
    getAst   (Right ast)   = return ast
    getAst   (Left err)    = fail $ show err

mainLoop :: SDL.Renderer -> Cond -> GameState -> RendererState -> IO ()
mainLoop r ast st tex = do
  evts <- SDL.pollEvents
  let nst = if phase st == Running
            then gameEngineTick st ast
            else st
  mainGraphicsLoop r nst tex
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload evts
  unless quit $ mainLoop r ast nst tex
