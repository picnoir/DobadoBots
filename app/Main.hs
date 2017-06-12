{-# LANGUAGE OverloadedStrings #-}

import Control.Monad                 (unless, when)
import Data.Text                     (Text(..))
import qualified Data.Text.IO as TIO (readFile)
import DobadoBots                    (createMainWindow, closeMainWindow,
                                      mainGraphicsLoop, GameState(..),
                                      loadLevel, createRendererState, RendererState,
                                      gameEngineTick, parseScript, Cond(..),
                                      generateGameState, GamePhase(..),
                                      handleEvents)
import qualified SDL                 (EventPayload(..), eventPayload,
                                      pollEvents, Renderer)

main :: IO ()
main = do
  levelStr <- TIO.readFile "data/levels/level1.json"
  level <- getState $ loadLevel levelStr
  let engineState = generateGameState level
  scriptStr <- TIO.readFile "data/script.script"
  ast <- getAst $ parseScript scriptStr
  (renderer, window) <- createMainWindow "DobadoBots" 
  rendererState <- createRendererState "data/img/robot.bmp" renderer engineState ast
  mainLoop renderer ast engineState rendererState
  closeMainWindow renderer window
  where
    getState (Right state) = return state
    getState (Left err)    = fail err
    getAst   (Right ast)   = return ast
    getAst   (Left err)    = fail $ show err

mainLoop :: SDL.Renderer -> Cond -> GameState -> RendererState -> IO ()
mainLoop r ast st rst = do
  evts <- SDL.pollEvents
  let (nrst, nst) = handleEvents evts rst st
  let nst2 = if phase nst == Running
            then gameEngineTick nst ast
            else nst
  mainGraphicsLoop r nst2 nrst
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload evts
  unless quit $ mainLoop r ast nst2 nrst
