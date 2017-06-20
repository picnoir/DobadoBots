{-# LANGUAGE OverloadedStrings #-}

import Control.Monad                 (unless, when)
import qualified Data.Text    as T   (Text(..), lines)
import qualified Data.Text.IO as TIO (readFile)
import DobadoBots                    (createMainWindow, closeMainWindow,
                                      mainGraphicsLoop, GameState(..),
                                      RendererState(..), EditorState(..),
                                      loadLevel, createRendererState, RendererState,
                                      gameEngineTick, parseScript, Cond(..),
                                      generateGameState, GamePhase(..),
                                      handleEvents, renderCode)
import qualified SDL                 (EventPayload(..), eventPayload,
                                      pollEvents, Renderer)

main :: IO ()
main = do
  levelStr <- TIO.readFile "data/levels/level1.json"
  level <- getState $ loadLevel levelStr
  scriptStr <- TIO.readFile "data/script.script"
  ast <- getAst $ parseScript scriptStr
  let engineState = generateGameState level ast
  (renderer, window) <- createMainWindow "DobadoBots" 
  rendererState <- createRendererState "data/img/robot.bmp" renderer engineState ast
  mainLoop renderer engineState rendererState
  closeMainWindow renderer window
  where
    getState (Right state) = return state
    getState (Left err)    = fail err
    getAst   (Right ast)   = return ast
    getAst   (Left err)    = fail $ show err

mainLoop :: SDL.Renderer -> GameState -> RendererState -> IO ()
mainLoop r st rst = do
  evts <- SDL.pollEvents
  let (nrst, nst) = handleEvents r evts rst st
  let nst2 = if phase nst == Running
            then gameEngineTick nst (ast st)
            else nst
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload evts
  if editor nrst == editor rst
  then do
    mainGraphicsLoop r nst nrst
    unless quit $ mainLoop r nst2 nrst
  else do
    newCodeTex <- renderCode r . T.lines . text $ editor nrst
    let nnrst = RendererState (robotTexture nrst) (editorCursor nrst) newCodeTex (running nrst)(editing nrst)(buttons nrst) (editor nrst)
    mainGraphicsLoop r nst nnrst
    unless quit $ mainLoop r nst2 nnrst
