module DobadoBots.Graphics.Data (
  RendererState(..),
  Buttons(..),
  Button(..),
  ButtonEvent(..)
) where

import qualified SDL   (Texture, V2,
                        Rectangle, Point)
import Foreign.C.Types (CInt) 

data RendererState = RendererState {
  robotTexture  :: (SDL.Texture, SDL.V2 CInt),
  codeTextures  :: [(SDL.Texture, SDL.V2 CInt)],
  buttons       :: Buttons
}

data Buttons = Buttons {
  startButton   :: Button
}

data Button = Button {
  buttonTex     :: (SDL.Texture, SDL.V2 CInt),
  buttonTexHover:: (SDL.Texture, SDL.V2 CInt),
  buttonPos     :: SDL.Point SDL.V2 CInt,
  isHover       :: Bool,
  isActive      :: Bool,
  event         :: ButtonEvent
}

data ButtonEvent = StartEvent
                 | EditEvent
