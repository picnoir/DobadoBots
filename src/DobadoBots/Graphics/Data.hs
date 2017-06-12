module DobadoBots.Graphics.Data (
  RendererState(..),
  Buttons(..)
) where

import qualified SDL   (Texture, V2,
                        Rectangle)
import Foreign.C.Types (CInt) 

data RendererState = RendererState {
  robotTexture  :: (SDL.Texture, SDL.V2 CInt),
  codeTextures  :: [(SDL.Texture, SDL.V2 CInt)],
  buttons       :: Buttons
}

newtype Buttons = Buttons {
  startButton   :: (SDL.Texture, SDL.V2 CInt)
}
