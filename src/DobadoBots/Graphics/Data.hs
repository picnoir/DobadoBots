module DobadoBots.Graphics.Data (
  RendererState(..)
) where

import qualified SDL            (Texture, V2)
import Foreign.C.Types          (CInt) 

data RendererState = RendererState {
  robotTexture :: SDL.Texture,
  codeTexture  :: [(SDL.Texture, SDL.V2 CInt)]
}
