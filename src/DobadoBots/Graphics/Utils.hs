module DobadoBots.Graphics.Utils (
 loadFontBlended,
 getBmpTex
) where

import Control.Monad            (unless)
import Foreign.C.Types          (CInt(..), CDouble(..))
import qualified SDL            (Renderer, Texture(..), V2(..),
                                 freeSurface, createTextureFromSurface,
                                 surfaceDimensions, loadBMP)
import qualified SDL.Raw as Raw (Color(..))
import qualified SDL.TTF as TTF (withInit, wasInit,
                                 openFont, renderUTF8Solid,
                                 renderUTF8Blended, closeFont,
                                 renderUTF8Solid)

loadFontBlended :: SDL.Renderer -> String -> Int -> Raw.Color -> String -> IO (SDL.Texture, SDL.V2 CInt)
loadFontBlended r fontFile size color text = TTF.withInit $ do
    inited <- TTF.wasInit
    unless inited $ error "[Error] Cannot initialise font system." 
    font <- TTF.openFont fontFile size
    textSurface <- TTF.renderUTF8Blended font text color
    textTexture <- SDL.createTextureFromSurface r textSurface
    size <- SDL.surfaceDimensions textSurface
    SDL.freeSurface textSurface
    TTF.closeFont font
    return (textTexture, size)

getBmpTex :: String -> SDL.Renderer -> IO (SDL.Texture, SDL.V2 CInt)
getBmpTex path r = do
          surf <- SDL.loadBMP path 
          tex  <- SDL.createTextureFromSurface r surf
          dim  <- SDL.surfaceDimensions surf
          SDL.freeSurface surf 
          return (tex, dim)

