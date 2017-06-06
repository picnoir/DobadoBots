module DobadoBots.Graphics.Utils (
 loadFontBlended
) where

import Control.Monad            (unless)
import Foreign.C.Types          (CInt(..), CDouble(..))
import qualified SDL            (Renderer, Texture(..), V2(..),
                                 freeSurface, createTextureFromSurface,
                                 surfaceDimensions)
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
