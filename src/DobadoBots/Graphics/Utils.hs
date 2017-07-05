module DobadoBots.Graphics.Utils (
 loadFontBlended,
 getBmpTex,
 isInRectangle,
 changeLevel
) where

import Control.Monad            (unless)
import Data.Int                 (Int32(..))
import Foreign.C.Types          (CInt(..), CDouble(..))
import qualified SDL            (Renderer, Texture(..), V2(..),
                                 Rectangle(..), Point(..), 
                                 freeSurface, createTextureFromSurface,
                                 surfaceDimensions, loadBMP)
import qualified Linear.V2 as L (V2(..))
import qualified Linear.Metric as LM (dot)
import qualified SDL.Raw as Raw (Color(..))
import qualified SDL.TTF as TTF (withInit, wasInit,
                                 openFont, renderUTF8Solid,
                                 renderUTF8Blended, closeFont,
                                 renderUTF8Solid)

import DobadoBots.Graphics.Data (RendererState(..))

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

-- We know when a point is in a rectangle when
-- for a rectangle A, B, C (where ab and bc are perpendicular)
-- we have 0 <= dot(AB,AM) <= dot(AB,AB) && 0 <= dot(bc, bm) <= dot(bc, bc)
isInRectangle :: SDL.Rectangle CInt -> SDL.Point SDL.V2 Int32 -> Bool
isInRectangle (SDL.Rectangle (SDL.P (SDL.V2 xr yr)) (SDL.V2 w h)) (SDL.P (SDL.V2 xm ym)) =
  LM.dot ab am <= LM.dot ab ab &&
  0 <= LM.dot ab am  &&
  0 <= LM.dot bc bm  &&
  LM.dot bc bm  <= LM.dot bc bc 
  where b = L.V2 xr yr
        a = L.V2 (xr + w) yr
        c = L.V2 xr (yr + h)
        m = L.V2 (CInt xm) (CInt ym)
        ab = b - a
        am = m - a
        bc = b - c
        bm = b - m

changeLevel :: Int -> RendererState -> Int
changeLevel i rst = ((currentSelectedLvl rst) + i) `mod` (length (levels rst))
