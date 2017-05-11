module DobadoBots.GameEngine.LevelLoader (
  loadLevel
) where

import Data.Aeson (decode)
import Data.Text (Text(..))
import qualified Data.Text.Lazy as TL (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import DobadoBots.GameEngine.Data (GameEngine(..))

loadLevel :: Text -> Maybe GameEngine
loadLevel = decode . encodeUtf8 . TL.fromStrict
