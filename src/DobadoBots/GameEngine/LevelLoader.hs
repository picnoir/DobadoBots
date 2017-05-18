module DobadoBots.GameEngine.LevelLoader (
  loadLevel
) where

import Data.Aeson (eitherDecode)
import Data.Text (Text(..))
import qualified Data.Text.Lazy as TL (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import DobadoBots.GameEngine.Data (Level(..))

loadLevel :: Text -> Either String Level
loadLevel = eitherDecode . encodeUtf8 . TL.fromStrict
