module DobadoBots.Graphics.Data (
  RendererState(..),
  Buttons(..),
  Button(..),
  ButtonEvent(..),
  EditorState(..),
  EditorEvent(..),
  toList
) where

import           Data.Text (Text(..))
import qualified SDL       (Texture, V2,
                            Rectangle, Point)
import Foreign.C.Types     (CInt) 

data RendererState = RendererState {
  robotTexture   :: (SDL.Texture, SDL.V2 CInt),
  editorCursor   :: (SDL.Texture, SDL.V2 CInt),
  codeTextures   :: [(SDL.Texture, SDL.V2 CInt)],
  running        :: (SDL.Texture, SDL.V2 CInt),
  editing        :: (SDL.Texture, SDL.V2 CInt),
  buttons        :: Buttons,
  isSyntaxError  :: Bool,
  parseErrorMess :: [(SDL.Texture, SDL.V2 CInt)],
  editor         :: EditorState
}
 
data Buttons = Buttons {
  startButton   :: Button,
  editButton    :: Button
}

toList :: Buttons -> [Button]
toList b = [startButton b, editButton b]

data Button = Button {
  buttonTex     :: (SDL.Texture, SDL.V2 CInt),
  buttonTexHover:: (SDL.Texture, SDL.V2 CInt),
  buttonPos     :: SDL.Point SDL.V2 CInt,
  isHover       :: Bool,
  isActive      :: Bool,
  event         :: ButtonEvent
}

data ButtonEvent = StartEvent
                 | EditEvent deriving (Eq, Show)

data EditorState = EditorState {
  text          :: Text,
  cursorColumn  :: Int,
  cursorLine    :: Int
} deriving (Eq, Show)

data EditorEvent = AppendChar Char
                 | NewLine
                 | BackSpace
                 | Space
                 | Delete 
                 | Left
                 | Right
                 | Up
                 | Down
                 deriving (Eq, Show)
