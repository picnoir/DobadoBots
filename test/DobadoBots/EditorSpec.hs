{-# LANGUAGE OverloadedStrings #-}
module DobadoBots.EditorSpec (spec) where
import Test.Hspec (describe, it, Spec(..), shouldBe)
import SDL
import SDL.Internal.Types
import Foreign.Ptr
import Prelude hiding (Left, Right)

import DobadoBots (handleEditorEvents, appendEventEditor,
                   EditorEvent(..), EditorState(..))

spec :: Spec
spec = do 
        describe "handleEditorEvents" $ do
          it "should return space" $
             handleEditorEvents [spaceEvent] `shouldBe` Just Space
          it "should return newLine" $
             handleEditorEvents [newLineEvent] `shouldBe` Just NewLine
          it "should return backspace" $
             handleEditorEvents [backSpaceEvent] `shouldBe` Just BackSpace
          it "should return delete" $
             handleEditorEvents [delEvent] `shouldBe` Just Delete 
          it "should return up" $
             handleEditorEvents [upEvent] `shouldBe` Just Up
          it "should return down" $
             handleEditorEvents [downEvent] `shouldBe` Just Down
          it "should return left" $
             handleEditorEvents [leftEvent] `shouldBe` Just Left
          it "should return right" $
             handleEditorEvents [rightEvent] `shouldBe` Just Right 
          it "should ignore key releases" $
             handleEditorEvents [releaseEvent ] `shouldBe` Nothing 
          it "should ignore key releases" $
             handleEditorEvents [releaseEvent ] `shouldBe` Nothing 
          it "should ignore unrelated events" $
             handleEditorEvents [quitEvent] `shouldBe` Nothing 
        describe "appendEventEditor" $ do
          it "should append a simple char" $
             appendEventEditor (AppendChar 'D') (EditorState "hello\nWorld" 0 1) `shouldBe` EditorState "hello\nDWorld" 1 1
          it "should append a simple char when the editor is empty" $
             appendEventEditor (AppendChar 'D') (EditorState "" 0 0) `shouldBe` EditorState "D" 1 0
          it "should append a simple char on last line" $
             appendEventEditor (AppendChar 'D') (EditorState "Hello\n" 1 1) `shouldBe` EditorState "Hello\nD" 2 1
          it "should append a new line" $
             appendEventEditor NewLine (EditorState "hello\nWorld" 5 0) `shouldBe` EditorState "hello\n\nWorld" 0 1
          it "should backspace a caracter" $
             appendEventEditor BackSpace (EditorState "hello\nWorld" 5 0) `shouldBe` EditorState "hell\nWorld" 4 0
          it "should backspace to the upper line" $
             appendEventEditor BackSpace (EditorState "hello\nWorld" 0 1) `shouldBe` EditorState "helloWorld" 5 0
          it "shouldn't backspace an empty text" $
             appendEventEditor BackSpace (EditorState "" 0 0) `shouldBe` EditorState "" 0 0
          it "shouldn't backspace when cursor is at the line beginning" $
             appendEventEditor BackSpace (EditorState "Hello\nWorld" 0 0) `shouldBe` EditorState "Hello\nWorld" 0 0
          it "should add a space" $
             appendEventEditor Space (EditorState "" 0 0) `shouldBe` EditorState " " 1 0
          it "should delete a character" $
             appendEventEditor Delete (EditorState "hello\nWorld" 4 0) `shouldBe` EditorState "hell\nWorld" 4 0
          it "should delete a character" $
             appendEventEditor Delete (EditorState "hello\nWorld" 0 0) `shouldBe` EditorState "ello\nWorld" 0 0
          it "shouldn't delete an empty text" $
             appendEventEditor Delete (EditorState "" 0 0) `shouldBe` EditorState "" 0 0
          it "shouldn't delete an almost empty text" $
             appendEventEditor Delete (EditorState "r" 0 0) `shouldBe` EditorState "" 0 0
          it "should go left" $
             appendEventEditor Left (EditorState "hello\nWorld" 4 0) `shouldBe` EditorState "hello\nWorld" 3 0
          it "shouldn't go left if at beginning of line" $
             appendEventEditor Left (EditorState "hello\nWorld" 0 0) `shouldBe` EditorState "hello\nWorld" 0 0
          it "should go right" $
             appendEventEditor Right (EditorState "hello\nWorld" 4 0) `shouldBe` EditorState "hello\nWorld" 5 0
          it "shouldn't go right if at end of line" $
             appendEventEditor Right (EditorState "hello\nWorld" 5 0) `shouldBe` EditorState "hello\nWorld" 5 0
          it "should go up" $
             appendEventEditor Up (EditorState "hello\nWorld" 2 1) `shouldBe` EditorState "hello\nWorld" 2 0
          it "shouldn't go up if already at top" $
             appendEventEditor Up (EditorState "hello\nWorld" 0 0) `shouldBe` EditorState "hello\nWorld" 0 0
          it "should go to the left if the upper line has less characters" $
             appendEventEditor Up (EditorState "a\nab" 2 1) `shouldBe` EditorState "a\nab" 1 0
          it "should go down" $
             appendEventEditor Down (EditorState "hello\nWorld" 2 0) `shouldBe` EditorState "hello\nWorld" 2 1
          it "shouldn't go down if already at bottom" $
             appendEventEditor Down (EditorState "hello\nWorld" 2 1) `shouldBe` EditorState "hello\nWorld" 2 1
          it "should go to the left if down line has less characters" $
             appendEventEditor Down (EditorState "ab\na" 2 0) `shouldBe` EditorState "ab\na" 1 1

        

charALowercaseEvent :: Event
charALowercaseEvent = Event {eventTimestamp = 4084, eventPayload = KeyboardEvent (KeyboardEventData {keyboardEventWindow = Window nullPtr, keyboardEventKeyMotion = Pressed, keyboardEventRepeat = False, keyboardEventKeysym = Keysym {keysymScancode = Scancode {unwrapScancode = 4}, keysymKeycode = Keycode {unwrapKeycode = 97}, keysymModifier = KeyModifier {keyModifierLeftShift = False, keyModifierRightShift = False, keyModifierLeftCtrl = False, keyModifierRightCtrl = False, keyModifierLeftAlt = False, keyModifierRightAlt = False, keyModifierLeftGUI = False, keyModifierRightGUI = False, keyModifierNumLock = False, keyModifierCapsLock = False, keyModifierAltGr = False}}})}

charAUppercaseEvent1 :: Event
charAUppercaseEvent1 = Event {eventTimestamp = 4084, eventPayload = KeyboardEvent (KeyboardEventData {keyboardEventWindow = Window nullPtr, keyboardEventKeyMotion = Pressed, keyboardEventRepeat = False, keyboardEventKeysym = Keysym {keysymScancode = Scancode {unwrapScancode = 4}, keysymKeycode = Keycode {unwrapKeycode = 97}, keysymModifier = KeyModifier {keyModifierLeftShift = True, keyModifierRightShift = False, keyModifierLeftCtrl = False, keyModifierRightCtrl = False, keyModifierLeftAlt = False, keyModifierRightAlt = False, keyModifierLeftGUI = False, keyModifierRightGUI = False, keyModifierNumLock = False, keyModifierCapsLock = False, keyModifierAltGr = False}}})}

charAUppercaseEvent2 :: Event
charAUppercaseEvent2 = Event {eventTimestamp = 4084, eventPayload = KeyboardEvent (KeyboardEventData {keyboardEventWindow = Window nullPtr, keyboardEventKeyMotion = Pressed, keyboardEventRepeat = False, keyboardEventKeysym = Keysym {keysymScancode = Scancode {unwrapScancode = 4}, keysymKeycode = Keycode {unwrapKeycode = 97}, keysymModifier = KeyModifier {keyModifierLeftShift = False, keyModifierRightShift = True, keyModifierLeftCtrl = False, keyModifierRightCtrl = False, keyModifierLeftAlt = False, keyModifierRightAlt = False, keyModifierLeftGUI = False, keyModifierRightGUI = False, keyModifierNumLock = False, keyModifierCapsLock = False, keyModifierAltGr = False}}})}

spaceEvent :: Event
spaceEvent = Event {eventTimestamp = 4084, eventPayload = KeyboardEvent (KeyboardEventData {keyboardEventWindow = Window nullPtr, keyboardEventKeyMotion = Pressed, keyboardEventRepeat = False, keyboardEventKeysym = Keysym {keysymScancode = Scancode {unwrapScancode = 44}, keysymKeycode = Keycode {unwrapKeycode = 32}, keysymModifier = KeyModifier {keyModifierLeftShift = True, keyModifierRightShift = False, keyModifierLeftCtrl = False, keyModifierRightCtrl = False, keyModifierLeftAlt = False, keyModifierRightAlt = False, keyModifierLeftGUI = False, keyModifierRightGUI = False, keyModifierNumLock = False, keyModifierCapsLock = False, keyModifierAltGr = False}}})}

newLineEvent :: Event
newLineEvent = Event {eventTimestamp = 4084, eventPayload = KeyboardEvent (KeyboardEventData {keyboardEventWindow = Window nullPtr, keyboardEventKeyMotion = Pressed, keyboardEventRepeat = False, keyboardEventKeysym = Keysym {keysymScancode = Scancode {unwrapScancode = 40}, keysymKeycode = Keycode {unwrapKeycode = 13}, keysymModifier = KeyModifier {keyModifierLeftShift = True, keyModifierRightShift = False, keyModifierLeftCtrl = False, keyModifierRightCtrl = False, keyModifierLeftAlt = False, keyModifierRightAlt = False, keyModifierLeftGUI = False, keyModifierRightGUI = False, keyModifierNumLock = False, keyModifierCapsLock = False, keyModifierAltGr = False}}})}

backSpaceEvent :: Event
backSpaceEvent = Event {eventTimestamp = 4084, eventPayload = KeyboardEvent (KeyboardEventData {keyboardEventWindow = Window nullPtr, keyboardEventKeyMotion = Pressed, keyboardEventRepeat = False, keyboardEventKeysym = Keysym {keysymScancode = Scancode {unwrapScancode = 42}, keysymKeycode = Keycode {unwrapKeycode = 8}, keysymModifier = KeyModifier {keyModifierLeftShift = True, keyModifierRightShift = False, keyModifierLeftCtrl = False, keyModifierRightCtrl = False, keyModifierLeftAlt = False, keyModifierRightAlt = False, keyModifierLeftGUI = False, keyModifierRightGUI = False, keyModifierNumLock = False, keyModifierCapsLock = False, keyModifierAltGr = False}}})}

delEvent :: Event
delEvent = Event {eventTimestamp = 4084, eventPayload = KeyboardEvent (KeyboardEventData {keyboardEventWindow = Window nullPtr, keyboardEventKeyMotion = Pressed, keyboardEventRepeat = False, keyboardEventKeysym = Keysym {keysymScancode = Scancode {unwrapScancode = 76}, keysymKeycode = Keycode {unwrapKeycode = 127}, keysymModifier = KeyModifier {keyModifierLeftShift = True, keyModifierRightShift = False, keyModifierLeftCtrl = False, keyModifierRightCtrl = False, keyModifierLeftAlt = False, keyModifierRightAlt = False, keyModifierLeftGUI = False, keyModifierRightGUI = False, keyModifierNumLock = False, keyModifierCapsLock = False, keyModifierAltGr = False}}})}

leftEvent :: Event
leftEvent = Event {eventTimestamp = 4084, eventPayload = KeyboardEvent (KeyboardEventData {keyboardEventWindow = Window nullPtr, keyboardEventKeyMotion = Pressed, keyboardEventRepeat = False, keyboardEventKeysym = Keysym {keysymScancode = Scancode {unwrapScancode = 80}, keysymKeycode = Keycode {unwrapKeycode = 1073741904}, keysymModifier = KeyModifier {keyModifierLeftShift = True, keyModifierRightShift = False, keyModifierLeftCtrl = False, keyModifierRightCtrl = False, keyModifierLeftAlt = False, keyModifierRightAlt = False, keyModifierLeftGUI = False, keyModifierRightGUI = False, keyModifierNumLock = False, keyModifierCapsLock = False, keyModifierAltGr = False}}})}

rightEvent :: Event
rightEvent = Event {eventTimestamp = 4084, eventPayload = KeyboardEvent (KeyboardEventData {keyboardEventWindow = Window nullPtr, keyboardEventKeyMotion = Pressed, keyboardEventRepeat = False, keyboardEventKeysym = Keysym {keysymScancode = Scancode {unwrapScancode = 79}, keysymKeycode = Keycode {unwrapKeycode = 1073741903}, keysymModifier = KeyModifier {keyModifierLeftShift = True, keyModifierRightShift = False, keyModifierLeftCtrl = False, keyModifierRightCtrl = False, keyModifierLeftAlt = False, keyModifierRightAlt = False, keyModifierLeftGUI = False, keyModifierRightGUI = False, keyModifierNumLock = False, keyModifierCapsLock = False, keyModifierAltGr = False}}})}

upEvent:: Event
upEvent = Event {eventTimestamp = 4084, eventPayload = KeyboardEvent (KeyboardEventData {keyboardEventWindow = Window nullPtr, keyboardEventKeyMotion = Pressed, keyboardEventRepeat = False, keyboardEventKeysym = Keysym {keysymScancode = Scancode {unwrapScancode = 82}, keysymKeycode = Keycode {unwrapKeycode = 1073741906}, keysymModifier = KeyModifier {keyModifierLeftShift = True, keyModifierRightShift = False, keyModifierLeftCtrl = False, keyModifierRightCtrl = False, keyModifierLeftAlt = False, keyModifierRightAlt = False, keyModifierLeftGUI = False, keyModifierRightGUI = False, keyModifierNumLock = False, keyModifierCapsLock = False, keyModifierAltGr = False}}})}

downEvent :: Event
downEvent = Event {eventTimestamp = 4084, eventPayload = KeyboardEvent (KeyboardEventData {keyboardEventWindow = Window nullPtr, keyboardEventKeyMotion = Pressed, keyboardEventRepeat = False, keyboardEventKeysym = Keysym {keysymScancode = Scancode {unwrapScancode = 81}, keysymKeycode = Keycode {unwrapKeycode = 1073741905}, keysymModifier = KeyModifier {keyModifierLeftShift = True, keyModifierRightShift = False, keyModifierLeftCtrl = False, keyModifierRightCtrl = False, keyModifierLeftAlt = False, keyModifierRightAlt = False, keyModifierLeftGUI = False, keyModifierRightGUI = False, keyModifierNumLock = False, keyModifierCapsLock = False, keyModifierAltGr = False}}})}

releaseEvent :: Event
releaseEvent = Event {eventTimestamp = 4084, eventPayload = KeyboardEvent (KeyboardEventData {keyboardEventWindow = Window nullPtr, keyboardEventKeyMotion = Released, keyboardEventRepeat = False, keyboardEventKeysym = Keysym {keysymScancode = Scancode {unwrapScancode = 81}, keysymKeycode = Keycode {unwrapKeycode = 1073741905}, keysymModifier = KeyModifier {keyModifierLeftShift = True, keyModifierRightShift = False, keyModifierLeftCtrl = False, keyModifierRightCtrl = False, keyModifierLeftAlt = False, keyModifierRightAlt = False, keyModifierLeftGUI = False, keyModifierRightGUI = False, keyModifierNumLock = False, keyModifierCapsLock = False, keyModifierAltGr = False}}})}

quitEvent :: Event
quitEvent = Event {eventTimestamp = 2203, eventPayload = QuitEvent}

  
