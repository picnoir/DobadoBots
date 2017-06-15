module DobadoBots.EditorSpec (spec) where
import Test.Hspec (describe, it, Spec(..), shouldBe)
import SDL
import SDL.Internal.Types
import Foreign.Ptr
import Prelude hiding (Left, Right)

import DobadoBots (handleEditorEvents, EditorEvent(..))

spec :: Spec
spec = describe "handleEditorEvents" $ do
          it "should return append char a" $
             handleEditorEvents [charALowercaseEvent] `shouldBe` Just (AppendChar 'a')
          it "should return append char A" $
             handleEditorEvents [charAUppercaseEvent1] `shouldBe` Just (AppendChar 'A')
          it "should return append char A" $
             handleEditorEvents [charAUppercaseEvent2] `shouldBe` Just (AppendChar 'A')
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
