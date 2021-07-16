{-# LANGUAGE TemplateHaskell, CApiFFI #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.EventStructures where

import GHC.Stack
import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Foreign.C.Enum
import Foreign.C.Struct
import Control.Arrow
import Data.Bits
import Data.Bits.Misc
import Data.Word
import Data.Int
import Data.Sealed.Internal
import System.IO.Unsafe

import Graphics.Gdk.GdkDevice
import Graphics.Gdk.GdkDevice.GdkAxes
import {-# SOURCE #-} Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkModifierType
import Graphics.Gdk.EventStructures.GdkKeySyms

#include <gdk/gdk.h>

enum "GdkEventType" ''#{type GdkEventType} [''Show, ''Storable] [
	("GdkNothing", #{const GDK_NOTHING}), ("GdkDelete", #{const GDK_DELETE}),
	("GdkExpose", #{const GDK_EXPOSE}),
	("GdkMotionNotify", #{const GDK_MOTION_NOTIFY}),
	("GdkButtonPress", #{const GDK_BUTTON_PRESS}),
	("Gdk2ButtonPress", #{const GDK_2BUTTON_PRESS}),
	("GdkDoubleButtonPress", #{const GDK_DOUBLE_BUTTON_PRESS}),
	("Gdk3ButtonPress", #{const GDK_3BUTTON_PRESS}),
	("GdkTripleButtonPress", #{const GDK_TRIPLE_BUTTON_PRESS}),
	("GdkButtonRelease", #{const GDK_BUTTON_RELEASE}),
	("GdkKeyPress", #{const GDK_KEY_PRESS}),
	("GdkKeyRelease", #{const GDK_KEY_RELEASE}),
	("GdkEnterNotify", #{const GDK_ENTER_NOTIFY}),
	("GdkLeaveNotify", #{const GDK_LEAVE_NOTIFY}),
	("GdkFocusChange", #{const GDK_FOCUS_CHANGE}),
	("GdkConfigure", #{const GDK_CONFIGURE}),
	("GdkMap", #{const GDK_MAP}),
	("GdkUnmap", #{const GDK_UNMAP}),
	("GdkPropertyNotify", #{const GDK_PROPERTY_NOTIFY}),
	("GdkSelectionClear", #{const GDK_SELECTION_CLEAR}),
	("GdkSelectionRequest", #{const GDK_SELECTION_REQUEST}),
	("GdkSelectionNotify", #{const GDK_SELECTION_NOTIFY}),
	("GdkProximityIn", #{const GDK_PROXIMITY_IN}),
	("GdkProximityOut", #{const GDK_PROXIMITY_OUT}),
	("GdkDragEnter", #{const GDK_DRAG_ENTER}),
	("GdkDragLeave", #{const GDK_DRAG_LEAVE}),
	("GdkDragMotion", #{const GDK_DRAG_MOTION}),
	("GdkDragStatus", #{const GDK_DRAG_STATUS}),
	("GdkDropStart", #{const GDK_DROP_START}),
	("GdkDropFinished", #{const GDK_DROP_FINISHED}),
	("GdkClientEvent", #{const GDK_CLIENT_EVENT}),
	("GdkVisibilityNotify", #{const GDK_VISIBILITY_NOTIFY}),
	("GdkScroll", #{const GDK_SCROLL}),
	("GdkWindowState_", #{const GDK_WINDOW_STATE}),
	("GdkSetting", #{const GDK_SETTING}),
	("GdkOwnerChange", #{const GDK_OWNER_CHANGE}),
	("GdkGrabBroken", #{const GDK_GRAB_BROKEN})
	]

data GdkEvent = GdkEvent GdkEventType (ForeignPtr GdkEvent) deriving Show

mkGdkEvent :: Ptr GdkEvent -> IO GdkEvent
mkGdkEvent p = do
	t <- GdkEventType <$> #{peek GdkEvent, type} p
	GdkEvent t <$> newForeignPtr p (c_gdk_event_free p)

newtype GdkEventSealed s = GdkEventSealed (ForeignPtr GdkEvent) deriving Show

foreign import ccall "gdk_event_free" c_gdk_event_free :: Ptr GdkEvent -> IO ()

enum "BoolGInt8" ''#{type gint8} [''Show, ''Storable, ''Eq, ''Num] [
	("False8", #{const FALSE}), ("True8", #{const TRUE}) ]

struct "GdkEventAnyRaw" #{size GdkEventAny}
	[	("type", ''GdkEventType, [| #{peek GdkEventAny, type} |],
			[| #{poke GdkEventAny, type} |]),
		("window", ''GdkWindow, [| #{peek GdkEventAny, window} |],
			[| #{poke GdkEventAny, window} |]),
		("sendEvent", ''BoolGInt8,
			[| #{peek GdkEventAny, send_event} |],
			[| #{poke GdkEventAny, send_event} |]) ]
	[''Show]

{-# COMPLETE GdkEventSealedGdkEventAny #-}

pattern GdkEventSealedGdkEventAny :: Sealed s GdkEventAnyRaw -> GdkEventSealed s
pattern GdkEventSealedGdkEventAny ea <-
	GdkEventSealed (Sealed . GdkEventAnyRaw_ . castForeignPtr -> ea)

gdkEventSealedType :: GdkEventSealed s -> GdkEventType
gdkEventSealedType (GdkEventSealedGdkEventAny (Sealed ea)) = gdkEventAnyRawType ea

pattern GdkEventSealedGdkNothing :: Sealed s GdkEventAnyRaw -> GdkEventSealed s
pattern GdkEventSealedGdkNothing ea <-
	GdkEventSealed (gdkEventTypeRaw GdkEventAnyRaw_ -> (GdkNothing, ea))

pattern GdkEventSealedGdkDelete :: Sealed s GdkEventAnyRaw -> GdkEventSealed s
pattern GdkEventSealedGdkDelete ea <-
	GdkEventSealed (gdkEventTypeRaw GdkEventAnyRaw_ -> (GdkDelete, ea))

pattern GdkEventSealedGdkMap :: Sealed s GdkEventAnyRaw -> GdkEventSealed s
pattern GdkEventSealedGdkMap e <-
	GdkEventSealed (gdkEventTypeRaw GdkEventAnyRaw_ -> (GdkMap, e))

pattern GdkEventSealedGdkUnmap :: Sealed s GdkEventAnyRaw -> GdkEventSealed s
pattern GdkEventSealedGdkUnmap e <-
	GdkEventSealed (gdkEventTypeRaw GdkEventAnyRaw_ -> (GdkUnmap, e))

pattern GdkEventGdkMap :: GdkEventAnyRaw -> GdkEvent
pattern GdkEventGdkMap p <- GdkEvent GdkMap (GdkEventAnyRaw_ . castForeignPtr -> p)

pattern GdkEventGdkUnmap :: GdkEventAnyRaw -> GdkEvent
pattern GdkEventGdkUnmap p <- GdkEvent GdkUnmap (GdkEventAnyRaw_ . castForeignPtr -> p)

pattern GdkEventGdkDelete :: GdkEventAnyRaw -> GdkEvent
pattern GdkEventGdkDelete p <- GdkEvent GdkDelete (GdkEventAnyRaw_ . castForeignPtr -> p)

pattern GdkEventGdkNothing :: GdkEventAnyRaw -> GdkEvent
pattern GdkEventGdkNothing p <- GdkEvent GdkNothing (GdkEventAnyRaw_ . castForeignPtr -> p)

gdkEventTypeRaw ::
	(ForeignPtr x -> a) -> ForeignPtr GdkEvent -> (GdkEventType, Sealed s a)
gdkEventTypeRaw c = gdkEventSealedType . GdkEventSealed &&& sealEvent c

sealEvent :: (ForeignPtr x -> a) -> ForeignPtr y -> Sealed s a
sealEvent c = Sealed . c . castForeignPtr

data {-# CTYPE "gdk/gdk.h" "GdkEventKey" #-} GdkEventKey'

enum "BoolCUInt" ''CUInt [''Show, ''Eq, ''Num] [
	("FalseCUInt", #{const FALSE}), ("TrueCUInt", #{const TRUE}) ]

foreign import capi "gdkhs.h peek_gdk_event_key_is_modifier"
	c_peek_gdk_event_key_is_modifier :: Ptr GdkEventKey' -> IO BoolCUInt

foreign import capi "gdkhs.h poke_gdk_event_key_is_modifier"
	c_poke_gdk_event_key_is_modifier :: Ptr GdkEventKey' -> BoolCUInt -> IO ()

newtype MilliSecond = MilliSecond #{type guint32} deriving (Show, Storable)

struct "GdkEventKeyRaw" #{size GdkEventKey}
	[	("type", ''GdkEventType, [| #{peek GdkEventKey, type} |],
			[| #{poke GdkEventKey, type} |]),
		("window", ''GdkWindow, [| #{peek GdkEventKey, window} |],
			[| #{poke GdkEventKey, window} |]),
		("sendEvent", ''BoolGInt8,
			[| #{peek GdkEventKey, send_event} |],
			[| #{poke GdkEventKey, send_event} |]),
		("time", ''MilliSecond, [| #{peek GdkEventKey, time} |],
			[| #{poke GdkEventKey, time} |]),
		("state", ''GdkModifierTypeMultiBits,
			[| #{peek GdkEventKey, state} |],
			[| #{poke GdkEventKey, state} |]),
		("keyval", ''GdkKeySym, [| #{peek GdkEventKey, keyval} |],
			[| #{poke GdkEventKey, keyval} |]),
		("lengthDeprecated", ''CInt, [| #{peek GdkEventKey, length} |],
			[| \p _ -> #{poke GdkEventKey, length} p (0 :: CInt) |]),
		("stringDeprecated", ''CString, [| #{peek GdkEventKey, string} |],
			[| \p _ -> #{poke GdkEventKey, string} p nullPtr |]),
		("hardwareKeycode", ''Word16,
			[| #{peek GdkEventKey, hardware_keycode} |],
			[| #{poke GdkEventKey, hardware_keycode} |]),
		("group", ''Word8, [| #{peek GdkEventKey, group} |],
			[| #{poke GdkEventKey, group} |]),
		("isModifier", ''BoolCUInt, [| c_peek_gdk_event_key_is_modifier . castPtr |],
			[| c_poke_gdk_event_key_is_modifier . castPtr |])
		]
	[''Show]

data GdkEventKey = GdkEventKey {
	gdkEventKeyWindow :: GdkWindow,
	gdkEventKeySendEvent :: Bool,
	gdkEventKeyTime :: MilliSecond,
	gdkEventKeyState :: [GdkModifierTypeSingleBit],
	gdkEventKeyKeyval :: GdkKeySym,
	gdkEventKeyHardwareKeycode :: Word16,
	gdkEventKeyGroup :: Word8,
	gdkEventKeyIsModifier :: Bool } deriving Show

gdkEventKey :: Sealed s GdkEventKeyRaw -> GdkEventKey
gdkEventKey (Sealed r) = GdkEventKey
	(gdkEventKeyRawWindow r)
	(case gdkEventKeyRawSendEvent r of
		#{const FALSE} -> False
		#{const TRUE} -> True
		_ -> error "gdkEventKeyRawSendEvent should be FALSE or TRUE")
	(gdkEventKeyRawTime r)
	(gdkModifierTypeSingleBitList $ gdkEventKeyRawState r)
	(gdkEventKeyRawKeyval r)
	(gdkEventKeyRawHardwareKeycode r)
	(gdkEventKeyRawGroup r)
	(case gdkEventKeyRawIsModifier r of
		#{const FALSE} -> False
		#{const TRUE} -> True
		_ -> error "gdkEventKeyRawIsModifier should be FALSE or TRUE")

pattern GdkEventSealedGdkKeyPress :: Sealed s GdkEventKeyRaw -> GdkEventSealed s
pattern GdkEventSealedGdkKeyPress s <-
	GdkEventSealed (gdkEventTypeRaw GdkEventKeyRaw_ -> (GdkKeyPress, s))

pattern GdkEventSealedGdkKeyRelease ::
	Sealed s GdkEventKeyRaw -> GdkEventSealed s
pattern GdkEventSealedGdkKeyRelease e <-
	GdkEventSealed (gdkEventTypeRaw GdkEventKeyRaw_ -> (GdkKeyRelease, e))

pattern GdkEventGdkKeyPress :: GdkEventKeyRaw -> GdkEvent
pattern GdkEventGdkKeyPress p <- GdkEvent (GdkEventType #const GDK_KEY_PRESS) (GdkEventKeyRaw_ . castForeignPtr -> p)

pattern GdkEventGdkKeyRelease :: GdkEventKeyRaw -> GdkEvent
pattern GdkEventGdkKeyRelease p <- GdkEvent (GdkEventType #const GDK_KEY_RELEASE) (GdkEventKeyRaw_ . castForeignPtr -> p)

type PtrCDouble = Ptr CDouble

struct "GdkEventButtonRaw" #{size GdkEventButton}
	[	("type", ''GdkEventType, [| #{peek GdkEventButton, type} |],
			[| #{poke GdkEventButton, type} |]),
		("window", ''GdkWindow, [| #{peek GdkEventButton, window} |],
			[| #{poke GdkEventButton, window} |]),
		("sendEvent", ''BoolGInt8,
			[| #{peek GdkEventButton, send_event} |],
			[| #{poke GdkEventButton, send_event} |]),
		("time", ''MilliSecond, [| #{peek GdkEventButton, time} |],
			[| #{poke GdkEventButton, time} |]),
		("x", ''CDouble, [| #{peek GdkEventButton, x} |],
			[| #{poke GdkEventButton, x} |]),
		("y", ''CDouble, [| #{peek GdkEventButton, y} |],
			[| #{poke GdkEventButton, y} |]),
		("axes", ''PtrCDouble, [| #{peek GdkEventButton, axes} |],
			[| #{poke GdkEventButton, axes} |]),
		("state", ''GdkModifierTypeMultiBits,
			[| #{peek GdkEventButton, state} |],
			[| #{poke GdkEventButton, state} |]),
		("button", ''CUInt, [| #{peek GdkEventButton, button} |],
			[| #{poke GdkEventButton, button} |]),
		("device", ''GdkDevice, [| #{peek GdkEventButton, device} |],
			[| #{poke GdkEventButton, device} |]),
		("xRoot", ''CDouble, [| #{peek GdkEventButton, x_root} |],
			[| #{poke GdkEventButton, x_root} |]),
		("yRoot", ''CDouble, [| #{peek GdkEventButton, y_root} |],
			[| #{poke GdkEventButton, y_root} |]) ]
	[''Show]

data GdkEventButton = GdkEventButton {
	gdkEventButtonType :: GdkEventType,
	gdkEventButtonWindow :: GdkWindow,
	gdkEventButtonSendEvent :: Bool,
	gdkEventButtonTime :: MilliSecond,
	gdkEventButtonX, gdkEventButtonY :: CDouble,
	gdkEventButtonAxes :: GdkAxes,
	gdkEventButtonState :: [GdkModifierTypeSingleBit],
	gdkEventButtonButton :: CUInt,
	gdkEventButtonDevice :: GdkDevice,
	gdkEventButtonXRoot, gdkEventButtonYRoot :: CDouble
	} deriving Show

gdkEventButton :: Sealed s GdkEventButtonRaw -> GdkEventButton
gdkEventButton (Sealed r) = GdkEventButton
	(gdkEventButtonRawType r)
	(gdkEventButtonRawWindow r)
	(case gdkEventButtonRawSendEvent r of
		#{const FALSE} -> False
		#{const TRUE} -> True
		_ -> error "gdkEventButtonSendEvent should be FALSE or TRUE")
	(gdkEventButtonRawTime r)
	(gdkEventButtonRawX r) (gdkEventButtonRawY r)
	(unsafePerformIO $ gdkAxesCopyFromPtr
		(gdkEventButtonRawDevice r) (gdkEventButtonRawAxes r))
	(gdkModifierTypeSingleBitList $ gdkEventButtonRawState r)
	(gdkEventButtonRawButton r)
	(gdkEventButtonRawDevice r)
	(gdkEventButtonRawXRoot r) (gdkEventButtonRawYRoot r)

pattern GdkEventSealedGdkButtonPress :: Sealed s GdkEventButtonRaw -> GdkEventSealed s
pattern GdkEventSealedGdkButtonPress e <-
	GdkEventSealed (gdkEventTypeRaw GdkEventButtonRaw_ -> (GdkButtonPress, e))

pattern GdkEventSealedGdkDoubleButtonPress :: Sealed s GdkEventButtonRaw -> GdkEventSealed s
pattern GdkEventSealedGdkDoubleButtonPress e <-
	GdkEventSealed (gdkEventTypeRaw GdkEventButtonRaw_ -> (GdkDoubleButtonPress, e))

pattern GdkEventSealedGdkTripleButtonPress :: Sealed s GdkEventButtonRaw -> GdkEventSealed s
pattern GdkEventSealedGdkTripleButtonPress e <-
	GdkEventSealed (gdkEventTypeRaw GdkEventButtonRaw_ -> (GdkTripleButtonPress, e))

pattern GdkEventSealedGdkButtonRelease :: Sealed s GdkEventButtonRaw -> GdkEventSealed s
pattern GdkEventSealedGdkButtonRelease e <-
	GdkEventSealed (gdkEventTypeRaw GdkEventButtonRaw_ -> (GdkButtonRelease, e))

struct "GdkEventMotionRaw" #{size GdkEventMotion}
	[	("type", ''GdkEventType, [| #{peek GdkEventMotion, type} |],
			[| #{poke GdkEventMotion, type} |]),
		("window", ''GdkWindow, [| #{peek GdkEventMotion, window} |],
			[| #{poke GdkEventMotion, window} |]),
		("sendEvent", ''BoolGInt8,
			[| #{peek GdkEventMotion, send_event} |],
			[| #{poke GdkEventMotion, send_event} |]),
		("time", ''MilliSecond, [| #{peek GdkEventMotion, time} |],
			[| #{poke GdkEventMotion, time} |]),
		("x", ''CDouble, [| #{peek GdkEventMotion, x} |],
			[| #{poke GdkEventMotion, x} |]),
		("y", ''CDouble, [| #{peek GdkEventMotion, y} |],
			[| #{poke GdkEventMotion, y} |]),
		("axes", ''PtrCDouble, [| #{peek GdkEventMotion, axes} |],
			[| #{poke GdkEventMotion, axes} |]),
		("state", ''GdkModifierTypeMultiBits,
			[| #{peek GdkEventMotion, state} |],
			[| #{poke GdkEventMotion, state} |]),
		("isHint", ''Int16, [| #{peek GdkEventMotion, is_hint} |],
			[| #{poke GdkEventMotion, is_hint} |]),
		("device", ''GdkDevice, [| #{peek GdkEventMotion, device} |],
			[| #{poke GdkEventMotion, device} |]),
		("xRoot", ''CDouble, [| #{peek GdkEventMotion, x_root} |],
			[| #{poke GdkEventMotion, x_root} |]),
		("yRoot", ''CDouble, [| #{peek GdkEventMotion, y_root} |],
			[| #{poke GdkEventMotion, y_root} |])
		]
	[''Show]

data GdkEventMotion = GdkEventMotion {
	gdkEventMotionWindow :: GdkWindow,
	gdkEventMotionSendEvent :: Bool,
	gdkEventMotionTime :: MilliSecond,
	gdkEventMotionX :: CDouble, gdkEventMotionY :: CDouble,
	gdkEventMotionAxes :: GdkAxes,
	gdkEventMotionState :: [GdkModifierTypeSingleBit],
	gdkEventMotionIsHint :: Bool,
	gdkEventMotionDevice :: GdkDevice,
	gdkEventMotionXRoot :: CDouble, gdkEventMotionYRoot :: CDouble
	} deriving Show

gdkEventMotion :: Sealed s GdkEventMotionRaw -> GdkEventMotion
gdkEventMotion (Sealed r) = GdkEventMotion
	(gdkEventMotionRawWindow r)
	(case gdkEventMotionRawSendEvent r of
		#{const FALSE} -> False
		#{const TRUE} -> True
		_ -> error "gdkEventMotionRawSendEvent should be FALSE or TRUE")
	(gdkEventMotionRawTime r)
	(gdkEventMotionRawX r) (gdkEventMotionRawY r)
	(unsafePerformIO $ gdkAxesCopyFromPtr
		(gdkEventMotionRawDevice r) (gdkEventMotionRawAxes r))
	(gdkModifierTypeSingleBitList $ gdkEventMotionRawState r)
	(case gdkEventMotionRawIsHint r of
		#{const FALSE} -> False
		#{const TRUE} -> True
		_ -> error "gdkEventMotionRawIsHint should be FALSE or TRUE")
	(gdkEventMotionRawDevice r)
	(gdkEventMotionRawXRoot r) (gdkEventMotionRawYRoot r)

tryGdkEventMotionCopy :: GdkEventMotionRaw -> IO GdkEventMotionRaw
tryGdkEventMotionCopy (GdkEventMotionRaw_ fem) = GdkEventMotionRaw_ <$> withForeignPtr fem \pem -> do
	pem' <- c_gdk_event_copy' pem
	newForeignPtr pem' . c_gdk_event_free $ castPtr pem'

foreign import ccall "gdk_event_copy"
	c_gdk_event_copy' :: Ptr a -> IO (Ptr a)

pattern GdkEventSealedGdkMotionNotify ::
	Sealed s GdkEventMotionRaw -> GdkEventSealed s
pattern GdkEventSealedGdkMotionNotify s <-
	GdkEventSealed (
		gdkEventSealedType . GdkEventSealed &&&
		Sealed . GdkEventMotionRaw_ . castForeignPtr ->
		(GdkMotionNotify, s) )

pattern GdkEventGdkMotionNotifyRaw :: GdkEventMotionRaw -> GdkEvent
pattern GdkEventGdkMotionNotifyRaw p <- GdkEvent (GdkEventType #const GDK_MOTION_NOTIFY) (GdkEventMotionRaw_ . castForeignPtr -> p)

enum "GdkVisibilityState" ''#{type GdkVisibilityState} [''Show, ''Storable] [
	("GdkVisibilityUnobscured", #{const GDK_VISIBILITY_UNOBSCURED}),
	("GdkVisibilityPartial", #{const GDK_VISIBILITY_PARTIAL}),
	("GdkVisibilityFullyObscured", #{const GDK_VISIBILITY_FULLY_OBSCURED}) ]

struct "GdkEventVisibilityRaw" #{size GdkEventVisibility}
	[	("type", ''GdkEventType, [| #{peek GdkEventVisibility, type} |],
			[| #{poke GdkEventVisibility, type} |]),
		("window", ''GdkWindow, [| #{peek GdkEventVisibility, window} |],
			[| #{poke GdkEventVisibility, window} |]),
		("sendEvent", ''BoolGInt8,
			[| #{peek GdkEventVisibility, send_event} |],
			[| #{poke GdkEventVisibility, send_event} |]),
		("state", ''GdkVisibilityState,
			[| #{peek GdkEventVisibility, state} |],
			[| #{poke GdkEventVisibility, state} |]) ]
	[''Show]

tryGdkEventVisibilitySealedWindow :: Sealed s GdkEventVisibilityRaw -> GdkWindow
tryGdkEventVisibilitySealedWindow (Sealed e) = gdkEventVisibilityRawWindow e

tryGdkEventSealedMapWindow :: Sealed s GdkEventAnyRaw -> GdkWindow
tryGdkEventSealedMapWindow (Sealed e) = gdkEventAnyRawWindow e

gdkEventVisibilityState :: GdkEventVisibilityRaw -> IO GdkVisibilityState
gdkEventVisibilityState (GdkEventVisibilityRaw_ p) = GdkVisibilityState <$> withForeignPtr p #peek GdkEventVisibility, state

pattern GdkEventSealedGdkVisibilityNotify ::
	Sealed s GdkEventVisibilityRaw -> GdkEventSealed s
pattern GdkEventSealedGdkVisibilityNotify e <-
	GdkEventSealed
		(gdkEventTypeRaw GdkEventVisibilityRaw_ -> (GdkVisibilityNotify, e))

pattern GdkEventGdkVisibilityNotify :: GdkEventVisibilityRaw -> GdkEvent
pattern GdkEventGdkVisibilityNotify p <-
	GdkEvent (GdkEventType #const GDK_VISIBILITY_NOTIFY) (GdkEventVisibilityRaw_ . castForeignPtr -> p)

gdkEventVisibilityWindow :: GdkEventVisibilityRaw -> IO GdkWindow
gdkEventVisibilityWindow (GdkEventVisibilityRaw_ p) =
--	GdkWindow <$> (c_g_object_ref =<< withForeignPtr p #peek GdkEventVisibility, window)
	GdkWindow <$> withForeignPtr p #peek GdkEventVisibility, window

gdkEventFocusWindow :: GdkEventFocus -> IO GdkWindow
gdkEventFocusWindow (GdkEventFocus p) =
	GdkWindow <$> withForeignPtr p #peek GdkEventFocus, window

newtype GdkEventFocus = GdkEventFocus (ForeignPtr GdkEventFocus) deriving Show

pattern GdkEventSealedGdkFocusChange :: Sealed s GdkEventFocus -> GdkEventSealed s
pattern GdkEventSealedGdkFocusChange e <-
	GdkEventSealed (gdkEventTypeRaw GdkEventFocus -> (GdkFocusChange, e))

pattern GdkEventGdkFocusChange :: GdkEventFocus -> GdkEvent
pattern GdkEventGdkFocusChange p <-
	GdkEvent (GdkEventType #const GDK_FOCUS_CHANGE) (GdkEventFocus . castForeignPtr -> p)

gdkEventFocusIn :: GdkEventFocus -> IO Bool
gdkEventFocusIn (GdkEventFocus p) = gint16ToBool <$> withForeignPtr p #peek GdkEventFocus, in

gint16ToBool :: HasCallStack => #{type gint16} -> Bool
gint16ToBool #{const TRUE} = True
gint16ToBool #{const FALSE} = False
gint16ToBool _ = error "something wrong"

struct "GdkEventConfigureRaw" #{size GdkEventConfigure}
	[	("type", ''GdkEventType, [| #{peek GdkEventConfigure, type} |],
			[| #{poke GdkEventConfigure, type} |]),
		("window", ''GdkWindow, [| #{peek GdkEventConfigure, window} |],
			[| #{poke GdkEventConfigure, window} |]),
		("sendEvent", ''BoolGInt8,
			[| #{peek GdkEventConfigure, send_event} |],
			[| #{poke GdkEventConfigure, send_event} |]),
		("x", ''CInt, [| #{peek GdkEventConfigure, x} |],
			[| #{poke GdkEventConfigure, x} |]),
		("y", ''CInt, [| #{peek GdkEventConfigure, x} |],
			[| #{poke GdkEventConfigure, y} |]),
		("width", ''CInt, [| #{peek GdkEventConfigure, width} |],
			[| #{poke GdkEventConfigure, width} |]),
		("height", ''CInt, [| #{peek GdkEventConfigure, height} |],
			[| #{poke GdkEventConfigure, height} |]) ]
	[''Show]

tryGdkEventSealedConfigureWindow :: Sealed s GdkEventConfigureRaw -> GdkWindow
tryGdkEventSealedConfigureWindow (Sealed e) = gdkEventConfigureRawWindow e
		
gdkEventConfigureWidth, gdkEventConfigureHeight :: GdkEventConfigureRaw -> IO #type gint
gdkEventConfigureWidth (GdkEventConfigureRaw_ p) = withForeignPtr p #peek GdkEventConfigure, width
gdkEventConfigureHeight (GdkEventConfigureRaw_ p) = withForeignPtr p #peek GdkEventConfigure, height

gdkEventConfigureX, gdkEventConfigureY :: GdkEventConfigureRaw -> IO #type gint
gdkEventConfigureX (GdkEventConfigureRaw_ p) = withForeignPtr p #peek GdkEventConfigure, x
gdkEventConfigureY (GdkEventConfigureRaw_ p) = withForeignPtr p #peek GdkEventConfigure, y

pattern GdkEventSealedGdkConfigure ::
	Sealed s GdkEventConfigureRaw -> GdkEventSealed s
pattern GdkEventSealedGdkConfigure e <-
	GdkEventSealed
		(gdkEventTypeRaw GdkEventConfigureRaw_ -> (GdkConfigure, e))

pattern GdkEventGdkConfigure :: GdkEventConfigureRaw -> GdkEvent
pattern GdkEventGdkConfigure p <- GdkEvent (GdkEventType #{const GDK_CONFIGURE}) (GdkEventConfigureRaw_ . castForeignPtr -> p)

gdkEventConfigureWindow :: GdkEventConfigureRaw -> IO GdkWindow
gdkEventConfigureWindow (GdkEventConfigureRaw_ p) = GdkWindow <$> withForeignPtr p #peek GdkEventConfigure, window

newtype GdkWindowStates = GdkWindowStates #{type GdkWindowState} deriving Show

enum "GdkWindowState" ''#{type GdkWindowState} [''Show, ''Storable] [
	("GdkWindowStateWithdrawn", #{const GDK_WINDOW_STATE_WITHDRAWN}),
	("GdkWindowStateIconified", #{const GDK_WINDOW_STATE_ICONIFIED}),
	("GdkWindowStateMaximized", #{const GDK_WINDOW_STATE_MAXIMIZED}),
	("GdkWindowStateSticky", #{const GDK_WINDOW_STATE_STICKY}),
	("GdkWindowStateFullscreen", #{const GDK_WINDOW_STATE_FULLSCREEN}),
	("GdkWindowStateAbove", #{const GDK_WINDOW_STATE_ABOVE}),
	("GdkWindowStateBelow", #{const GDK_WINDOW_STATE_BELOW}),
	("GdkWindowStateFocused", #{const GDK_WINDOW_STATE_FOCUSED}),
	("GdkWindowStateTopTiled", #{const GDK_WINDOW_STATE_TOP_TILED}),
	("GdkWidnwoStateTopResizable", #{const GDK_WINDOW_STATE_TOP_RESIZABLE}),
	("GdkWindowStateRightTiled", #{const GDK_WINDOW_STATE_RIGHT_TILED}),
	("GdkWindowStateRightResizable",
		#{const GDK_WINDOW_STATE_RIGHT_RESIZABLE}),
	("GdkWindowStateBottomTiled", #{const GDK_WINDOW_STATE_BOTTOM_TILED}),
	("GdkWindowStaetBottomResizable",
		#{const GDK_WINDOW_STATE_BOTTOM_RESIZABLE}),
	("GdkWindowStateLeftTiled", #{const GDK_WINDOW_STATE_LEFT_TILED}),
	("GdkWindowStateLeftResizable",
		#{const GDK_WINDOW_STATE_LEFT_RESIZABLE}) ]

gdkWindowStateCheck :: GdkWindowState -> GdkWindowStates -> Bool
gdkWindowStateCheck (GdkWindowState s) (GdkWindowStates ss) = s .&. ss /= zeroBits

gdkWindowStateList :: GdkWindowStates -> [GdkWindowState]
gdkWindowStateList (GdkWindowStates ss) = GdkWindowState <$> separateBits 32 ss

struct "GdkEventWindowStateRaw" #{size GdkEventWindowState}
	[	("type", ''GdkEventType,
			[| #{peek GdkEventWindowState, type} |],
			[| #{poke GdkEventWindowState, type} |]),
		("window", ''GdkWindow,
			[| #{peek GdkEventWindowState, window} |],
			[| #{poke GdkEventWindowState, window} |]),
		("changedMask", ''GdkWindowState,
			[| #{peek GdkEventWindowState, changed_mask} |],
			[| #{poke GdkEventWindowState, changed_mask} |]),
		("newWindowState", ''GdkWindowState,
			[| #{peek GdkEventWindowState, new_window_state} |],
			[| #{poke GdkEventWindowState, new_window_state} |]) ]
	[''Show]

gdkEventWindowStateNewWindowState :: GdkEventWindowStateRaw -> IO GdkWindowState
gdkEventWindowStateNewWindowState (GdkEventWindowStateRaw_ p) =
	GdkWindowState <$> withForeignPtr p #peek GdkEventWindowState, new_window_state

pattern GdkEventSealedGdkWindowState ::
	Sealed s GdkEventWindowStateRaw -> GdkEventSealed s
pattern GdkEventSealedGdkWindowState e <-
	GdkEventSealed (gdkEventTypeRaw GdkEventWindowStateRaw_ -> (GdkWindowState_, e))

pattern GdkEventGdkWindowState :: GdkEventWindowStateRaw -> GdkEvent
pattern GdkEventGdkWindowState p <- GdkEvent (GdkEventType #{const GDK_WINDOW_STATE}) (GdkEventWindowStateRaw_ . castForeignPtr -> p)

enum "GdkScrollDirection" ''#{type GdkScrollDirection} [''Show] [
	("GdkScrollUp", #{const GDK_SCROLL_UP}),
	("GdkScrollDown", #{const GDK_SCROLL_DOWN}),
	("GdkScrollLeft", #{const GDK_SCROLL_LEFT}),
	("GdkScrollRight", #{const GDK_SCROLL_RIGHT}),
	("GdkScrollSmooth", #{const GDK_SCROLL_SMOOTH}) ]
