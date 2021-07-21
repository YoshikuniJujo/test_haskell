{-# LANGUAGE TemplateHaskell, CApiFFI #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.EventStructures where

import GHC.Stack
import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
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

---------------------------------------------------------------------------
-- GDK EVENT TYPE                                                        --
---------------------------------------------------------------------------

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
	("GdkGrabBroken", #{const GDK_GRAB_BROKEN}) ]

---------------------------------------------------------------------------
-- GDK EVENT                                                             --
---------------------------------------------------------------------------

newtype GdkEvent s = GdkEvent (ForeignPtr GdkEventTag) deriving Show

data GdkEventTag

---------------------------------------------------------------------------
-- BOOLS                                                                 --
---------------------------------------------------------------------------

enum "BoolInt8" ''Int8 [''Show, ''Storable, ''Eq, ''Num] [
	("FalseInt8", #{const FALSE}), ("TrueInt8", #{const TRUE}) ]

enum"BoolInt16" ''Int16 [''Show, ''Storable, ''Eq, ''Num] [
	("FalseInt16", #{const FALSE}), ("TrueInt16", #{const TRUE}) ]

enum "BoolCUInt" ''CUInt [''Show, ''Eq, ''Num] [
	("FalseCUInt", #{const FALSE}), ("TrueCUInt", #{const TRUE}) ]

---------------------------------------------------------------------------
-- GDK EVENT ANY                                                         --
---------------------------------------------------------------------------

struct "GdkEventAnyRaw" #{size GdkEventAny}
	[	("type", ''GdkEventType, [| #{peek GdkEventAny, type} |],
			[| #{poke GdkEventAny, type} |]),
		("window", ''GdkWindow, [| #{peek GdkEventAny, window} |],
			[| #{poke GdkEventAny, window} |]),
		("sendEvent", ''BoolInt8,
			[| #{peek GdkEventAny, send_event} |],
			[| #{poke GdkEventAny, send_event} |]) ]
	[''Show]

data GdkEventAny = GdkEventAny {
	gdkEventAnyType :: GdkEventType, gdkEventAnyWindow :: GdkWindow,
	gdkEventAnySendEvent :: Bool }
	deriving Show

gdkEventAny :: Sealed s GdkEventAnyRaw -> GdkEventAny
gdkEventAny (Sealed r) = GdkEventAny
	(gdkEventAnyRawType r) (gdkEventAnyRawWindow r)
	(case gdkEventAnyRawSendEvent r of
		FalseInt8 -> False; TrueInt8 -> True
		_ -> error "gdkEventAnyRawSendEvent should be FALSE or TRUE")

{-# COMPLETE GdkEventGdkAny #-}

pattern GdkEventGdkAny :: Sealed s GdkEventAnyRaw -> GdkEvent s
pattern GdkEventGdkAny ea <-
	GdkEvent (Sealed . GdkEventAnyRaw_ . castForeignPtr -> ea)

pattern GdkEventGdkNothing :: Sealed s GdkEventAnyRaw -> GdkEvent s
pattern GdkEventGdkNothing ea <-
	GdkEvent (gdkEventTypeRaw GdkEventAnyRaw_ -> (GdkNothing, ea))

pattern GdkEventGdkDelete :: Sealed s GdkEventAnyRaw -> GdkEvent s
pattern GdkEventGdkDelete ea <-
	GdkEvent (gdkEventTypeRaw GdkEventAnyRaw_ -> (GdkDelete, ea))

pattern GdkEventGdkMap :: Sealed s GdkEventAnyRaw -> GdkEvent s
pattern GdkEventGdkMap e <-
	GdkEvent (gdkEventTypeRaw GdkEventAnyRaw_ -> (GdkMap, e))

pattern GdkEventGdkUnmap :: Sealed s GdkEventAnyRaw -> GdkEvent s
pattern GdkEventGdkUnmap e <-
	GdkEvent (gdkEventTypeRaw GdkEventAnyRaw_ -> (GdkUnmap, e))

---------------------------------------------------------------------------
-- COMMON                                                                --
---------------------------------------------------------------------------

foreign import ccall "gdk_event_free"
	c_gdk_event_free :: Ptr GdkEventTag -> IO ()

gdkEventTypeRaw :: (ForeignPtr x -> a) ->
	ForeignPtr GdkEventTag -> (GdkEventType, Sealed s a)
gdkEventTypeRaw c =
	unsafePerformIO . (`withForeignPtr` #{peek GdkEventAny, type}) &&&
	Sealed . c . castForeignPtr

newtype MilliSecond = MilliSecond #{type guint32} deriving (Show, Storable)

---------------------------------------------------------------------------
-- GDK EVENT KEY                                                         --
---------------------------------------------------------------------------

data {-# CTYPE "gdk/gdk.h" "GdkEventKey" #-} GdkEventKey'

foreign import capi "gdkhs.h peek_gdk_event_key_is_modifier"
	c_peek_gdk_event_key_is_modifier :: Ptr GdkEventKey' -> IO BoolCUInt

foreign import capi "gdkhs.h poke_gdk_event_key_is_modifier"
	c_poke_gdk_event_key_is_modifier ::
		Ptr GdkEventKey' -> BoolCUInt -> IO ()

struct "GdkEventKeyRaw" #{size GdkEventKey}
	[	("type", ''GdkEventType, [| #{peek GdkEventKey, type} |],
			[| #{poke GdkEventKey, type} |]),
		("window", ''GdkWindow, [| #{peek GdkEventKey, window} |],
			[| #{poke GdkEventKey, window} |]),
		("sendEvent", ''BoolInt8,
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
	gdkEventKeyWindow :: GdkWindow, gdkEventKeySendEvent :: Bool,
	gdkEventKeyTime :: MilliSecond,
	gdkEventKeyState :: [GdkModifierTypeSingleBit],
	gdkEventKeyKeyval :: GdkKeySym, gdkEventKeyHardwareKeycode :: Word16,
	gdkEventKeyGroup :: Word8, gdkEventKeyIsModifier :: Bool }
	deriving Show

gdkEventKey :: Sealed s GdkEventKeyRaw -> GdkEventKey
gdkEventKey (Sealed r) = GdkEventKey
	(gdkEventKeyRawWindow r)
	(case gdkEventKeyRawSendEvent r of
		FalseInt8 -> False; TrueInt8 -> True
		_ -> error "gdkEventKeyRawSendEvent should be FALSE or TRUE")
	(gdkEventKeyRawTime r)
	(gdkModifierTypeSingleBitList $ gdkEventKeyRawState r)
	(gdkEventKeyRawKeyval r) (gdkEventKeyRawHardwareKeycode r)
	(gdkEventKeyRawGroup r)
	(case gdkEventKeyRawIsModifier r of
		FalseCUInt -> False; TrueCUInt -> True
		_ -> error "gdkEventKeyRawIsModifier should be FALSE or TRUE")

pattern GdkEventGdkKeyPress :: Sealed s GdkEventKeyRaw -> GdkEvent s
pattern GdkEventGdkKeyPress s <-
	GdkEvent (gdkEventTypeRaw GdkEventKeyRaw_ -> (GdkKeyPress, s))

pattern GdkEventGdkKeyRelease :: Sealed s GdkEventKeyRaw -> GdkEvent s
pattern GdkEventGdkKeyRelease e <-
	GdkEvent (gdkEventTypeRaw GdkEventKeyRaw_ -> (GdkKeyRelease, e))

---------------------------------------------------------------------------
-- GDK EVENT BUTTON
---------------------------------------------------------------------------

type PtrCDouble = Ptr CDouble

struct "GdkEventButtonRaw" #{size GdkEventButton}
	[	("type", ''GdkEventType, [| #{peek GdkEventButton, type} |],
			[| #{poke GdkEventButton, type} |]),
		("window", ''GdkWindow, [| #{peek GdkEventButton, window} |],
			[| #{poke GdkEventButton, window} |]),
		("sendEvent", ''BoolInt8,
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
	gdkEventButtonWindow :: GdkWindow, gdkEventButtonSendEvent :: Bool,
	gdkEventButtonTime :: MilliSecond,
	gdkEventButtonX, gdkEventButtonY :: CDouble,
	gdkEventButtonAxes :: GdkAxes,
	gdkEventButtonState :: [GdkModifierTypeSingleBit],
	gdkEventButtonButton :: CUInt,
	gdkEventButtonDevice :: GdkDevice,
	gdkEventButtonXRoot, gdkEventButtonYRoot :: CDouble }
	deriving Show

gdkEventButton :: Sealed s GdkEventButtonRaw -> GdkEventButton
gdkEventButton (Sealed r) = GdkEventButton
	(gdkEventButtonRawWindow r)
	(case gdkEventButtonRawSendEvent r of
		FalseInt8 -> False; TrueInt8 -> True
		_ -> error "gdkEventButtonSendEvent should be FALSE or TRUE")
	(gdkEventButtonRawTime r)
	(gdkEventButtonRawX r) (gdkEventButtonRawY r)
	(unsafePerformIO $ gdkAxesCopyFromPtr
		(gdkEventButtonRawDevice r) (gdkEventButtonRawAxes r))
	(gdkModifierTypeSingleBitList $ gdkEventButtonRawState r)
	(gdkEventButtonRawButton r)
	(gdkEventButtonRawDevice r)
	(gdkEventButtonRawXRoot r) (gdkEventButtonRawYRoot r)

pattern GdkEventGdkButtonPress :: Sealed s GdkEventButtonRaw -> GdkEvent s
pattern GdkEventGdkButtonPress e <-
	GdkEvent (gdkEventTypeRaw GdkEventButtonRaw_ -> (GdkButtonPress, e))

pattern GdkEventGdkDoubleButtonPress :: Sealed s GdkEventButtonRaw -> GdkEvent s
pattern GdkEventGdkDoubleButtonPress e <- GdkEvent
	(gdkEventTypeRaw GdkEventButtonRaw_ -> (GdkDoubleButtonPress, e))

pattern GdkEventGdkTripleButtonPress :: Sealed s GdkEventButtonRaw -> GdkEvent s
pattern GdkEventGdkTripleButtonPress e <- GdkEvent
	(gdkEventTypeRaw GdkEventButtonRaw_ -> (GdkTripleButtonPress, e))

pattern GdkEventGdkButtonRelease :: Sealed s GdkEventButtonRaw -> GdkEvent s
pattern GdkEventGdkButtonRelease e <-
	GdkEvent (gdkEventTypeRaw GdkEventButtonRaw_ -> (GdkButtonRelease, e))

---------------------------------------------------------------------------
-- GDK EVENT SCROLL                                                      --
---------------------------------------------------------------------------

enum "GdkScrollDirection" ''#{type GdkScrollDirection} [''Show, ''Storable] [
	("GdkScrollUp", #{const GDK_SCROLL_UP}),
	("GdkScrollDown", #{const GDK_SCROLL_DOWN}),
	("GdkScrollLeft", #{const GDK_SCROLL_LEFT}),
	("GdkScrollRight", #{const GDK_SCROLL_RIGHT}),
	("GdkScrollSmooth", #{const GDK_SCROLL_SMOOTH}) ]

data {-# CTYPE "gdk/gdk.h" "GdkEventScroll" #-} GdkEventScroll'

foreign import capi "gdkhs.h peek_gdk_event_scroll_is_stop"
	c_peek_gdk_event_scroll_is_stop :: Ptr GdkEventScroll' -> IO BoolCUInt

foreign import capi "gdkhs.h poke_gdk_event_scroll_is_stop"
	c_poke_gdk_event_scroll_is_stop ::
		Ptr GdkEventScroll' -> BoolCUInt -> IO ()

struct "GdkEventScrollRaw" #{size GdkEventScroll}
	[	("type", ''GdkEventType, [| #{peek GdkEventScroll, type} |],
			[| #{poke GdkEventScroll, type} |]),
		("window", ''GdkWindow, [| #{peek GdkEventScroll, window} |],
			[| #{poke GdkEventScroll, window} |]),
		("sendEvent", ''BoolInt8,
			[| #{peek GdkEventScroll, send_event} |],
			[| #{poke GdkEventScroll, send_event} |]),
		("time", ''MilliSecond, [| #{peek GdkEventScroll, time} |],
			[| #{poke GdkEventScroll, time} |]),
		("x", ''CDouble, [| #{peek GdkEventScroll, x} |],
			[| #{poke GdkEventScroll, x} |]),
		("y", ''CDouble, [| #{peek GdkEventScroll, y} |],
			[| #{poke GdkEventScroll, y} |]),
		("state", ''GdkModifierTypeMultiBits,
			[| #{peek GdkEventScroll, state} |],
			[| #{poke GdkEventScroll, state} |]),
		("direction", ''GdkScrollDirection,
			[| #{peek GdkEventScroll, direction} |],
			[| #{poke GdkEventScroll, direction} |]),
		("device", ''GdkDevice,
			[| #{peek GdkEventScroll, device} |],
			[| #{poke GdkEventScroll, device} |]),
		("xRoot", ''CDouble, [| #{peek GdkEventScroll, x_root} |],
			[| #{poke GdkEventScroll, x_root} |]),
		("yRoot", ''CDouble, [| #{peek GdkEventScroll, y_root} |],
			[| #{poke GdkEventScroll, y_root} |]),
		("deltaX", ''CDouble, [| #{peek GdkEventScroll, delta_x} |],
			[| #{poke GdkEventScroll, delta_x} |]),
		("deltaY", ''CDouble, [| #{peek GdkEventScroll, delta_y} |],
			[| #{poke GdkEventScroll, delta_y} |]),
		("isStop", ''BoolCUInt,
			[| c_peek_gdk_event_scroll_is_stop . castPtr |],
			[| c_poke_gdk_event_scroll_is_stop . castPtr |]) ]
	[''Show]

data GdkEventScroll = GdkEventScroll {
	gdkEventScrollWindow :: GdkWindow, gdkEventScrollSendEvent :: Bool,
	gdkEventScrollTime :: MilliSecond,
	gdkEventScrollX, gdkEventScrollY :: CDouble,
	gdkEventScrollState :: [GdkModifierTypeSingleBit],
	gdkEventScrollDirection :: GdkScrollDirection,
	gdkEventScrollDevice :: GdkDevice,
	gdkEventScrollXRoot, gdkEventScrollYRoot :: CDouble,
	gdkEventScrollDeltas :: Maybe (CDouble, CDouble),
	gdkEventScrollIsStop :: Bool }
	deriving Show

gdkEventScroll :: Sealed s GdkEventScrollRaw -> GdkEventScroll
gdkEventScroll (Sealed s) = GdkEventScroll
	(gdkEventScrollRawWindow s)
	(case gdkEventScrollRawSendEvent s of
		FalseInt8 -> False; TrueInt8 -> True
		_ -> error "gdkEventScrollRawSendEvent should be FALSE or TRUE")
	(gdkEventScrollRawTime s)
	(gdkEventScrollRawX s) (gdkEventScrollRawY s)
	(gdkModifierTypeSingleBitList $ gdkEventScrollRawState s)
	(gdkEventScrollRawDirection s)
	(gdkEventScrollRawDevice s)
	(gdkEventScrollRawXRoot s) (gdkEventScrollRawYRoot s)
	(case gdkEventScrollRawDirection s of
		GdkScrollSmooth -> Just
			(gdkEventScrollRawDeltaX s, gdkEventScrollRawDeltaY s)
		_ -> Nothing)
	(case gdkEventScrollRawIsStop s of
		FalseCUInt -> False; TrueCUInt -> True
		_ -> error "never occur")

pattern GdkEventGdkScroll :: Sealed s GdkEventScrollRaw -> GdkEvent s
pattern GdkEventGdkScroll s <-
	GdkEvent (gdkEventTypeRaw GdkEventScrollRaw_ -> (GdkScroll, s))

---------------------------------------------------------------------------
-- GDK EVENT MOTION                                                      --
---------------------------------------------------------------------------

struct "GdkEventMotionRaw" #{size GdkEventMotion}
	[	("type", ''GdkEventType, [| #{peek GdkEventMotion, type} |],
			[| #{poke GdkEventMotion, type} |]),
		("window", ''GdkWindow, [| #{peek GdkEventMotion, window} |],
			[| #{poke GdkEventMotion, window} |]),
		("sendEvent", ''BoolInt8,
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
		("isHint", ''BoolInt16, [| #{peek GdkEventMotion, is_hint} |],
			[| #{poke GdkEventMotion, is_hint} |]),
		("device", ''GdkDevice, [| #{peek GdkEventMotion, device} |],
			[| #{poke GdkEventMotion, device} |]),
		("xRoot", ''CDouble, [| #{peek GdkEventMotion, x_root} |],
			[| #{poke GdkEventMotion, x_root} |]),
		("yRoot", ''CDouble, [| #{peek GdkEventMotion, y_root} |],
			[| #{poke GdkEventMotion, y_root} |]) ]
	[''Show]

data GdkEventMotion = GdkEventMotion {
	gdkEventMotionWindow :: GdkWindow, gdkEventMotionSendEvent :: Bool,
	gdkEventMotionTime :: MilliSecond,
	gdkEventMotionX :: CDouble, gdkEventMotionY :: CDouble,
	gdkEventMotionAxes :: GdkAxes,
	gdkEventMotionState :: [GdkModifierTypeSingleBit],
	gdkEventMotionIsHint :: Bool,
	gdkEventMotionDevice :: GdkDevice,
	gdkEventMotionXRoot :: CDouble, gdkEventMotionYRoot :: CDouble }
	deriving Show

gdkEventMotion :: Sealed s GdkEventMotionRaw -> GdkEventMotion
gdkEventMotion (Sealed r) = GdkEventMotion
	(gdkEventMotionRawWindow r)
	(case gdkEventMotionRawSendEvent r of
		FalseInt8 -> False; TrueInt8 -> True
		_ -> error "gdkEventMotionRawSendEvent should be FALSE or TRUE")
	(gdkEventMotionRawTime r)
	(gdkEventMotionRawX r) (gdkEventMotionRawY r)
	(unsafePerformIO $ gdkAxesCopyFromPtr
		(gdkEventMotionRawDevice r) (gdkEventMotionRawAxes r))
	(gdkModifierTypeSingleBitList $ gdkEventMotionRawState r)
	(case gdkEventMotionRawIsHint r of
		FalseInt16 -> False; TrueInt16 -> True
		_ -> error "gdkEventMotionRawIsHint should be FALSE or TRUE")
	(gdkEventMotionRawDevice r)
	(gdkEventMotionRawXRoot r) (gdkEventMotionRawYRoot r)

pattern GdkEventGdkMotionNotify :: Sealed s GdkEventMotionRaw -> GdkEvent s
pattern GdkEventGdkMotionNotify s <-
	GdkEvent (gdkEventTypeRaw GdkEventMotionRaw_ -> (GdkMotionNotify, s))

---------------------------------------------------------------------------
-- GDK EVENT VISIBILITY                                                  --
---------------------------------------------------------------------------

enum "GdkVisibilityState" ''#{type GdkVisibilityState} [''Show, ''Storable] [
	("GdkVisibilityUnobscured", #{const GDK_VISIBILITY_UNOBSCURED}),
	("GdkVisibilityPartial", #{const GDK_VISIBILITY_PARTIAL}),
	("GdkVisibilityFullyObscured", #{const GDK_VISIBILITY_FULLY_OBSCURED}) ]

struct "GdkEventVisibilityRaw" #{size GdkEventVisibility}
	[	("type", ''GdkEventType, [| #{peek GdkEventVisibility, type} |],
			[| #{poke GdkEventVisibility, type} |]),
		("window", ''GdkWindow,
			[| #{peek GdkEventVisibility, window} |],
			[| #{poke GdkEventVisibility, window} |]),
		("sendEvent", ''BoolInt8,
			[| #{peek GdkEventVisibility, send_event} |],
			[| #{poke GdkEventVisibility, send_event} |]),
		("state", ''GdkVisibilityState,
			[| #{peek GdkEventVisibility, state} |],
			[| #{poke GdkEventVisibility, state} |]) ]
	[''Show]

data GdkEventVisibility = GdkEventVisibility {
	gdkEventVisibilityWindow :: GdkWindow,
	gdkEventVisibilitySendEvent :: Bool,
	gdkEventVisibilityState :: GdkVisibilityState }
	deriving Show

gdkEventVisibility :: Sealed s GdkEventVisibilityRaw -> GdkEventVisibility
gdkEventVisibility (Sealed r) = GdkEventVisibility
	(gdkEventVisibilityRawWindow r)
	(case gdkEventVisibilityRawSendEvent r of
		FalseInt8 -> False; TrueInt8 -> True
		_ -> error $ "gdkEventVisibilityRawSendEvent "
			++ "should be FALSE or TRUE")
	(gdkEventVisibilityRawState r)

pattern GdkEventGdkVisibilityNotify ::
	Sealed s GdkEventVisibilityRaw -> GdkEvent s
pattern GdkEventGdkVisibilityNotify e <- GdkEvent
	(gdkEventTypeRaw GdkEventVisibilityRaw_ -> (GdkVisibilityNotify, e))

---------------------------------------------------------------------------
-- GDK EVENT CROSSING                                                    --
---------------------------------------------------------------------------

struct "GdkEventCrossingRaw" #{size GdkEventCrossing}
	[	("type", ''GdkEventType, [| #{peek GdkEventCrossing, type} |],
			[| #{poke GdkEventCrossing, type} |]),
		("window", ''GdkWindow, [| #{peek GdkEventCrossing, window} |],
			[| #{poke GdkEventCrossing, window} |]),
		("sendEvent", ''BoolInt8,
			[| #{peek GdkEventCrossing, send_event} |],
			[| #{poke GdkEventCrossing, send_event} |]),
		("subwindow", ''GdkWindow,
			[| #{peek GdkEventCrossing, subwindow} |],
			[| #{poke GdkEventCrossing, subwindow} |]),
		("time", ''MilliSecond, [| #{peek GdkEventCrossing, time} |],
			[| #{poke GdkEventCrossing, time} |]),
		("x", ''CDouble, [| #{peek GdkEventCrossing, x} |],
			[| #{poke GdkEventCrossing, x} |]),
		("y", ''CDouble, [| #{peek GdkEventCrossing, y} |],
			[| #{poke GdkEventCrossing, y} |])
		]
	[''Show]

pattern GdkEventGdkEnterNotify :: Sealed s GdkEventCrossingRaw -> GdkEvent s
pattern GdkEventGdkEnterNotify e <- GdkEvent
	(gdkEventTypeRaw GdkEventCrossingRaw_ -> (GdkEnterNotify, e))

---------------------------------------------------------------------------
-- GDK EVENT FOCUS                                                       --
---------------------------------------------------------------------------

tryGdkEventSealedMapWindow :: Sealed s GdkEventAnyRaw -> GdkWindow
tryGdkEventSealedMapWindow (Sealed e) = gdkEventAnyRawWindow e

gdkEventFocusWindow :: GdkEventFocus -> IO GdkWindow
gdkEventFocusWindow (GdkEventFocus p) =
	GdkWindow <$> withForeignPtr p #peek GdkEventFocus, window

newtype GdkEventFocus = GdkEventFocus (ForeignPtr GdkEventFocus) deriving Show

pattern GdkEventSealedGdkFocusChange :: Sealed s GdkEventFocus -> GdkEvent s
pattern GdkEventSealedGdkFocusChange e <-
	GdkEvent (gdkEventTypeRaw GdkEventFocus -> (GdkFocusChange, e))

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
		("sendEvent", ''BoolInt8,
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
	Sealed s GdkEventConfigureRaw -> GdkEvent s
pattern GdkEventSealedGdkConfigure e <-
	GdkEvent (gdkEventTypeRaw GdkEventConfigureRaw_ -> (GdkConfigure, e))

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
	Sealed s GdkEventWindowStateRaw -> GdkEvent s
pattern GdkEventSealedGdkWindowState e <-
	GdkEvent (gdkEventTypeRaw GdkEventWindowStateRaw_ -> (GdkWindowState_, e))
