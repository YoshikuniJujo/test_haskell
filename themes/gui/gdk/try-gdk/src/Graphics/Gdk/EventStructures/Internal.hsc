{-# LANGUAGE TemplateHaskell, CApiFFI #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.EventStructures.Internal (

	-- * GDK EVENT
	GdkEvent(..), GdkEventTag, c_gdk_event_free,

	-- * SEALED
	Sealed, seal, unsafeUnseal,

	-- * GDK EVENT ANY
	GdkEventAny(..), GdkEventAnyRaw, gdkEventAny,
	pattern GdkEventGdkAny, pattern GdkEventGdkNothing,
	pattern GdkEventGdkDelete, pattern GdkEventGdkDestroy,
	pattern GdkEventGdkMap, pattern GdkEventGdkUnmap,

	-- * GDK EVENT KEY
	GdkEventKey(..), GdkEventKeyRaw, gdkEventKey,
	pattern GdkEventGdkKeyPress, pattern GdkEventGdkKeyRelease,
	MilliSecond(..),

	-- * GDK EVENT BUTTON
	GdkEventButton(..), GdkEventButtonRaw, gdkEventButton,
	pattern GdkEventGdkButtonPress, pattern GdkEventGdkButtonRelease,
	pattern GdkEventGdkDoubleButtonPress,
	pattern GdkEventGdkTripleButtonPress,

	-- * GDK EVENT SCROLL
	GdkEventScroll(..), GdkEventScrollRaw, gdkEventScroll,
	pattern GdkEventGdkScroll,
	GdkScrollDirection,
	pattern GdkScrollUp, pattern GdkScrollDown,
	pattern GdkScrollLeft, pattern GdkScrollRight,
	pattern GdkScrollSmooth,

	-- * GDK EVENT MOTION
	GdkEventMotion(..), GdkEventMotionRaw, gdkEventMotion,
	pattern GdkEventGdkMotionNotify,

	-- * GDK EVENT VISIBILITY
	GdkEventVisibility(..), GdkEventVisibilityRaw, gdkEventVisibility,
	pattern GdkEventGdkVisibilityNotify,
	GdkVisibilityState,
	pattern GdkVisibilityUnobscured, pattern GdkVisibilityPartial,
	pattern GdkVisibilityFullyObscured,

	-- * GDK EVENT CROSSING
	GdkEventCrossing(..), GdkEventCrossingRaw, gdkEventCrossing,
	pattern GdkEventGdkEnterNotify, pattern GdkEventGdkLeaveNotify,
	-- ** Gdk Crossing Mode
	GdkCrossingMode,
	pattern GdkCrossingNormal,
	pattern GdkCrossingGrab, pattern GdkCrossingUngrab,
	pattern GdkCrossingGtkGrab, pattern GdkCrossingGtkUngrab,
	pattern GdkCrossingStateChanged,
	pattern GdkCrossingTouchBegin, pattern GdkCrossingTouchEnd,
	pattern GdkCrossingDeviceSwitch,
	-- ** Gdk Notify Type
	GdkNotifyType,
	pattern GdkNotifyAncestor, pattern GdkNotifyVirtual,
	pattern GdkNotifyInferior, pattern GdkNotifyNonlinear,
	pattern GdkNotifyNonlinearVirtual, pattern GdkNotifyUnknown,

	-- * GDK EVENT FOCUS
	GdkEventFocus(..), GdkEventFocusRaw, gdkEventFocus,
	pattern GdkEventGdkFocusChange,

	-- * GDK EVENT CONFIGURE
	GdkEventConfigure(..), GdkEventConfigureRaw, gdkEventConfigure,
	pattern GdkEventGdkConfigure,

	-- * GDK EVENT PROPERTY
	GdkEventProperty(..), GdkEventPropertyRaw, gdkEventProperty,
	pattern GdkEventGdkPropertyNotify,
	-- ** Gdk Property State
	GdkPropertyState,
	pattern GdkPropertyNewValue, pattern GdkPropertyDelete,

	-- * GDK EVENT WINDOW STATE
	GdkEventWindowState(..), GdkEventWindowStateRaw, gdkEventWindowState,
	pattern GdkEventGdkWindowState,
	-- ** Gdk Window State
	GdkWindowStates(..), GdkWindowState, gdkWindowStateList, gdkWindowStateCheck,
	pattern GdkWindowStateWithdrawn, pattern GdkWindowStateIconified,
	pattern GdkWindowStateMaximized, pattern GdkWindowStateSticky,
	pattern GdkWindowStateFullscreen,
	pattern GdkWindowStateAbove, pattern GdkWindowStateBelow,
	pattern GdkWindowStateFocused, pattern GdkWindowStateTiled,
	pattern GdkWindowStateTopTiled, pattern GdkWindowStateTopResizable,
	pattern GdkWindowStateRightTiled, pattern GdkWindowStateRightResizable,
	pattern GdkWindowStateBottomTiled, pattern GdkWindowStateBottomResizable,
	pattern GdkWindowStateLeftTiled, pattern GdkWindowStateLeftResizable ) where

import Foreign.Ptr
import Foreign.Ptr.Misc
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.Enum
import Foreign.C.Struct
import Control.Arrow
import Data.Bits
import Data.Bits.Misc
import Data.Word
import Data.Int
import Data.Sealed.Internal
import System.IO.Unsafe

import Graphics.Gdk.GdkDevice.Internal
import Graphics.Gdk.GdkDevice.GdkAxes.Internal
import {-# SOURCE #-} Graphics.Gdk.Windows.Internal
import Graphics.Gdk.Windows.GdkModifierType
import Graphics.Gdk.EventStructures.GdkEventType
import Graphics.Gdk.EventStructures.GdkKeySyms
import Graphics.Gdk.PropertiesAndAtoms.GdkAtom

#include <gdk/gdk.h>

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

enum "GBoolean" ''#{type gboolean} [''Show, ''Storable, ''Eq, ''Num] [
	("FalseGBoolean", #{const FALSE}), ("TrueGBoolean" , #{const TRUE}) ]

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
gdkEventAny (unsafeUnseal -> r) = GdkEventAny
	(gdkEventAnyRawType r) (gdkEventAnyRawWindow r)
	(case gdkEventAnyRawSendEvent r of
		FalseInt8 -> False; TrueInt8 -> True
		_ -> error "gdkEventAnyRawSendEvent should be FALSE or TRUE")

{-# COMPLETE GdkEventGdkAny #-}

pattern GdkEventGdkAny :: Sealed s GdkEventAnyRaw -> GdkEvent s
pattern GdkEventGdkAny ea <-
	GdkEvent (seal . GdkEventAnyRaw_ . castForeignPtr -> ea)

pattern GdkEventGdkNothing :: Sealed s GdkEventAnyRaw -> GdkEvent s
pattern GdkEventGdkNothing ea <-
	GdkEvent (gdkEventTypeRaw GdkEventAnyRaw_ -> (GdkNothing, ea))

pattern GdkEventGdkDelete :: Sealed s GdkEventAnyRaw -> GdkEvent s
pattern GdkEventGdkDelete ea <-
	GdkEvent (gdkEventTypeRaw GdkEventAnyRaw_ -> (GdkDelete, ea))

pattern GdkEventGdkDestroy :: Sealed s GdkEventAnyRaw -> GdkEvent s
pattern GdkEventGdkDestroy ea <-
	GdkEvent (gdkEventTypeRaw GdkEventAnyRaw_ -> (GdkDestroy, ea))

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
	seal . c . castForeignPtr

newtype MilliSecond = MilliSecond #{type guint32}
	deriving (Show, Eq, Ord, Storable)

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
	[	("window", ''GdkWindow, [| #{peek GdkEventKey, window} |],
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
	gdkEventKeyState :: GdkModifierTypeMultiBits,
	gdkEventKeyKeyval :: GdkKeySym, gdkEventKeyHardwareKeycode :: Word16,
	gdkEventKeyGroup :: Word8, gdkEventKeyIsModifier :: Bool }
	deriving Show

gdkEventKey :: Sealed s GdkEventKeyRaw -> GdkEventKey
gdkEventKey (unsafeUnseal -> r) = GdkEventKey
	(gdkEventKeyRawWindow r)
	(case gdkEventKeyRawSendEvent r of
		FalseInt8 -> False; TrueInt8 -> True
		_ -> error "gdkEventKeyRawSendEvent should be FALSE or TRUE")
	(gdkEventKeyRawTime r)
	(gdkEventKeyRawState r)
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
	[	("window", ''GdkWindow, [| #{peek GdkEventButton, window} |],
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
	gdkEventButtonState :: GdkModifierTypeMultiBits,
	gdkEventButtonButton :: CUInt,
	gdkEventButtonDevice :: GdkDeviceMaster 'Pointer,
	gdkEventButtonSourceDevice :: Maybe (GdkDevicePhysical 'Pointer),
	gdkEventButtonXRoot, gdkEventButtonYRoot :: CDouble }
	deriving Show

foreign import ccall "gdk_event_get_source_device"
	c_gdk_event_get_source_device :: Ptr re -> IO GdkDevice

gdkEventButton :: Sealed s GdkEventButtonRaw -> GdkEventButton
gdkEventButton (unsafeUnseal -> r@(GdkEventButtonRaw_ fe)) = GdkEventButton
	(gdkEventButtonRawWindow r)
	(case gdkEventButtonRawSendEvent r of
		FalseInt8 -> False; TrueInt8 -> True
		_ -> error "gdkEventButtonSendEvent should be FALSE or TRUE")
	(gdkEventButtonRawTime r)
	(gdkEventButtonRawX r) (gdkEventButtonRawY r)
	(unsafePerformIO $ gdkAxesCopyFromPtr
		(gdkEventButtonRawDevice r) (gdkEventButtonRawAxes r))
	(gdkEventButtonRawState r)
	(gdkEventButtonRawButton r)
	(toGdkDeviceMasterPointer $ gdkEventButtonRawDevice r)
	(toGdkDevicePhysicalPointer . unsafePerformIO
		$ withForeignPtr fe c_gdk_event_get_source_device)
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

toGdkDeviceMasterPointer :: GdkDevice -> GdkDeviceMaster 'Pointer
toGdkDeviceMasterPointer d@(GdkDevice pd) = unsafePerformIO do
	b <- checkGdkDeviceMasterPointer d
	pure $ if b then GdkDeviceMaster pd
		else error "The device which a event hsa should be master pointer"

toGdkDevicePhysicalPointer :: GdkDevice -> Maybe (GdkDevicePhysical 'Pointer)
toGdkDevicePhysicalPointer d@(GdkDevice pd) = unsafePerformIO do
	b <- checkGdkDevicePhysicalPointer d
	pure $ if b then Just $ GdkDevicePhysical pd else Nothing

checkGdkDeviceMasterPointer :: GdkDevice -> IO Bool
checkGdkDeviceMasterPointer d =
	(&&) <$> checkGdkDeviceIsMaster d <*> checkGdkDeviceIsPointer d

checkGdkDevicePhysicalPointer :: GdkDevice -> IO Bool
checkGdkDevicePhysicalPointer d =
	(&&) <$> (not <$> checkGdkDeviceIsMaster d) <*> checkGdkDeviceIsPointer d

checkGdkDeviceIsMaster :: GdkDevice -> IO Bool
checkGdkDeviceIsMaster d = (<$> gdkDeviceGetDeviceTypeInternal d) \case
	GdkDeviceTypeMaster -> True
	_ -> False

checkGdkDeviceIsPointer :: GdkDevice -> IO Bool
checkGdkDeviceIsPointer d = (<$> gdkDeviceGetSourceInternal d) \case
	GdkSourceKeyboard -> False
	_ -> True

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
	[	("window", ''GdkWindow, [| #{peek GdkEventScroll, window} |],
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
	gdkEventScrollState :: GdkModifierTypeMultiBits,
	gdkEventScrollDirection :: GdkScrollDirection,
	gdkEventScrollDevice :: GdkDeviceMaster 'Pointer,
	gdkEventScrollSourceDevice :: Maybe (GdkDevicePhysical 'Pointer),
	gdkEventScrollXRoot, gdkEventScrollYRoot :: CDouble,
	gdkEventScrollDeltas :: Maybe (CDouble, CDouble),
	gdkEventScrollIsStop :: Bool }
	deriving Show

gdkEventScroll :: Sealed s GdkEventScrollRaw -> GdkEventScroll
gdkEventScroll (unsafeUnseal -> s@(GdkEventScrollRaw_ fs)) = GdkEventScroll
	(gdkEventScrollRawWindow s)
	(case gdkEventScrollRawSendEvent s of
		FalseInt8 -> False; TrueInt8 -> True
		_ -> error "gdkEventScrollRawSendEvent should be FALSE or TRUE")
	(gdkEventScrollRawTime s)
	(gdkEventScrollRawX s) (gdkEventScrollRawY s)
	(gdkEventScrollRawState s)
	(gdkEventScrollRawDirection s)
	(toGdkDeviceMasterPointer $ gdkEventScrollRawDevice s)
	(toGdkDevicePhysicalPointer . unsafePerformIO
		$ withForeignPtr fs c_gdk_event_get_source_device)
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
	[	("window", ''GdkWindow, [| #{peek GdkEventMotion, window} |],
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
	gdkEventMotionState :: GdkModifierTypeMultiBits,
	gdkEventMotionIsHint :: Bool,
	gdkEventMotionDevice :: GdkDeviceMaster 'Pointer,
	gdkEventMotionSourceDevice :: Maybe (GdkDevicePhysical 'Pointer),
	gdkEventMotionXRoot :: CDouble, gdkEventMotionYRoot :: CDouble }
	deriving Show

gdkEventMotion :: Sealed s GdkEventMotionRaw -> GdkEventMotion
gdkEventMotion (unsafeUnseal -> r@(GdkEventMotionRaw_ fr)) = GdkEventMotion
	(gdkEventMotionRawWindow r)
	(case gdkEventMotionRawSendEvent r of
		FalseInt8 -> False; TrueInt8 -> True
		_ -> error "gdkEventMotionRawSendEvent should be FALSE or TRUE")
	(gdkEventMotionRawTime r)
	(gdkEventMotionRawX r) (gdkEventMotionRawY r)
	(unsafePerformIO $ gdkAxesCopyFromPtr
		(gdkEventMotionRawDevice r) (gdkEventMotionRawAxes r))
	(gdkEventMotionRawState r)
	(case gdkEventMotionRawIsHint r of
		FalseInt16 -> False; TrueInt16 -> True
		_ -> error "gdkEventMotionRawIsHint should be FALSE or TRUE")
	(toGdkDeviceMasterPointer $ gdkEventMotionRawDevice r)
	(toGdkDevicePhysicalPointer . unsafePerformIO $
		withForeignPtr fr c_gdk_event_get_source_device)
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
	[	("window", ''GdkWindow,
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
gdkEventVisibility (unsafeUnseal -> r) = GdkEventVisibility
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

enum "GdkCrossingMode" ''#{type GdkCrossingMode} [''Show, ''Storable] [
	("GdkCrossingNormal", #{const GDK_CROSSING_NORMAL}),
	("GdkCrossingGrab", #{const GDK_CROSSING_GRAB}),
	("GdkCrossingUngrab", #{const GDK_CROSSING_UNGRAB}),
	("GdkCrossingGtkGrab", #{const GDK_CROSSING_GTK_GRAB}),
	("GdkCrossingGtkUngrab", #{const GDK_CROSSING_GTK_UNGRAB}),
	("GdkCrossingStateChanged", #{const GDK_CROSSING_STATE_CHANGED}),
	("GdkCrossingTouchBegin", #{const GDK_CROSSING_TOUCH_BEGIN}),
	("GdkCrossingTouchEnd", #{const GDK_CROSSING_TOUCH_END}),
	("GdkCrossingDeviceSwitch", #{const GDK_CROSSING_DEVICE_SWITCH}) ]

enum "GdkNotifyType" ''#{type GdkNotifyType} [''Show, ''Storable] [
	("GdkNotifyAncestor", #{const GDK_NOTIFY_ANCESTOR}),
	("GdkNotifyVirtual", #{const GDK_NOTIFY_VIRTUAL}),
	("GdkNotifyInferior", #{const GDK_NOTIFY_INFERIOR}),
	("GdkNotifyNonlinear", #{const GDK_NOTIFY_NONLINEAR}),
	("GdkNotifyNonlinearVirtual", #{const GDK_NOTIFY_NONLINEAR_VIRTUAL}),
	("GdkNotifyUnknown", #{const GDK_NOTIFY_UNKNOWN}) ]

type PtrGdkWindow = Ptr GdkWindow

struct "GdkEventCrossingRaw" #{size GdkEventCrossing}
	[	("window", ''GdkWindow, [| #{peek GdkEventCrossing, window} |],
			[| #{poke GdkEventCrossing, window} |]),
		("sendEvent", ''BoolInt8,
			[| #{peek GdkEventCrossing, send_event} |],
			[| #{poke GdkEventCrossing, send_event} |]),
		("subwindow", ''PtrGdkWindow,
			[| #{peek GdkEventCrossing, subwindow} |],
			[| #{poke GdkEventCrossing, subwindow} |]),
		("time", ''MilliSecond, [| #{peek GdkEventCrossing, time} |],
			[| #{poke GdkEventCrossing, time} |]),
		("x", ''CDouble, [| #{peek GdkEventCrossing, x} |],
			[| #{poke GdkEventCrossing, x} |]),
		("y", ''CDouble, [| #{peek GdkEventCrossing, y} |],
			[| #{poke GdkEventCrossing, y} |]),
		("xRoot", ''CDouble, [| #{peek GdkEventCrossing, x_root} |],
			[| #{poke GdkEventCrossing, x_root} |]),
		("yRoot", ''CDouble, [| #{peek GdkEventCrossing, y_root} |],
			[| #{poke GdkEventCrossing, y_root} |]),
		("mode", ''GdkCrossingMode,
			[| #{peek GdkEventCrossing, mode} |],
			[| #{poke GdkEventCrossing, mode} |]),
		("detail", ''GdkNotifyType,
			[| #{peek GdkEventCrossing, detail} |],
			[| #{poke GdkEventCrossing, detail} |]),
		("focus", ''GBoolean, [| #{peek GdkEventCrossing, focus} |],
			[| #{poke GdkEventCrossing, focus} |]),
		("state", ''GdkModifierTypeMultiBits,
			[| #{peek GdkEventCrossing, state} |],
			[| #{poke GdkEventCrossing, state} |])
		]
	[''Show]

data GdkEventCrossing = GdkEventCrossing {
	gdkEventCrossingWindow :: GdkWindow,
	gdkEventCrossingSendEvent :: Bool,
	gdkEventCrossingSubwindow :: Maybe GdkWindow,
	gdkEventCrossingTime :: MilliSecond,
	gdkEventCrossingX, gdkEventCrossingY :: CDouble,
	gdkEventCrossingXRoot, gdkEventCrossingYRoot :: CDouble,
	gdkEventCrossingMode :: GdkCrossingMode,
	gdkEventCrossingDetail :: GdkNotifyType,
	gdkEventCrossingFocus :: Bool,
	gdkEventCrossingState :: [GdkModifierTypeSingleBit] }
	deriving Show

gdkEventCrossing :: Sealed s GdkEventCrossingRaw -> GdkEventCrossing
gdkEventCrossing (unsafeUnseal -> r) = GdkEventCrossing
	(gdkEventCrossingRawWindow r)
	(case gdkEventCrossingRawSendEvent r of
		FalseInt8 -> False; TrueInt8 -> True
		_ -> error $ "gdkEventCrossingRawSendEvent " ++
			"should be FALSE or TRUE")
	(toGdkWindow $ gdkEventCrossingRawSubwindow r)
	(gdkEventCrossingRawTime r)
	(gdkEventCrossingRawX r) (gdkEventCrossingRawY r)
	(gdkEventCrossingRawXRoot r) (gdkEventCrossingRawYRoot r)
	(gdkEventCrossingRawMode r) (gdkEventCrossingRawDetail r)
	(case gdkEventCrossingRawFocus r of
		FalseGBoolean -> False; TrueGBoolean -> True
		_ -> error $ "gdkEventCrossingRawFocus should be FALSE or TRUE")
	(gdkModifierTypeSingleBitList $ gdkEventCrossingRawState r)

toGdkWindow :: Ptr GdkWindow -> Maybe GdkWindow
toGdkWindow = \case NullPtr -> Nothing; p -> Just $ GdkWindow p

pattern GdkEventGdkEnterNotify :: Sealed s GdkEventCrossingRaw -> GdkEvent s
pattern GdkEventGdkEnterNotify e <- GdkEvent
	(gdkEventTypeRaw GdkEventCrossingRaw_ -> (GdkEnterNotify, e))

pattern GdkEventGdkLeaveNotify :: Sealed s GdkEventCrossingRaw -> GdkEvent s
pattern GdkEventGdkLeaveNotify l <- GdkEvent
	(gdkEventTypeRaw GdkEventCrossingRaw_ -> (GdkLeaveNotify, l))

---------------------------------------------------------------------------
-- GDK EVENT FOCUS                                                       --
---------------------------------------------------------------------------

struct "GdkEventFocusRaw" #{size GdkEventFocus}
	[	("window", ''GdkWindow, [| #{peek GdkEventFocus, window} |],
			[| #{poke GdkEventFocus, window} |]),
		("sendEvent", ''BoolInt8, [| #{peek GdkEventFocus, send_event} |],
			[| #{poke GdkEventFocus, send_event} |]),
		("in", ''BoolInt16, [| #{peek GdkEventFocus, in} |],
			[| #{poke GdkEventFocus, in} |]) ]
	[''Show]

data GdkEventFocus = GdkEventFocus {
	gdkEventFocusWindow :: GdkWindow, gdkEventFocusSendEvent :: Bool,
	gdkEventFocusIn :: Bool }
	deriving Show

gdkEventFocus :: Sealed s GdkEventFocusRaw -> GdkEventFocus
gdkEventFocus (unsafeUnseal -> r) = GdkEventFocus
	(gdkEventFocusRawWindow r)
	(case gdkEventFocusRawSendEvent r of
		FalseInt8 -> False; TrueInt8 -> True;
		_ -> error "gdkEventFocusRawSendEvent shoudl be FALSE or TRUE")
	(case gdkEventFocusRawIn r of
		FalseInt16 -> False; TrueInt16 -> True;
		_ -> error "gdkEventFocusRawIn should be FALSE or TRUE")

pattern GdkEventGdkFocusChange :: Sealed s GdkEventFocusRaw -> GdkEvent s
pattern GdkEventGdkFocusChange e <-
	GdkEvent (gdkEventTypeRaw GdkEventFocusRaw_ -> (GdkFocusChange, e))

---------------------------------------------------------------------------
-- GDK EVENT CONFIGURE                                                   --
---------------------------------------------------------------------------

struct "GdkEventConfigureRaw" #{size GdkEventConfigure}
	[	("window", ''GdkWindow, [| #{peek GdkEventConfigure, window} |],
			[| #{poke GdkEventConfigure, window} |]),
		("sendEvent", ''BoolInt8,
			[| #{peek GdkEventConfigure, send_event} |],
			[| #{poke GdkEventConfigure, send_event} |]),
		("x", ''CInt, [| #{peek GdkEventConfigure, x} |],
			[| #{poke GdkEventConfigure, x} |]),
		("y", ''CInt, [| #{peek GdkEventConfigure, y} |],
			[| #{poke GdkEventConfigure, y} |]),
		("width", ''CInt, [| #{peek GdkEventConfigure, width} |],
			[| #{poke GdkEventConfigure, width} |]),
		("height", ''CInt, [| #{peek GdkEventConfigure, height} |],
			[| #{poke GdkEventConfigure, height} |]) ]
	[''Show]

data GdkEventConfigure = GdkEventConfigure {
	gdkEventConfigureWindow :: GdkWindow,
	gdkEventConfigureSendEvent :: Bool,
	gdkEventConfigureX, gdkEventConfigureY :: CInt,
	gdkEventConfigureWidth, gdkEventConfigureHeight :: CInt }
	deriving Show

gdkEventConfigure :: Sealed s GdkEventConfigureRaw -> GdkEventConfigure
gdkEventConfigure (unsafeUnseal -> r) = GdkEventConfigure
	(gdkEventConfigureRawWindow r)
	(case gdkEventConfigureRawSendEvent r of
		FalseInt8 -> False; TrueInt8 -> True
		_ -> error $ "gdkEventConfigureRawSendEvent " ++
			"should be FALSE or TRUE")
	(gdkEventConfigureRawX r) (gdkEventConfigureRawY r)
	(gdkEventConfigureRawWidth r) (gdkEventConfigureRawHeight r)

pattern GdkEventGdkConfigure :: Sealed s GdkEventConfigureRaw -> GdkEvent s
pattern GdkEventGdkConfigure e <-
	GdkEvent (gdkEventTypeRaw GdkEventConfigureRaw_ -> (GdkConfigure, e))

---------------------------------------------------------------------------
-- GDK EVENT PROPERTY
---------------------------------------------------------------------------

enum "GdkPropertyState" ''#{type GdkPropertyState} [''Show, ''Read, ''Eq, ''Storable] [
	("GdkPropertyNewValue", #{const GDK_PROPERTY_NEW_VALUE}),
	("GdkPropertyDelete", #{const GDK_PROPERTY_DELETE}) ]

struct "GdkEventPropertyRaw" #{size GdkEventProperty}
	[	("window", ''GdkWindow, [| #{peek GdkEventProperty, window} |],
			[| #{poke GdkEventProperty, window} |]),
		("sendEvent", ''BoolInt8,
			[| #{peek GdkEventProperty, send_event} |],
			[| #{poke GdkEventProperty, send_event} |]),
		("atom", ''GdkAtom, [| #{peek GdkEventProperty, atom} |],
			[| #{poke GdkEventProperty, atom} |]),
		("time", ''MilliSecond, [| #{peek GdkEventProperty, time} |],
			[| #{poke GdkEventProperty, time} |]),
		("state", ''GdkPropertyState,
			[| #{peek GdkEventProperty, state} |],
			[| #{poke GdkEventProperty, state} |]) ]
	[''Show]

data GdkEventProperty = GdkEventProperty {
	gdkEventPropertyWindow :: GdkWindow,
	gdkEventPropertySendEvent :: Bool,
	gdkEventPropertyAtom :: GdkAtom,
	gdkEventPropertyTime :: MilliSecond,
	gdkEventPropertyState :: GdkPropertyState }
	deriving Show

gdkEventProperty :: Sealed s GdkEventPropertyRaw -> GdkEventProperty
gdkEventProperty (unsafeUnseal -> r) = GdkEventProperty
	(gdkEventPropertyRawWindow r)
	(case gdkEventPropertyRawSendEvent r of
		FalseInt8 -> False; TrueInt8 -> True
		_ -> error $ "gdkEventPropertyRawSendEvent " ++
			"should be FALSE or TRUE")
	(gdkEventPropertyRawAtom r)
	(gdkEventPropertyRawTime r)
	(gdkEventPropertyRawState r)

pattern GdkEventGdkPropertyNotify :: Sealed s GdkEventPropertyRaw -> GdkEvent s
pattern GdkEventGdkPropertyNotify e <-
	GdkEvent (gdkEventTypeRaw GdkEventPropertyRaw_ -> (GdkPropertyNotify, e))

---------------------------------------------------------------------------
-- GDK EVENT WINDOW STATES
---------------------------------------------------------------------------

enum "GdkWindowStates" ''#{type GdkWindowState} [''Show, ''Storable] [
	("GdkWindowStateZero", 0) ]

enum "GdkWindowState" ''#{type GdkWindowState} [''Show, ''Storable] [
	("GdkWindowStateWithdrawn", #{const GDK_WINDOW_STATE_WITHDRAWN}),
	("GdkWindowStateIconified", #{const GDK_WINDOW_STATE_ICONIFIED}),
	("GdkWindowStateMaximized", #{const GDK_WINDOW_STATE_MAXIMIZED}),
	("GdkWindowStateSticky", #{const GDK_WINDOW_STATE_STICKY}),
	("GdkWindowStateFullscreen", #{const GDK_WINDOW_STATE_FULLSCREEN}),
	("GdkWindowStateAbove", #{const GDK_WINDOW_STATE_ABOVE}),
	("GdkWindowStateBelow", #{const GDK_WINDOW_STATE_BELOW}),
	("GdkWindowStateFocused", #{const GDK_WINDOW_STATE_FOCUSED}),
	("GdkWindowStateTiled", #{const GDK_WINDOW_STATE_TILED}),
	("GdkWindowStateTopTiled", #{const GDK_WINDOW_STATE_TOP_TILED}),
	("GdkWindowStateTopResizable", #{const GDK_WINDOW_STATE_TOP_RESIZABLE}),
	("GdkWindowStateRightTiled", #{const GDK_WINDOW_STATE_RIGHT_TILED}),
	("GdkWindowStateRightResizable",
		#{const GDK_WINDOW_STATE_RIGHT_RESIZABLE}),
	("GdkWindowStateBottomTiled", #{const GDK_WINDOW_STATE_BOTTOM_TILED}),
	("GdkWindowStateBottomResizable",
		#{const GDK_WINDOW_STATE_BOTTOM_RESIZABLE}),
	("GdkWindowStateLeftTiled", #{const GDK_WINDOW_STATE_LEFT_TILED}),
	("GdkWindowStateLeftResizable",
		#{const GDK_WINDOW_STATE_LEFT_RESIZABLE}) ]

gdkWindowStateCheck :: GdkWindowState -> GdkWindowStates -> Bool
gdkWindowStateCheck (GdkWindowState s) (GdkWindowStates ss) =
	s .&. ss /= zeroBits

gdkWindowStateList :: GdkWindowStates -> [GdkWindowState]
gdkWindowStateList (GdkWindowStates ss) = GdkWindowState <$> separateBits 32 ss

struct "GdkEventWindowStateRaw" #{size GdkEventWindowState}
	[	("window", ''GdkWindow,
			[| #{peek GdkEventWindowState, window} |],
			[| #{poke GdkEventWindowState, window} |]),
		("changedMask", ''GdkWindowStates,
			[| #{peek GdkEventWindowState, changed_mask} |],
			[| #{poke GdkEventWindowState, changed_mask} |]),
		("newWindowState", ''GdkWindowStates,
			[| #{peek GdkEventWindowState, new_window_state} |],
			[| #{poke GdkEventWindowState, new_window_state} |]) ]
	[''Show]

data GdkEventWindowState = GdkEventWindowState {
	gdkEventWindowStateWindow :: GdkWindow,
	gdkEventWindowStateChangedMask :: GdkWindowStates,
	gdkEventWindowStateNewWindowState :: GdkWindowStates }
	deriving Show

gdkEventWindowState :: Sealed s GdkEventWindowStateRaw -> GdkEventWindowState
gdkEventWindowState (unsafeUnseal -> r) = GdkEventWindowState
	(gdkEventWindowStateRawWindow r)
	(gdkEventWindowStateRawChangedMask r)
	(gdkEventWindowStateRawNewWindowState r)

pattern GdkEventGdkWindowState :: Sealed s GdkEventWindowStateRaw -> GdkEvent s
pattern GdkEventGdkWindowState e <- GdkEvent
	(gdkEventTypeRaw GdkEventWindowStateRaw_ -> (GdkWindowState_, e))

---------------------------------------------------------------------------
-- GDK EVENT SETTING
---------------------------------------------------------------------------
