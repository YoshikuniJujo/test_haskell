{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.EventStructures (

	-- * GDK EVENT
	GdkEvent,

	-- * SEALED
	Sealed,

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
	GdkWindowStates, GdkWindowState, gdkWindowStateList, gdkWindowStateCheck,
	pattern GdkWindowStateWithdrawn, pattern GdkWindowStateIconified,
	pattern GdkWindowStateMaximized, pattern GdkWindowStateSticky,
	pattern GdkWindowStateFullscreen,
	pattern GdkWindowStateAbove, pattern GdkWindowStateBelow,
	pattern GdkWindowStateFocused, pattern GdkWindowStateTiled,
	pattern GdkWindowStateTopTiled, pattern GdkWindowStateTopResizable,
	pattern GdkWindowStateRightTiled, pattern GdkWindowStateRightResizable,
	pattern GdkWindowStateBottomTiled, pattern GdkWindowStateBottomResizable,
	pattern GdkWindowStateLeftTiled, pattern GdkWindowStateLeftResizable ) where

import Graphics.Gdk.EventStructures.Internal
