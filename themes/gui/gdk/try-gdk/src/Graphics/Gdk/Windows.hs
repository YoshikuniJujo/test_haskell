{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Windows (
	-- * GDK WINDOW AND GDK WINDOW AUTO UNREF
	GdkWindow, GdkWindowAutoUnref, withGdkWindowAutoUnref,

	-- * NEW, DESTROY AND SHOW
	gdkWindowNew, gdkToplevelNew, gdkWindowDestroy,
	gdkWindowShow, gdkWindowShowUnraised, gdkWindowHide,

	-- * DISPLAY, SCREEN, VISUAL AND WINDOW
	gdkWindowGetDisplay, gdkWindowGetScreen, gdkWindowGetVisual,
	gdkGetDefaultRootWindow, gdkWindowGetToplevel,
	gdkWindowGetParent, gdkWindowReparent, gdkWindowPeekChildren,

	-- * EVENT
	-- ** Event Mask
	gdkWindowGetEvents, gdkWindowSetEvents,
	-- ** Event Compression
	gdkWindowGetEventCompression, gdkWindowSetEventCompression,

	-- * TITLE AND CURSOR
	gdkWindowSetTitle, gdkWindowSetCursor, gdkWindowGetCursor,

	-- * WITH DRAW FRAME
	gdkWindowWithDrawFrame, gdkWindowGetVisibleRegion,

	-- * WINDOW TYPE
	gdkWindowGetWindowType,

	-- * IS DESTROYED, VISIBLE, VIEWABLE, INPUT ONLY OR SHAPED
	gdkWindowIsDestroyed, gdkWindowIsVisible, gdkWindowIsViewable,
	gdkWindowIsInputOnly, gdkWindowIsShaped,

	-- * GDK WINDOW STATES
	gdkWindowGetState,
	gdkWindowWithdraw,
	gdkWindowIconify, gdkWindowDeiconify, gdkWindowStick, gdkWindowUnstick,
	gdkWindowMaximize, gdkWindowUnmaximize,
	gdkWindowFullscreen, gdkWindowUnfullscreen,
	-- ** Gdk Fullscreen Mode
	gdkWindowGetFullscreenMode, gdkWindowSetFullscreenMode,
	GdkFullscreenMode,
	pattern GdkFullscreenOnCurrentMonitor,
	pattern GdkFullscreenOnAllMonitors,

	-- * GEOMETRY AND OPACITY
	-- ** Z Axis
	gdkWindowSetKeepAbove, gdkWindowSetKeepBelow,
	gdkWindowRaise, gdkWindowLower, gdkWindowFocus,
	gdkWindowSetOpacity,
	-- ** XY Axis
	-- *** get position and size
	-- **** window
	gdkWindowGetGeometry,
	gdkWindowGetPosition, gdkWindowGetWidth, gdkWindowGetHeight,
	gdkWindowGetOrigin, gdkWindowGetRootCoords,
	-- **** frame
	gdkWindowGetFrameExtents, gdkWindowGetRootOrigin,
	-- *** move and resize
	gdkWindowMoveResize, gdkWindowMove, gdkWindowResize,

	-- * WINDOW BEHAVIER AND APPEARANCE
	-- ** Transient For
	gdkWindowSetTransientFor,
	-- ** Modal Hint
	gdkWindowSetModalHint, gdkWindowGetModalHint,
	-- ** Window Type Hint
	gdkWindowSetTypeHint, gdkWindowGetTypeHint,
	-- ** Task Bar, Pager and Urgency
	gdkWindowSetSkipTaskbarHint, gdkWindowSetSkipPagerHint,
	gdkWindowSetUrgencyHint,
	-- ** Gdk Wm Decoration
	gdkWindowSetDecorations, gdkWindowGetDecorations,
	GdkWmDecorations, gdkWmDecorations,
	GdkWmDecoration, gdkWmDecorationList,
	pattern GdkDecorAll, pattern GdkDecorBorder, pattern GdkDecorResizeh,
	pattern GdkDecorTitle, pattern GdkDecorMenu, pattern GdkDecorMinimize,
	pattern GdkDecorMaximize,

	) where

import Graphics.Gdk.Windows.Internal
