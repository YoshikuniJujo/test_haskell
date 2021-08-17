{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Windows (
	-- * GDK WINDOW AND GDK WINDOW AUTO UNREF
	GdkWindow, GdkWindowAutoUnref, withGdkWindowAutoUnref,

	-- * NEW, DESTROY AND SHOW
	gdkWindowNew, gdkWindowDestroy,
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
	GdkWindowType,
	pattern GdkWindowRoot, pattern GdkWindowToplevel,
	pattern GdkWindowChild, pattern GdkWindowTemp, pattern GdkWindowForeign,
	pattern GdkWindowOffscreen, pattern GdkWindowSubsurface,

	-- * IS DESTROYED, VISIBLE, VIEWABLE, INPUT ONLY OR SHAPED
	gdkWindowIsDestroyed, gdkWindowIsVisible, gdkWindowIsViewable,
	gdkWindowIsInputOnly, gdkWindowIsShaped,

	-- * GDK WINDOW STATES
	gdkWindowGetState,
	gdkWindowWithdraw,
	gdkWindowIconify, gdkWindowDeiconify, gdkWindowStick, gdkWindowUnstick,
	gdkWindowMaximize, gdkWindowUnmaximize,
	gdkWindowFullscreen, gdkWindowUnfullscreen,
	-- ** GDK FULLSCREEN MODE
	GdkFullscreenMode,
	pattern GdkFullscreenOnCurrentMonitor,
	pattern GdkFullscreenOnAllMonitors,
	gdkWindowGetFullscreenMode, gdkWindowSetFullscreenMode,

	-- * GEOMETRY AND OPACITY
	gdkWindowSetKeepAbove, gdkWindowSetKeepBelow,

	gdkWindowMove, gdkWindowResize, gdkWindowMoveResize,
	gdkWindowRaise, gdkWindowLower, gdkWindowFocus,

	gdkWindowGetGeometry,
	gdkWindowGetWidth, gdkWindowGetHeight,
	gdkWindowGetPosition, gdkWindowGetRootOrigin, gdkWindowGetFrameExtents,
	gdkWindowGetOrigin, gdkWindowGetRootCoords,

	gdkWindowSetOpacity,

	-- * WINDOW BEHAVIER AND APPEARANCE
	-- ** Pass Through
	gdkWindowSetPassThrough, gdkWindowGetPassThrough,
	-- ** Modal Hint
	gdkWindowSetModalHint, gdkWindowGetModalHint,
	-- ** Window Type Hint
	gdkWindowSetTypeHint, gdkWindowGetTypeHint,
	GdkWindowTypeHint,
	pattern GdkWindowTypeHintNormal, pattern GdkWindowTypeHintDialog,
	-- ** Task Bar, Pager and Urgency
	gdkWindowSetSkipTaskbarHint, gdkWindowSetSkipPagerHint,
	gdkWindowSetUrgencyHint,
	-- ** Transient For
	gdkWindowSetTransientFor,
	-- ** Gdk Wm Decoration
	gdkWindowSetDecorations, gdkWindowGetDecorations,
	GdkWMDecoration, GdkWMDecorations, gdkWMDecorations, gdkWMDecorationList,
	pattern GdkDecorAll, pattern GdkDecorBorder, pattern GdkDecorResizeh,
	pattern GdkDecorTitle, pattern GdkDecorMenu, pattern GdkDecorMinimize,
	pattern GdkDecorMaximize,

	) where

import Graphics.Gdk.Windows.Internal
