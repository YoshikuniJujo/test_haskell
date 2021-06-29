{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Cursors (
	-- * FUNCTION
	gdkCursorNewFromSurface,
	gdkCursorNewFromName,
	gdkCursorGetDisplay,
	gdkCursorGetSurface,
	gdkCursorGetCursorType,

	-- * GDK CURSOR TYPE
	GdkCursorType(..),
	gdkCursorNewForDisplay,
	pattern GdkXCursor, pattern GdkArrow,
	pattern GdkBasedArrowDown, pattern GdkBasedArrowUp,
	pattern GdkBoat, pattern GdkBogosity,
	pattern GdkBottomLeftCorner, pattern GdkBottomRightCorner,
	pattern GdkBottomSide, pattern GdkBottomTee,
	pattern GdkBoxSpiral, pattern GdkCenterPtr,
	pattern GdkCircle, pattern GdkClock,
	pattern GdkCoffeeMug, pattern GdkCross,
	pattern GdkCrossReverse, pattern GdkCrosshair,
	pattern GdkDiamondCross, pattern GdkDot,
	pattern GdkDotbox, pattern GdkDoubleArrow,
	pattern GdkDraftLarge, pattern GdkDraftSmall,
	pattern GdkDrapedBox, pattern GdkExchange,
	pattern GdkFleur, pattern GdkGobbler,
	pattern GdkGumby, pattern GdkHand1,
	pattern GdkHand2, pattern GdkHeart,
	pattern GdkIcon, pattern GdkIronCross,
	pattern GdkLeftPtr, pattern GdkLeftSide,

	) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C
import Foreign.C.Enum
import Data.Int

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.Types

import Graphics.Cairo.Surfaces.CairoSurfaceT.Internal

#include <gdk/gdk.h>

gdkCursorNewFromSurface :: GdkDisplay -> CairoSurfaceT s ps -> #{type gdouble} -> #{type gdouble} -> IO GdkCursor
gdkCursorNewFromSurface (GdkDisplay d) (CairoSurfaceT fs) x y = withForeignPtr fs \s ->
	mkGdkCursor =<< c_gdk_cursor_new_from_surface d s x y

foreign import ccall "gdk_cursor_new_from_surface" c_gdk_cursor_new_from_surface ::
	Ptr GdkDisplay -> Ptr (CairoSurfaceT s ps) -> #{type gdouble} -> #{type gdouble} -> IO (Ptr GdkCursor)

gdkCursorNewFromName :: GdkDisplay -> String -> IO GdkCursor
gdkCursorNewFromName (GdkDisplay d) nm = withCString nm \cnm ->
	mkGdkCursor =<< c_gdk_cursor_new_from_name d cnm

foreign import ccall "gdk_cursor_new_from_name" c_gdk_cursor_new_from_name ::
	Ptr GdkDisplay -> CString -> IO (Ptr GdkCursor)

enum "GdkCursorType" ''#{type GdkCursorType} [''Show] [
	("GdkXCursor", #{const GDK_X_CURSOR}),
	("GdkArrow", #{const GDK_ARROW}),
	("GdkBasedArrowDown", #{const GDK_BASED_ARROW_DOWN}),
	("GdkBasedArrowUp", #{const GDK_BASED_ARROW_UP}),
	("GdkBoat", #{const GDK_BOAT}),
	("GdkBogosity", #{const GDK_BOGOSITY}),
	("GdkBottomLeftCorner", #{const GDK_BOTTOM_LEFT_CORNER}),
	("GdkBottomRightCorner", #{const GDK_BOTTOM_RIGHT_CORNER}),
	("GdkBottomSide", #{const GDK_BOTTOM_SIDE}),
	("GdkBottomTee", #{const GDK_BOTTOM_TEE}),
	("GdkBoxSpiral", #{const GDK_BOX_SPIRAL}),
	("GdkCenterPtr", #{const GDK_CENTER_PTR}),
	("GdkCircle", #{const GDK_CIRCLE}),
	("GdkClock", #{const GDK_CLOCK}),
	("GdkCoffeeMug", #{const GDK_COFFEE_MUG}),
	("GdkCross", #{const GDK_CROSS}),
	("GdkCrossReverse", #{const GDK_CROSS_REVERSE}),
	("GdkCrosshair", #{const GDK_CROSSHAIR}),
	("GdkDiamondCross", #{const GDK_DIAMOND_CROSS}),
	("GdkDot", #{const GDK_DOT}),
	("GdkDotbox", #{const GDK_DOTBOX}),
	("GdkDoubleArrow", #{const GDK_DOUBLE_ARROW}),
	("GdkDraftLarge", #{const GDK_DRAFT_LARGE}),
	("GdkDraftSmall", #{const GDK_DRAFT_SMALL}),
	("GdkDrapedBox", #{const GDK_DRAPED_BOX}),
	("GdkExchange", #{const GDK_EXCHANGE}),
	("GdkFleur", #{const GDK_FLEUR}),
	("GdkGobbler", #{const GDK_GOBBLER}),
	("GdkGumby", #{const GDK_GUMBY}),
	("GdkHand1", #{const GDK_HAND1}),
	("GdkHand2", #{const GDK_HAND2}),
	("GdkHeart", #{const GDK_HEART}),
	("GdkIcon", #{const GDK_ICON}),
	("GdkIronCross", #{const GDK_IRON_CROSS}),
	("GdkLeftPtr", #{const GDK_LEFT_PTR}),
	("GdkLeftSide", #{const GDK_LEFT_SIDE})
	]

gdkCursorNewForDisplay :: GdkDisplay -> GdkCursorType -> IO GdkCursor
gdkCursorNewForDisplay d (GdkCursorType t) =
	mkGdkCursor =<< c_gdk_cursor_new_for_display d t

foreign import ccall "gdk_cursor_new_for_display"
	c_gdk_cursor_new_for_display ::
	GdkDisplay -> #{type GdkCursorType} -> IO (Ptr GdkCursor)

gdkCursorGetDisplay :: GdkCursor -> IO GdkDisplay
gdkCursorGetDisplay (GdkCursor fc) = withForeignPtr fc \c ->
	GdkDisplay <$> c_gdk_cursor_get_display c

foreign import ccall "gdk_cursor_get_display" c_gdk_cursor_get_display ::
	Ptr GdkCursor -> IO (Ptr GdkDisplay)

gdkCursorGetSurface :: GdkCursor -> IO (CairoSurfaceT s ps)
gdkCursorGetSurface (GdkCursor fc) = withForeignPtr fc \c ->
	mkCairoSurfaceT =<< c_gdk_cursor_get_surface c

foreign import ccall "gdk_cursor_get_surface" c_gdk_cursor_get_surface ::
	Ptr GdkCursor -> IO (Ptr (CairoSurfaceT s ps))

gdkCursorGetCursorType :: GdkCursor -> IO GdkCursorType
gdkCursorGetCursorType (GdkCursor fc) = withForeignPtr fc \c ->
	GdkCursorType <$> c_gdk_cursor_get_cursor_type c

foreign import ccall "gdk_cursor_get_cursor_type" c_gdk_cursor_get_cursor_type ::
	Ptr GdkCursor -> IO #{type GdkCursorType}
