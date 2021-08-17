{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Cursors.Internal (
	-- * TYPE
	GdkCursor(..),

	-- * FUNCTION
	gdkCursorGetDisplay,
	gdkCursorNewFromSurface,
	gdkCursorNewFromName,

	-- * GDK CURSOR TYPE
	GdkCursorType(..), GdkNoCursorType(..),
	gdkCursorNewForDisplay, gdkCursorGetCursorType,
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
	pattern GdkLeftTee, pattern GdkLeftbutton,
	pattern GdkLlAngle, pattern GdkLrAngle,
	pattern GdkMan, pattern GdkMiddlebutton,
	pattern GdkMouse, pattern GdkPencil,
	pattern GdkPirate, pattern GdkPlus,
	pattern GdkQuestionArrow, pattern GdkRightPtr,
	pattern GdkRightSide, pattern GdkRightTee,
	pattern GdkRightbutton, pattern GdkRtlLogo,
	pattern GdkSailboat, pattern GdkSbDownArrow,
	pattern GdkSbHDoubleArrow, pattern GdkSbLeftArrow,
	pattern GdkSbRightArrow, pattern GdkSbUpArrow,
	pattern GdkSbVDoubleArrow, pattern GdkShuttle,
	pattern GdkSizing, pattern GdkSpider,
	pattern GdkSpraycan, pattern GdkStar,
	pattern GdkTarget, pattern GdkTcross,
	pattern GdkTopLeftArrow, pattern GdkTopLeftCorner,
	pattern GdkTopRightCorner, pattern GdkTopSide,
	pattern GdkTopTee, pattern GdkTrek,
	pattern GdkUlAngle, pattern GdkUmbrella,
	pattern GdkUrAngle, pattern GdkWatch,
	pattern GdkXterm, pattern GdkLastCursor,
	pattern GdkBlankCursor, pattern GdkCursorIsPixmap

	) where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.C
import Foreign.C.Enum
import Data.Int
import System.IO.Unsafe

import {-# SOURCE #-} Graphics.Gdk.GdkDisplay.Internal

import Graphics.Cairo.Surfaces.CairoSurfaceT.Internal
import Graphics.Cairo.Surfaces.ImageSurfaces

#include <gdk/gdk.h>

newtype GdkCursor = GdkCursor (ForeignPtr GdkCursor) deriving Show

mkGdkCursor :: Ptr GdkCursor -> IO GdkCursor
mkGdkCursor p = GdkCursor <$> newForeignPtr p (c_g_object_unref p)

mkGdkCursor' :: IO () -> Ptr GdkCursor -> IO GdkCursor
mkGdkCursor' f p = GdkCursor <$> newForeignPtr p (f >> c_g_object_unref p)

foreign import ccall "g_object_unref" c_g_object_unref :: Ptr a -> IO ()

gdkCursorGetDisplay :: GdkCursor -> GdkDisplay
gdkCursorGetDisplay (GdkCursor fc) = unsafePerformIO
	$ withForeignPtr fc \c -> GdkDisplay <$> c_gdk_cursor_get_display c

foreign import ccall "gdk_cursor_get_display" c_gdk_cursor_get_display ::
	Ptr GdkCursor -> IO (Ptr GdkDisplay)

gdkCursorNewFromSurface ::
	GdkDisplay -> CairoSurfaceImageT s ps -> CDouble -> CDouble -> IO GdkCursor
gdkCursorNewFromSurface (GdkDisplay d) (toCairoSurfaceT -> CairoSurfaceT fs) x y =
	withForeignPtr fs \s -> do
		c_cairo_surface_reference s
		mkGdkCursor' (c_cairo_surface_destroy s) =<< c_gdk_cursor_new_from_surface d s x y

foreign import ccall "cairo_surface_reference" c_cairo_surface_reference ::
	Ptr (CairoSurfaceT s ps) -> IO ()

-- foreign import ccall "cairo_surface_destroy" c_cairo_surface_destroy ::
--	Ptr (CairoSurfaceT s ps) -> IO ()

foreign import ccall "gdk_cursor_new_from_surface"
	c_gdk_cursor_new_from_surface ::
	Ptr GdkDisplay -> Ptr (CairoSurfaceT s ps) -> CDouble -> CDouble ->
	IO (Ptr GdkCursor)

gdkCursorNewFromName :: GdkDisplay -> String -> IO GdkCursor
gdkCursorNewFromName (GdkDisplay d) nm = withCString nm \cnm ->
	mkGdkCursor =<< c_gdk_cursor_new_from_name d cnm

foreign import ccall "gdk_cursor_new_from_name" c_gdk_cursor_new_from_name ::
	Ptr GdkDisplay -> CString -> IO (Ptr GdkCursor)

enum "GdkCursorType" ''#{type GdkCursorType} [''Show, ''Read, ''Eq] [
	("GdkXCursor", #{const GDK_X_CURSOR}), ("GdkArrow", #{const GDK_ARROW}),
	("GdkBasedArrowDown", #{const GDK_BASED_ARROW_DOWN}),
	("GdkBasedArrowUp", #{const GDK_BASED_ARROW_UP}),
	("GdkBoat", #{const GDK_BOAT}), ("GdkBogosity", #{const GDK_BOGOSITY}),
	("GdkBottomLeftCorner", #{const GDK_BOTTOM_LEFT_CORNER}),
	("GdkBottomRightCorner", #{const GDK_BOTTOM_RIGHT_CORNER}),
	("GdkBottomSide", #{const GDK_BOTTOM_SIDE}),
	("GdkBottomTee", #{const GDK_BOTTOM_TEE}),
	("GdkBoxSpiral", #{const GDK_BOX_SPIRAL}),
	("GdkCenterPtr", #{const GDK_CENTER_PTR}),
	("GdkCircle", #{const GDK_CIRCLE}), ("GdkClock", #{const GDK_CLOCK}),
	("GdkCoffeeMug", #{const GDK_COFFEE_MUG}),
	("GdkCross", #{const GDK_CROSS}),
	("GdkCrossReverse", #{const GDK_CROSS_REVERSE}),
	("GdkCrosshair", #{const GDK_CROSSHAIR}),
	("GdkDiamondCross", #{const GDK_DIAMOND_CROSS}),
	("GdkDot", #{const GDK_DOT}), ("GdkDotbox", #{const GDK_DOTBOX}),
	("GdkDoubleArrow", #{const GDK_DOUBLE_ARROW}),
	("GdkDraftLarge", #{const GDK_DRAFT_LARGE}),
	("GdkDraftSmall", #{const GDK_DRAFT_SMALL}),
	("GdkDrapedBox", #{const GDK_DRAPED_BOX}),
	("GdkExchange", #{const GDK_EXCHANGE}),
	("GdkFleur", #{const GDK_FLEUR}), ("GdkGobbler", #{const GDK_GOBBLER}),
	("GdkGumby", #{const GDK_GUMBY}), ("GdkHand1", #{const GDK_HAND1}),
	("GdkHand2", #{const GDK_HAND2}), ("GdkHeart", #{const GDK_HEART}),
	("GdkIcon", #{const GDK_ICON}),
	("GdkIronCross", #{const GDK_IRON_CROSS}),
	("GdkLeftPtr", #{const GDK_LEFT_PTR}),
	("GdkLeftSide", #{const GDK_LEFT_SIDE}),
	("GdkLeftTee", #{const GDK_LEFT_TEE}),
	("GdkLeftbutton", #{const GDK_LEFTBUTTON}),
	("GdkLlAngle", #{const GDK_LL_ANGLE}),
	("GdkLrAngle", #{const GDK_LR_ANGLE}), ("GdkMan", #{const GDK_MAN}),
	("GdkMiddlebutton", #{const GDK_MIDDLEBUTTON}),
	("GdkMouse", #{const GDK_MOUSE}), ("GdkPencil", #{const GDK_PENCIL}),
	("GdkPirate", #{const GDK_PIRATE}), ("GdkPlus", #{const GDK_PLUS}),
	("GdkQuestionArrow", #{const GDK_QUESTION_ARROW}),
	("GdkRightPtr", #{const GDK_RIGHT_PTR}),
	("GdkRightSide", #{const GDK_RIGHT_SIDE}),
	("GdkRightTee", #{const GDK_RIGHT_TEE}),
	("GdkRightbutton", #{const GDK_RIGHTBUTTON}),
	("GdkRtlLogo", #{const GDK_RTL_LOGO}),
	("GdkSailboat", #{const GDK_SAILBOAT}),
	("GdkSbDownArrow", #{const GDK_SB_DOWN_ARROW}),
	("GdkSbHDoubleArrow", #{const GDK_SB_H_DOUBLE_ARROW}),
	("GdkSbLeftArrow", #{const GDK_SB_LEFT_ARROW}),
	("GdkSbRightArrow", #{const GDK_SB_RIGHT_ARROW}),
	("GdkSbUpArrow", #{const GDK_SB_UP_ARROW}),
	("GdkSbVDoubleArrow", #{const GDK_SB_V_DOUBLE_ARROW}),
	("GdkShuttle", #{const GDK_SHUTTLE}),
	("GdkSizing", #{const GDK_SIZING}), ("GdkSpider", #{const GDK_SPIDER}),
	("GdkSpraycan", #{const GDK_SPRAYCAN}), ("GdkStar", #{const GDK_STAR}),
	("GdkTarget", #{const GDK_TARGET}), ("GdkTcross", #{const GDK_TCROSS}),
	("GdkTopLeftArrow", #{const GDK_TOP_LEFT_ARROW}),
	("GdkTopLeftCorner", #{const GDK_TOP_LEFT_CORNER}),
	("GdkTopRightCorner", #{const GDK_TOP_RIGHT_CORNER}),
	("GdkTopSide", #{const GDK_TOP_SIDE}),
	("GdkTopTee", #{const GDK_TOP_TEE}), ("GdkTrek", #{const GDK_TREK}),
	("GdkUlAngle", #{const GDK_UL_ANGLE}),
	("GdkUmbrella", #{const GDK_UMBRELLA}),
	("GdkUrAngle", #{const GDK_UR_ANGLE}), ("GdkWatch", #{const GDK_WATCH}),
	("GdkXterm", #{const GDK_XTERM}),
	("GdkBlankCursor", #{const GDK_BLANK_CURSOR}) ]

enum "GdkNoCursorType" ''#{type GdkCursorType} [''Show] [
	("GdkLastCursor", #{const GDK_LAST_CURSOR}),
	("GdkCursorIsPixmap", #{const GDK_CURSOR_IS_PIXMAP}) ]

gdkCursorNewForDisplay :: GdkDisplay -> GdkCursorType -> IO GdkCursor
gdkCursorNewForDisplay d (GdkCursorType t) =
	mkGdkCursor =<< c_gdk_cursor_new_for_display d t

foreign import ccall "gdk_cursor_new_for_display"
	c_gdk_cursor_new_for_display ::
	GdkDisplay -> #{type GdkCursorType} -> IO (Ptr GdkCursor)

gdkCursorGetCursorType :: GdkCursor -> IO (Either GdkNoCursorType GdkCursorType)
gdkCursorGetCursorType (GdkCursor fc) = withForeignPtr fc \c ->
	(<$> c_gdk_cursor_get_cursor_type c) \case
		t@(#{const GDK_LAST_CURSOR}) -> Left $ GdkNoCursorType t
		t@(#{const GDK_CURSOR_IS_PIXMAP}) -> Left $ GdkNoCursorType t
		t -> Right $ GdkCursorType t

foreign import ccall "gdk_cursor_get_cursor_type" c_gdk_cursor_get_cursor_type ::
	Ptr GdkCursor -> IO #{type GdkCursorType}
