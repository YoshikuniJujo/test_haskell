{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Values where

import Data.Bits
import Data.Word

#include <gdk/gdk.h>

newtype GdkWindowAttributesType = GdkWindowAttributesType #{type GdkWindowAttributesType} deriving Show
#enum GdkWindowAttributesType, GdkWindowAttributesType, \
	GDK_WA_TITLE, GDK_WA_X, GDK_WA_Y, GDK_WA_CURSOR, GDK_WA_VISUAL, \
	GDK_WA_WMCLASS, GDK_WA_NOREDIR, GDK_WA_TYPE_HINT

newtype GdkEventMask = GdkEventMask #{type GdkEventMask} deriving Show

#enum GdkEventMask, GdkEventMask, GDK_EXPOSURE_MASK, \
	GDK_BUTTON_PRESS_MASK

mergeGdkEventMask :: [GdkEventMask] -> #{type GdkEventMask}
mergeGdkEventMask [] = 0
mergeGdkEventMask (GdkEventMask em : ems) = em .|. mergeGdkEventMask ems

newtype GdkWindowType = GdkWindowType #{type GdkWindowType} deriving Show
#enum GdkWindowType, GdkWindowType, GDK_WINDOW_ROOT, GDK_WINDOW_TOPLEVEL, \
	GDK_WINDOW_CHILD

newtype GdkWindowWindowClass = GdkWindowWindowClass #{type GdkWindowWindowClass} deriving Show
#enum GdkWindowWindowClass, GdkWindowWindowClass, GDK_INPUT_OUTPUT, GDK_INPUT_ONLY
