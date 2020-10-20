{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Values where

import Data.Word

#include <gdk/gdk.h>

newtype GdkWindowAttributesType = GdkWindowAttributesType #{type GdkWindowAttributesType} deriving Show
#enum GdkWindowAttributesType, GdkWindowAttributesType, \
	GDK_WA_TITLE, GDK_WA_X, GDK_WA_Y, GDK_WA_CURSOR, GDK_WA_VISUAL, \
	GDK_WA_WMCLASS, GDK_WA_NOREDIR, GDK_WA_TYPE_HINT
