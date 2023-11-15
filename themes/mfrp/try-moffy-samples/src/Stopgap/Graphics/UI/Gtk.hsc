{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gtk where

import Foreign.C.Enum
import Foreign.C.Types
import Data.Word

#include <gtk/gtk.h>

newtype Pixel = Pixel CInt deriving (Show, Num)

enum "Orientation" ''#{type GtkOrientation} [''Show, ''Read] [
	("OrientationHorizontal", #{const GTK_ORIENTATION_HORIZONTAL}),
	("OrientationVertical", #{const GTK_ORIENTATION_VERTICAL}) ]
