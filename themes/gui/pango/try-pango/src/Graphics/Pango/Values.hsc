{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Values where

import Data.Word

#include <pango/pango.h>

pangoScale :: Num n => n
pangoScale = #const PANGO_SCALE

newtype PangoStyle = PangoStyle #{type PangoStyle} deriving Show

#enum PangoStyle, PangoStyle, PANGO_STYLE_NORMAL, PANGO_STYLE_OBLIQUE, \
	PANGO_STYLE_ITALIC
