{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Values where

import Data.Word

#include <pango/pango.h>

pangoScale :: Num n => n
pangoScale = #const PANGO_SCALE

newtype PangoEllipsizeMode = PangoEllipsizeMode #{type PangoEllipsizeMode} deriving Show

#enum PangoEllipsizeMode, PangoEllipsizeMode, PANGO_ELLIPSIZE_NONE, \
	PANGO_ELLIPSIZE_START, PANGO_ELLIPSIZE_MIDDLE, PANGO_ELLIPSIZE_END

newtype PangoAlignment = PangoAlignment #{type PangoAlignment} deriving Show

#enum PangoAlignment, PangoAlignment, PANGO_ALIGN_LEFT, PANGO_ALIGN_CENTER, \
	PANGO_ALIGN_RIGHT

newtype PangoTabAlign = PangoTabAlign #{type PangoTabAlign} deriving Show

#enum PangoTabAlign, PangoTabAlign, PANGO_TAB_LEFT
