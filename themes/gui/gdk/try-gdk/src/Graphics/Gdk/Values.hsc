{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Values where

import Foreign.C.Enum

import Data.Word

#include <gdk/gdk.h>

newtype GdkGrabStatus = GdkGrabStatus #{type GdkGrabStatus} deriving Show

#enum GdkGrabStatus, GdkGrabStatus, GDK_GRAB_SUCCESS, \
	GDK_GRAB_ALREADY_GRABBED, GDK_GRAB_INVALID_TIME, GDK_GRAB_NOT_VIEWABLE, \
	GDK_GRAB_FROZEN, GDK_GRAB_FAILED
