{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gdk.Event where

import Foreign.Storable
import Foreign.C.Enum
import Data.Bits
import Data.Word
import Data.Int

#include <gtk/gtk.h>

enum "Mask" ''#{type GdkEventMask} [''Show, ''Read, ''Eq, ''Bits] [
	("ExposureMask", #{const GDK_EXPOSURE_MASK}),
	("PointerMotionMask", #{const GDK_POINTER_MOTION_MASK}),
	("PointerMotionHintMask", #{const GDK_POINTER_MOTION_HINT_MASK}),
	("ButtonMotionMask", #{const GDK_BUTTON_MOTION_MASK}),
	("Button1MotionMask", #{const GDK_BUTTON1_MOTION_MASK}),
	("Button2MotionMask", #{const GDK_BUTTON2_MOTION_MASK}),
	("Button3MotionMask", #{const GDK_BUTTON3_MOTION_MASK}),
	("ButtonPressMask", #{const GDK_BUTTON_PRESS_MASK}),
	("ButtonReleaseMask", #{const GDK_BUTTON_RELEASE_MASK}),
	("KeyPressMask", #{const GDK_KEY_PRESS_MASK}),
	("KeyReleaseMask", #{const GDK_KEY_RELEASE_MASK}),
	("EnterNotifyMask", #{const GDK_ENTER_NOTIFY_MASK}),
	("LeaveNotifyMask", #{const GDK_LEAVE_NOTIFY_MASK})
	]

enum "Type" ''#{type GdkEventType} [''Show, ''Read, ''Eq, ''Ord, ''Storable] [
	("Delete", #{const GDK_DELETE}),
	("MotionNotify", #{const GDK_MOTION_NOTIFY}),
	("ButtonPress", #{const GDK_BUTTON_PRESS}),
	("ButtonRelease", #{const GDK_BUTTON_RELEASE})
	]
