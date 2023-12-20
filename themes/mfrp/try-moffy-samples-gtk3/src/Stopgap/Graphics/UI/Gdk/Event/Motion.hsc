{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gdk.Event.Motion where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word
import Data.Int

import Stopgap.Graphics.UI.Gdk.Event

#include <gtk/gtk.h>

data GdkWindowTag
data GdkDeviceTag

type PtrGdkWindow = Ptr GdkWindowTag
type PtrGdkDevice = Ptr GdkDeviceTag
type PtrGdouble = Ptr #{type gdouble}

struct "M" #{size GdkEventMotion} #{alignment GdkEventMotion}
	[	("type", ''Type,
			[| #{peek GdkEventMotion, type} |],
			[| #{poke GdkEventMotion, type} |]),
		("window", ''PtrGdkWindow,
			[| #{peek GdkEventMotion, window} |],
			[| #{poke GdkEventMotion, window} |]),
		("send_event", ''#{type gint8},
			[| #{peek GdkEventMotion, send_event} |],
			[| #{poke GdkEventMotion, send_event} |]),
		("time", ''#{type guint32},
			[| #{peek GdkEventMotion, time} |],
			[| #{poke GdkEventMotion, time} |]),
		("x", ''#{type gdouble},
			[| #{peek GdkEventMotion, x} |],
			[| #{poke GdkEventMotion, x} |]),
		("y", ''#{type gdouble},
			[| #{peek GdkEventMotion, y} |],
			[| #{poke GdkEventMotion, y} |]),
		("axes", ''PtrGdouble,
			[| #{peek GdkEventMotion, axes} |],
			[| #{poke GdkEventMotion, axes} |]),
		("state", ''#{type GdkModifierType},
			[| #{peek GdkEventMotion, state} |],
			[| #{poke GdkEventMotion, state} |]),
		("is_hint", ''#{type gint16},
			[| #{peek GdkEventMotion, is_hint} |],
			[| #{poke GdkEventMotion, is_hint} |]),
		("device", ''PtrGdkDevice,
			[| #{peek GdkEventMotion, device} |],
			[| #{poke GdkEventMotion, device} |]),
		("x_root", ''#{type gdouble},
			[| #{peek GdkEventMotion, x_root} |],
			[| #{poke GdkEventMotion, x_root} |]),
		("y_root", ''#{type gdouble},
			[| #{peek GdkEventMotion, y_root} |],
			[| #{poke GdkEventMotion, y_root} |]) ]
	[''Show, ''Eq, ''Ord, ''Storable]
