{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gdk.Event.Button where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word
import Data.Int

#include <gtk/gtk.h>

data GdkWindowTag
data GdkDeviceTag

type PtrGdkWindow = Ptr GdkWindowTag
type PtrGdkDevice = Ptr GdkDeviceTag
type PtrGdouble = Ptr #{type gdouble}

struct "B" #{size GdkEventButton} #{alignment GdkEventButton}
	[	("type", ''#{type GdkEventType},
			[| #{peek GdkEventButton, type} |],
			[| #{poke GdkEventButton, type} |]),
		("window", ''PtrGdkWindow,
			[| #{peek GdkEventButton, window} |],
			[| #{poke GdkEventButton, window} |]),
		("send_event", ''#{type gint8},
			[| #{peek GdkEventButton, send_event} |],
			[| #{poke GdkEventButton, send_event} |]),
		("time", ''#{type guint32},
			[| #{peek GdkEventButton, time} |],
			[| #{poke GdkEventButton, time} |]),
		("x", ''#{type gdouble},
			[| #{peek GdkEventButton, x} |],
			[| #{poke GdkEventButton, x} |]),
		("y", ''#{type gdouble},
			[| #{peek GdkEventButton, y} |],
			[| #{poke GdkEventButton, y} |]),
		("axes", ''PtrGdouble,
			[| #{peek GdkEventButton, axes} |],
			[| #{poke GdkEventButton, axes} |]),
		("state", ''#{type GdkModifierType},
			[| #{peek GdkEventButton, state} |],
			[| #{poke GdkEventButton, state} |]),
		("button", ''#{type guint},
			[| #{peek GdkEventButton, button} |],
			[| #{poke GdkEventButton, button} |]),
		("device", ''PtrGdkDevice,
			[| #{peek GdkEventButton, device} |],
			[| #{poke GdkEventButton, device} |]),
		("x_root", ''#{type gdouble},
			[| #{peek GdkEventButton, x_root} |],
			[| #{poke GdkEventButton, x_root} |]),
		("y_root", ''#{type gdouble},
			[| #{peek GdkEventButton, y_root} |],
			[| #{poke GdkEventButton, y_root} |]) ]
	[''Show, ''Eq, ''Ord, ''Storable]
