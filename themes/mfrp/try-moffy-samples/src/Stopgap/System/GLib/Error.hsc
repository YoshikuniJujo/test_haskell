{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.System.GLib.Error where

import Foreign.Ptr
import Foreign.C.Struct
import Foreign.Storable
import Data.Word
import Data.Int

#include <gtk/gtk.h>

type PGChar = Ptr #{type gchar}

struct "E" #{size GError} #{alignment GError} [
	("domain", ''#{type GQuark},
		[| #{peek GError, domain} |], [| #{poke GError, domain} |]),
	("code", ''#{type gint},
		[| #{peek GError, code} |], [| #{poke GError, code} |]),
	("message", ''PGChar,
		[| #{peek GError, message} |], [| #{poke GError, message} |])
	]
	[''Show]
