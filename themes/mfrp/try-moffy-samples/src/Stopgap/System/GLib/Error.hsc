{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.System.GLib.Error where

import Control.Exception.Hierarchy
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Struct
import Foreign.Storable
import Data.Word
import Data.Int

#include <gtk/gtk.h>

type PGChar = Ptr #{type gchar}

struct "E_" #{size GError} #{alignment GError} [
	("domain_", ''#{type GQuark},
		[| #{peek GError, domain} |], [| #{poke GError, domain} |]),
	("code_", ''#{type gint},
		[| #{peek GError, code} |], [| #{poke GError, code} |]),
	("message_", ''PGChar,
		[| #{peek GError, message} |], [| #{poke GError, message} |])
	]
	[''Show, ''Storable]

{-
data E = E E_ deriving Show

fromC :: E_ -> E
fromC = E
-}

data E = E {
	domain :: #{type GQuark},
	code :: #{type gint},
	message :: String }
	deriving Show

fromC :: E_ -> IO E
fromC E_ { e_Domain_ = d, e_Code_ = c, e_Message_ = cm } =
	E d c <$> peekCString (castPtr cm)

exceptionHierarchy Nothing $ ExType ''E
