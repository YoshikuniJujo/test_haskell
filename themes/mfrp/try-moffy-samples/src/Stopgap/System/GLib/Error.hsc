{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.System.GLib.Error where

import Control.Exception
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

type DomainCode = (#{type GQuark}, #{type gint})

data EOthers = EOthers {
	domainCode :: DomainCode,
	message :: String }
	deriving Show

exceptionHierarchy Nothing $ ExNode "E" [
	ExType ''EOthers ]

fromC :: (?makeEFuns :: [DomainCode -> String -> IO (Maybe E)]) =>  E_ -> IO E
fromC E_ { e_Domain_ = d, e_Code_ = c, e_Message_ = cm } = do
	get ?makeEFuns =<< peekCString (castPtr cm)
	where
	get = \case
		[] -> pure . E . EOthers (d, c)
		f : fs -> \msg -> maybe (get fs msg) pure =<< f (d, c) msg
