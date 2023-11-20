{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.System.GLib.Error where

import Control.Exception
import Control.Exception.Hierarchy
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Enum
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

foreign import capi "gtk/gtk.h value G_IO_ERROR" c_G_IO_ERROR :: #{type GQuark}

fromC :: [DomainCode -> String -> IO (Maybe E)] ->  E_ -> IO E
fromC fs0 E_ { e_Domain_ = d, e_Code_ = c, e_Message_ = cm } = do
	print c_G_IO_ERROR
	get fs0 =<< peekCString (castPtr cm)
	where
	get = \case
		[] -> pure . E . EOthers (d, c)
		f : fs -> \msg -> maybe (get fs msg) pure =<< f (d, c) msg

enum "I_" ''#{type GFileError} [''Show, ''Read] [
	("IFailed", #{const G_IO_ERROR_FAILED}),
	("INotFound", #{const G_IO_ERROR_NOT_FOUND}),

	("IPermissionDenied", #{const G_IO_ERROR_PERMISSION_DENIED})
	]

data I = I { iCode :: I_, iMessage :: String } deriving Show

exceptionHierarchy (Just ''E) $ ExType ''I

fFromC :: DomainCode -> String -> IO (Maybe E)
fFromC (d, c) msg = do
	let	d0 = c_G_IO_ERROR
	pure if d == d0 then Just . E $ I (I_ $ fromIntegral c) msg else Nothing
