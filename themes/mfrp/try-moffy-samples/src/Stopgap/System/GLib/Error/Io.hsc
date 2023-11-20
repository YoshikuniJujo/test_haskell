{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.System.GLib.Error.Io where

import Foreign.C.Enum
import Control.Exception
import Control.Exception.Hierarchy
import Data.Word

import Stopgap.System.GLib.Error qualified as G.Error
import Stopgap.System.GLib.Error qualified

#include <gtk/gtk.h>

enum "I_" ''#{type GFileError} [''Show, ''Read] [
	("IFailed", #{const G_IO_ERROR_FAILED}),
	("INotFound", #{const G_IO_ERROR_NOT_FOUND}),

	("IPermissionDenied", #{const G_IO_ERROR_PERMISSION_DENIED})
	]

data I = I { iCode :: I_, iMessage :: String } deriving Show

exceptionHierarchy (Just ''G.Error.E) $ ExType ''I

foreign import capi "gtk/gtk.h value G_IO_ERROR" c_G_IO_ERROR :: #{type GQuark}

mkEFun :: G.Error.DomainCode -> String -> IO (Maybe G.Error.E)
mkEFun (d, c) msg = do
	let	d0 = c_G_IO_ERROR
	pure if d == d0
	then Just . G.Error.E $ I (I_ $ fromIntegral c) msg
	else Nothing
