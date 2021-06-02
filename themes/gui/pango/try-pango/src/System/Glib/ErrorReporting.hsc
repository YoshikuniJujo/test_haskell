{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module System.Glib.ErrorReporting (
	GError(..), mkGError, gErrorReport
	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String

import System.Glib.Quarks

#include <glib.h>

data GError = GError {
	gErrorDomain :: GQuark, gErrorCode :: CInt, gErrorMessage :: String }
	deriving Show

mkGError :: Ptr GError -> IO GError
mkGError p = do
	ge <- GError
		<$> #{peek GError, domain} p
		<*> #{peek GError, code} p
		<*> (peekCString =<< #{peek GError, message} p)
	c_g_error_free p
	pure ge

foreign import ccall "g_error_free" c_g_error_free :: Ptr GError -> IO ()

gErrorReport :: GError -> String
gErrorReport (GError d c m) = gQuarkToString d ++ ": " ++ show c ++ ": " ++ m
