{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.System.GLib.Application where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.C.Types
import Foreign.C.String
import Foreign.C.Enum
import Data.Word
import System.Exit

#include <gtk/gtk.h>

data ATag

data A = A (Ptr ATag) deriving Show

class IsA a where toA :: a -> A

instance IsA A where toA = id

run :: IsA a => a -> String -> [String] -> IO ExitCode
run (toA -> A pa) cmd as = toExitCode
	<$> withArgcArgv cmd as \argc argv -> c_g_application_run pa argc argv

withArgcArgv :: String -> [String] -> (CInt -> Ptr CString -> IO a) -> IO a
withArgcArgv cmd as f = withCStringList (cmd : as) \cas ->
	withArrayLen cas \argc argv -> f (fromIntegral argc) argv

withCStringList :: [String] -> ([CString] -> IO a) -> IO a
withCStringList [] f = f []
withCStringList (a : as) f =
	withCString a \ca -> withCStringList as \cas -> f $ ca : cas

toExitCode :: CInt -> ExitCode
toExitCode = \case 0 -> ExitSuccess; s -> ExitFailure $ fromIntegral s

foreign import ccall "g_application_run" c_g_application_run ::
	Ptr ATag -> CInt -> Ptr CString -> IO CInt

enum "Flags" ''#{type GApplicationFlags} [''Show, ''Read] [
	("DefaultFlags", #{const G_APPLICATION_DEFAULT_FLAGS}),
	("IsService", #{const G_APPLICATION_IS_SERVICE}),
	("IsLauncher", #{const G_APPLICATION_IS_LAUNCHER}),
	("HandlesOpen", #{const G_APPLICATION_HANDLES_OPEN}),
	("HandlesCommandLine", #{const G_APPLICATION_HANDLES_COMMAND_LINE}),
	("SendEnvironment", #{const G_APPLICATION_SEND_ENVIRONMENT}),
	("NonUnique", #{const G_APPLICATION_NON_UNIQUE}),
	("CanOverrideAppId", #{const G_APPLICATION_CAN_OVERRIDE_APP_ID}),
	("AllowReplacement", #{const G_APPLICATION_ALLOW_REPLACEMENT}),
	("Replace", #{const G_APPLICATION_REPLACE}) ]
