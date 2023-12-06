{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.System.GLib.Object where

import Foreign.Ptr
import Foreign.C.String
import Data.Bool
import Data.Word
import Data.Int
import Stopgap.Data.Ptr

#include <glib.h>

data OTag

newtype O = O (Ptr OTag) deriving Show

class IsPtr o => IsO o where toO :: o -> O

withObject :: IsO o => IO o -> (o -> IO a) -> IO a
withObject cr f = do
	o <- cr
	f o <* unref o

unref :: IsO o => o -> IO ()
unref (toO -> O o) = c_g_object_unref o

foreign import ccall "g_object_unref" c_g_object_unref :: Ptr OTag -> IO ()

class Property p where toP :: p -> Either #{type guint64} #{type gint64}

instance Property Bool where toP = Left . bool #{const FALSE} #{const TRUE}

set :: (IsO o, Property p) => o -> String -> p -> IO ()
set (toO -> (O o)) n p = withCString n \cn -> either
	(\pp -> c_g_object_set_guint64 o (castPtr cn) pp nullPtr)
	(\pp -> c_g_object_set_gint64 o (castPtr cn) pp nullPtr) (toP p)

foreign import ccall "g_object_set" c_g_object_set_guint64 ::
	Ptr OTag -> Ptr #{type gchar} -> #{type guint64} -> Ptr () -> IO ()

foreign import ccall "g_object_set" c_g_object_set_gint64 ::
	Ptr OTag -> Ptr #{type gchar} -> #{type gint64} -> Ptr () -> IO ()
