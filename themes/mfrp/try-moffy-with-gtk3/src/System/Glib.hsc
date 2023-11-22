{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module System.Glib (
	SourceId, gTimeoutAdd) where

#include <glib.h>

import Foreign.Ptr
import Foreign.Tools
import Data.Word
import Data.Int

newtype SourceId = SourceId #{type guint} deriving Show

gTimeoutAdd :: AsPointer a => #{type guint} -> (a -> IO Bool) -> a -> IO SourceId
gTimeoutAdd t f x = do
	fp <- g_callback_timeout \x' -> (boolToGBoolean <$>) . f =<< asValue x'
	SourceId <$> asPointer x (c_g_timeout_add t fp)

foreign import ccall "g_timeout_add" c_g_timeout_add ::
	#{type guint} -> FunPtr (Ptr a -> IO #{type gboolean}) -> Ptr a -> IO #type guint
foreign import ccall "wrapper" g_callback_timeout ::
	(Ptr a -> IO #{type gboolean}) -> IO (FunPtr (Ptr a -> IO #{type gboolean}))

boolToGBoolean :: Bool -> #type gboolean
boolToGBoolean False = #const FALSE
boolToGBoolean True = #const TRUE
