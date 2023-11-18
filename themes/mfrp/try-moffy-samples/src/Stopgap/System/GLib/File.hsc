{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.System.GLib.File where

import Foreign.Ptr
import Foreign.C.String
import Stopgap.Data.Ptr

import Stopgap.System.GLib.Cancellable qualified as G.Cancellable

data FTag

newtype F = F (Ptr FTag) deriving Show

instance IsPtr F where type Tag F = FTag; fromPtr = F; toPtr (F p) = p

newForPath :: FilePath -> IO F
newForPath fp = F <$> withCString fp c_g_file_new_for_path

foreign import ccall "g_file_new_for_path" c_g_file_new_for_path ::
	CString -> IO (Ptr FTag)

{-
foreign import ccall "g_file_load_contents" c_g_file_load_contents ::
	Ptr FTag -> Ptr G.Cancellable.CTag -> Ptr CString ->
	Ptr #{type gsize} -> Ptr CString ->
	-}
