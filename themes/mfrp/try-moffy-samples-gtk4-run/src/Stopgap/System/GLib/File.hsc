{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.System.GLib.File where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.C.String
import Control.Monad
import Control.Exception
import Data.Word
import Data.Int
import Stopgap.Data.Ptr

import Stopgap.System.GLib.Error qualified as G.Error
import Stopgap.System.GLib.Cancellable qualified as G.Cancellable

#include <gtk/gtk.h>

data FTag

newtype F = F (Ptr FTag) deriving Show

instance IsPtr F where type Tag F = FTag; fromPtr = F; toPtr (F p) = p

newForPath :: FilePath -> IO F
newForPath fp = F <$> withCString fp c_g_file_new_for_path

foreign import ccall "g_file_new_for_path" c_g_file_new_for_path ::
	CString -> IO (Ptr FTag)

loadContents :: (?makeEFuns :: [G.Error.MakeEFun]) =>
	F -> Maybe G.Cancellable.C -> IO (String, String)
loadContents (F f) (cancellableToPtr -> c) =
	alloca \pcnt -> alloca \plen -> alloca \petag -> alloca \pperr -> do
		rslt <- c_g_file_load_contents f c pcnt plen petag pperr
		when (rslt == #{const FALSE}) do
			perr <- peek pperr
			err <- G.Error.fromC =<< peek perr
			G.Error.free perr
			throw err
		cntlen <- (,) <$> peek pcnt <*> (fromIntegral <$> peek plen)
		cetag <- peek petag
		(,) <$> peekCStringLen cntlen <*> peekCString cetag

cancellableToPtr :: Maybe G.Cancellable.C -> Ptr G.Cancellable.CTag
cancellableToPtr = \case Nothing -> nullPtr; Just (G.Cancellable.C p) -> p

foreign import ccall "g_file_load_contents" c_g_file_load_contents ::
	Ptr FTag -> Ptr G.Cancellable.CTag -> Ptr CString ->
	Ptr #{type gsize} -> Ptr CString -> Ptr (Ptr G.Error.E_) ->
	IO #{type gboolean}

getBasename :: F -> IO String
getBasename (F f) = peekCString =<< c_g_file_get_basename f

foreign import ccall "g_file_get_basename" c_g_file_get_basename ::
	Ptr FTag -> IO CString
