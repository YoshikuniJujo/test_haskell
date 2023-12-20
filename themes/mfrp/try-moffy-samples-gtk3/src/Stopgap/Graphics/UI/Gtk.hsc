{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gtk where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String

init :: String -> [String] -> IO ()
init cmd as = withArgcArgv cmd as c_gtk_init

withArgcArgv :: String -> [String] -> (Ptr CInt -> Ptr (Ptr CString) -> IO a) -> IO a
withArgcArgv cmd as f = withCStringList (cmd : as) \cas ->
	withArrayLen cas \argc argv ->
		alloca \pargc -> alloca \pargv -> do
			poke pargc (fromIntegral argc)
			poke pargv argv
			f pargc pargv

withCStringList :: [String] -> ([CString] -> IO a) -> IO a
withCStringList [] f = f []
withCStringList (a : as) f =
	withCString a \ca -> withCStringList as \cas -> f $ ca : cas

foreign import ccall "gtk_init" c_gtk_init :: Ptr CInt -> Ptr (Ptr CString) -> IO ()

main :: IO ()
main = c_gtk_main

foreign import ccall "gtk_main" c_gtk_main :: IO ()

mainQuit :: IO ()
mainQuit = c_gtk_main_quit

foreign import ccall "gtk_main_quit" c_gtk_main_quit :: IO ()
