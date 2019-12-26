{-# LANGUAGE OverloadedStrings, OverloadedLabels, BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

#include <gdk/gdk.h>

module Main where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable

import Data.Bool
import Data.Word
import Numeric

import qualified GI.Gtk as G
import Data.GI.Base
import GI.Gdk.Structs.EventKey
-- import GI.Gtk.Objects.EventControllerKey

main :: IO ()
main = do
	_ <- G.init Nothing
	win <- new G.Window [ ##title := "Hi there" ]
	_ <- on win ##destroy G.mainQuit
	_ <- on win ##keyPressEvent \(EventKey k) -> True <$ do
		kv <- foo $ managedForeignPtr k
		putStrLn $ "key pressed: 0x" ++ showHex kv ""
		bool (return ()) G.mainQuit $ kv == 0x71

{-
	eck <- eventControllerKeyNew win
	onEventControllerKeyKeyPressed eck \kv kc md -> True <$ print (kv, kc, md)
--	on eck \kv kc md -> True <$ print (kv, kc, md)
	-}

--	##add win =<< G.eventControllerGetWidget eck
	##showAll win
	G.main

foo :: ForeignPtr a -> IO #type guint
foo = (`withForeignPtr`  (#peek GdkEventKey, keyval))
