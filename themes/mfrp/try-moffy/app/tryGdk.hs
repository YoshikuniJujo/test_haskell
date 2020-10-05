{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.Marshal
import Foreign.Storable

import System.Environment
import Graphics.Gdk

main :: IO ()
main = do
	as <- getArgs
	print =<< gdkInit as
	alloca $ \p -> do
		attr <- peek p
		gdkWindowAttrSetWindowType attr gdkWindowToplevel
		gdkWindowAttrSetWidth attr 400
		gdkWindowAttrSetHeight attr 400
		gdkWindowAttrSetWClass attr gdkInputOutput
		gdkWindowShow =<< gdkWindowNew Nothing attr [gdkWaWmclass]
		print =<< gdkEventGet
		print =<< gdkEventGet
		print =<< gdkEventGet
		print =<< gdkEventGet
		print =<< gdkEventGet
	getChar
	pure ()
