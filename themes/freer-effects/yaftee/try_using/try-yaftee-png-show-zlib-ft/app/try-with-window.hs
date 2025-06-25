{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.Png.Decode qualified as Png
import Control.Monad.Yaftee.Except qualified as Except
import Data.ByteString.FingerTree qualified as BSF
import Data.Png.Header
import System.IO
import System.Environment
import Graphics.Pipe.Draw

import Stopgap.Data.Ptr
import Stopgap.System.GLib.Signal qualified as G.Signal
import Stopgap.Graphics.UI.Gtk qualified as Gtk
import Stopgap.Graphics.UI.Gtk.Widget qualified as Gtk.Widget
import Stopgap.Graphics.UI.Gtk.Window qualified as Gtk.Window

import Lib

main :: IO ()
main = do
	fpi : opts <- getArgs

	let	blk = opts == ["--block"]

	hh <- openFile fpi ReadMode
	Right hdr <- Eff.runM
		. Except.run @String
		. Png.runHeader @"foobar" . Pipe.run
		$ PipeBS.hGet 32 hh Pipe.=$=
			PipeT.convert BSF.fromStrict Pipe.=$=
			Png.decodeHeader "foobar"
	hClose hh
	print hdr

	let	wdt = fromIntegral $ headerWidth hdr
		hgt = fromIntegral $ headerHeight hdr
		ilm = headerInterlaceMethod hdr
		(yss, xss) = case ilm of
			InterlaceMethodNon -> ([0 ..] `zip` repeat (1, 1), repeat [0 ..])
			InterlaceMethodAdam7 -> (mkyss blk hgt ++ [(0, (1, 1))], mkxss wdt hgt ++ [[]])
	print (wdt, hgt, ilm)

	img <- newImageArgb32Mut wdt hgt
	print img

	join $ Gtk.init <$> getProgName <*> getArgs
	w <- Gtk.Window.new Gtk.Window.Toplevel
	G.Signal.connect_ab_bool w "delete-event" (\_ _ -> pure False) Null
	G.Signal.connect_void_void w "destroy" Gtk.mainQuit Null

	Gtk.Widget.showAll w
	Gtk.main
