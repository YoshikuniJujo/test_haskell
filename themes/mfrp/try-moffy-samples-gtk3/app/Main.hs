{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import System.Environment
import Stopgap.Data.Ptr
import Stopgap.System.GLib.Signal qualified as G.Signal
import Stopgap.Graphics.UI.Gtk qualified as Gtk
import Stopgap.Graphics.UI.Gtk.Window qualified as Gtk.Window
import Stopgap.Graphics.UI.Gtk.Widget qualified as Gtk.Widget

cbDelete :: Gtk.Window.W -> ud -> IO Bool
cbDelete w ud = pure False

main :: IO ()
main = do
	join $ Gtk.init <$> getProgName <*> getArgs

	w <- Gtk.Window.new Gtk.Window.Toplevel
	G.Signal.connect_ab_bool w "delete-event" cbDelete Null
	G.Signal.connect_void_void w "destroy" Gtk.mainQuit Null

	Gtk.Widget.showAll w
	Gtk.main
