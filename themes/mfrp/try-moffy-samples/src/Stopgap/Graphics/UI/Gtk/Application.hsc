{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gtk.Application where

import Foreign.Ptr
import Foreign.C.String
import Data.Word
import Data.String

import Stopgap.System.GLib.Object qualified as G.Object
import Stopgap.System.GLib.Application qualified as G.Application
import Stopgap.Data.Ptr

#include <gtk/gtk.h>

data ATag

newtype A = A (Ptr ATag) deriving Show

instance G.Application.IsA A where toA = gApplication

instance IsPtr A where
	type Tag A = ATag
	fromPtr = A
	toPtr (A p) = p

data Id = Id String deriving Show

instance IsString Id where fromString = Id

new :: Id -> G.Application.Flags -> IO A
new (Id aid) (G.Application.Flags flgs) =
	A <$> withCString aid \caid -> c_gtk_application_new caid flgs

foreign import ccall "gtk_application_new" c_gtk_application_new ::
	CString -> #{type GApplicationFlags} -> IO (Ptr ATag)

gApplication :: A -> G.Application.A
gApplication (A pa) = G.Application.A $ c_G_APPLICATION pa

foreign import capi "gtk/gtk.h G_APPLICATION" c_G_APPLICATION ::
	Ptr ATag -> Ptr G.Application.ATag
