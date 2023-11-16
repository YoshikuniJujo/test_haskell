{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
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

newtype A s = A (Ptr ATag) deriving Show

instance G.Application.IsA (A s) where toA = gApplication

instance IsPtr (A s) where
	type Tag (A s) = ATag
	fromPtr = A
	toPtr (A p) = p

instance G.Object.IsO (A s) where toO (A p) = G.Object.O $ castPtr p

data Id = Id String deriving Show

instance IsString Id where fromString = Id

with :: Id -> G.Application.Flags -> (forall s . A s -> IO a) -> IO a
with (Id aid) (G.Application.Flags flgs) f = G.Object.withObject
	(A <$> withCString aid \caid -> c_gtk_application_new caid flgs) f

foreign import ccall "gtk_application_new" c_gtk_application_new ::
	CString -> #{type GApplicationFlags} -> IO (Ptr ATag)

gApplication :: A s -> G.Application.A
gApplication (A pa) = G.Application.A $ c_G_APPLICATION pa

foreign import capi "gtk/gtk.h G_APPLICATION" c_G_APPLICATION ::
	Ptr ATag -> Ptr G.Application.ATag
