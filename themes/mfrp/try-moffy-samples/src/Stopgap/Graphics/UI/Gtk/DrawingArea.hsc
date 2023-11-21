{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gtk.DrawingArea where

import Foreign.Ptr
import Foreign.Concurrent
import Control.Monad.ST
import Data.Int
import Stopgap.Data.Ptr

import Data.CairoContext
import Graphics.Cairo.Drawing.CairoT

import Stopgap.Graphics.UI.Gtk.Widget qualified as Gtk.Widget
import Stopgap.System.GLib.Object qualified as G.Object

data DTag

newtype D = D (Ptr DTag) deriving Show

instance IsPtr D where type Tag D = DTag; fromPtr = D; toPtr (D p) = p
instance G.Object.IsO D where toO (D d) = G.Object.O $ castPtr d
instance Gtk.Widget.IsW D where toW (D d) = Gtk.Widget.W $ castPtr d

new :: IO D
new = D <$> c_gtk_drawing_area_new

foreign import ccall "gtk_drawing_area_new" c_gtk_drawing_area_new ::
	IO (Ptr DTag)

setDrawFunc :: IsPtr ud => D -> DrawFunction r ud -> ud -> IO ()
setDrawFunc (D d) f ud = do
	cf <- wrapDrawFunction f
	c_gtk_drawing_area_set_draw_func d cf (toPtr ud) nullPtr

foreign import ccall "gtk_drawing_area_set_draw_func"
	c_gtk_drawing_area_set_draw_func ::
	Ptr DTag -> FunPtr (CDrawFunction r ud) -> Ptr ud -> Ptr () -> IO ()

wrapDrawFunction :: IsPtr ud => DrawFunction r ud -> IO (FunPtr (CDrawFunction r (Tag ud)))
wrapDrawFunction = c_wrapDrawFunction . drawFunctionToC

drawFunctionToC :: IsPtr ud => DrawFunction r ud -> CDrawFunction r (Tag ud)
drawFunctionToC f pd pcr w h pud = do
	cr <- CairoT <$> newForeignPtr pcr (pure ())
	f (D pd) cr w h (fromPtr pud)

type DrawFunction r ud =
	D -> CairoT r RealWorld -> #{type int} -> #{type int} -> ud -> IO ()

type CDrawFunction r ud =
	Ptr DTag -> Ptr (CairoT r RealWorld) -> #{type int} -> #{type int} ->
	Ptr ud -> IO ()

foreign import ccall "wrapper" c_wrapDrawFunction ::
	CDrawFunction r ud -> IO (FunPtr (CDrawFunction r ud))
