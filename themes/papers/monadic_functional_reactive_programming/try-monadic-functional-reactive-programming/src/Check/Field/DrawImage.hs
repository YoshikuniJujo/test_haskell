{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Check.Field.DrawImage (createImageSimple, putImage, sample) where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Array
import qualified Graphics.X11 as X

import Field.Internal

createImage :: Field -> CInt -> X.ImageFormat -> CInt -> Ptr CChar -> Dimension -> Dimension -> CInt -> CInt -> IO X.Image
createImage Field { display = dpy } depth format offset dat width height bitmap_pad bytes_per_line =
	X.createImage dpy vis depth format offset dat width height bitmap_pad bytes_per_line
	where
	vis = X.defaultVisual dpy (X.defaultScreen dpy)

createImageSimple :: Field -> Ptr CChar -> Dimension -> Dimension -> IO X.Image
createImageSimple f@Field { display = dpy } dat width height =
	createImage f (defaultDepth f) X.zPixmap 0 dat width height (X.bitmapUnit dpy) 0

createImageSimple' :: Field -> [CChar] -> Dimension -> Dimension -> IO X.Image
createImageSimple' f@Field { display = dpy } dat width height =
	withArray dat \d -> createImage f (defaultDepth f) X.zPixmap 0 d width height (X.bitmapUnit dpy) 0

putImage :: Field -> X.Image -> Position -> Position -> Dimension -> Dimension -> IO ()
putImage Field { display = dpy, pixmap = win, graphicsContext = gc } img x y w h =
	X.putImage dpy win gc img 0 0 x y w h

defaultDepth :: Field -> CInt
defaultDepth Field { display = dpy } = X.defaultDepth dpy (X.defaultScreen dpy)

sample :: IO Field
sample = do
	f <- openField "foo" []
	img <- createImageSimple' f (replicate 40000 0x7f) 100 100
	putImage f img 100 100 100 100
	flushField f
	pure f
