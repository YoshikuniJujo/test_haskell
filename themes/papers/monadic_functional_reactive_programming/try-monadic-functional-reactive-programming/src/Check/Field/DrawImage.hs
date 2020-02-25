{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Check.Field.DrawImage where

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

createImageSimple :: Field -> [CChar] -> Dimension -> Dimension -> IO X.Image
createImageSimple f@Field { display = dpy } dat width height =
	withArray dat \d -> createImage f 24 X.zPixmap 0 d width height (X.bitmapUnit dpy) (4 * fromIntegral width)

createTwoColorImage :: Field -> [CChar] -> Dimension -> Dimension -> IO X.Image
createTwoColorImage f dat width height = withArray dat \d  -> createImage f 1 X.xyBitmap 0 d width height 8 0

putImage :: Field -> X.Image -> Position -> Position -> Dimension -> Dimension -> IO ()
putImage Field { display = dpy, pixmap = win, graphicsContext = gc } img x y w h =
	X.putImage dpy win gc img 0 0 x y w h

defaultDepth :: Field -> CInt
defaultDepth Field { display = dpy } = X.defaultDepth dpy (X.defaultScreen dpy)

sample :: IO Field
sample = do
	f <- openField "foo" []
	img <- createImageSimple f (replicate 40000 0x7f) 100 100
	putImage f img 100 100 100 100
	flushField f
	pure f
