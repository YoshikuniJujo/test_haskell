{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.LayoutObjects.PangoLayoutLine where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.C.Types

import Graphics.Pango.Basic.LayoutObjects.PangoLayout

pangoLayoutGetLine :: PangoLayout -> CInt -> IO PangoLayoutLine
pangoLayoutGetLine (PangoLayout_ fpl) ln =
	makePangoLayoutLine0 =<< withForeignPtr fpl \pl -> c_pango_layout_get_line_readonly pl ln

foreign import ccall "pango_layout_get_line_readonly" c_pango_layout_get_line_readonly ::
	Ptr PangoLayout -> CInt -> IO (Ptr PangoLayoutLine)

newtype PangoLayoutLine = PangoLayoutLine (ForeignPtr PangoLayoutLine) deriving Show

makePangoLayoutLine0 :: Ptr PangoLayoutLine -> IO PangoLayoutLine
makePangoLayoutLine0 p = PangoLayoutLine <$> newForeignPtr p (pure ())

makePangoLayoutLine :: Ptr PangoLayoutLine -> IO PangoLayoutLine
makePangoLayoutLine p = PangoLayoutLine <$> newForeignPtr p (c_pango_layout_line_unref p)

foreign import ccall "pango_layout_line_unref" c_pango_layout_line_unref ::
	Ptr PangoLayoutLine -> IO ()
