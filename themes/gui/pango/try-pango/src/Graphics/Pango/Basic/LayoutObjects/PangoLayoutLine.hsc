{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.LayoutObjects.PangoLayoutLine where

import Foreign.Ptr
import Foreign.Ptr.Misc
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.C.Types

import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import System.Glib.SinglyLinkedLists

pangoLayoutGetLine :: PangoLayout -> CInt -> IO (Maybe PangoLayoutLine)
pangoLayoutGetLine (PangoLayout_ fpl) ln = makePangoLayoutLineMaybe
	=<< withForeignPtr fpl \pl -> c_pango_layout_get_line pl ln

foreign import ccall "pango_layout_get_line" c_pango_layout_get_line ::
	Ptr PangoLayout -> CInt -> IO (Ptr PangoLayoutLine)

newtype PangoLayoutLine = PangoLayoutLine (ForeignPtr PangoLayoutLine) deriving Show

makePangoLayoutLine :: Ptr PangoLayoutLine -> IO PangoLayoutLine
makePangoLayoutLine p = PangoLayoutLine <$> newForeignPtr p (c_pango_layout_line_unref p)

makePangoLayoutLineMaybe :: Ptr PangoLayoutLine -> IO (Maybe PangoLayoutLine)
makePangoLayoutLineMaybe = \case
	NullPtr -> pure Nothing
	p -> Just . PangoLayoutLine
		<$> newForeignPtr p (c_pango_layout_line_unref p)

makePangoLayoutLine0 :: Ptr PangoLayoutLine -> IO PangoLayoutLine
makePangoLayoutLine0 p = PangoLayoutLine <$> newForeignPtr p (pure ())

foreign import ccall "pango_layout_line_unref" c_pango_layout_line_unref ::
	Ptr PangoLayoutLine -> IO ()

pangoLayoutGetLines :: PangoLayout -> IO [PangoLayoutLine]
pangoLayoutGetLines (PangoLayout_ fl) = withForeignPtr fl \pl ->
	(makePangoLayoutLine `mapM`) =<< g_slist_to_list =<< c_pango_layout_get_lines pl

foreign import ccall "pango_layout_get_lines" c_pango_layout_get_lines ::
	Ptr PangoLayout -> IO (Ptr (GSList PangoLayoutLine))
