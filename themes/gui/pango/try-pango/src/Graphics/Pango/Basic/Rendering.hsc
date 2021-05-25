{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.Rendering where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Data.Word

import Graphics.Pango.Types
import Graphics.Pango.Values
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Type

import Graphics.Pango.Basic.GlyphStorage.PangoMatrix

#include <pango/pango.h>

newtype PangoContext = PangoContext (ForeignPtr PangoContext) deriving Show

mkPangoContext :: Ptr PangoContext -> IO PangoContext
mkPangoContext p = PangoContext <$> newForeignPtr p (c_g_object_unref p)

class PangoContextSetting s where
	pangoContextSet :: PangoContext -> s -> IO ()
	pangoContextGet :: PangoContext -> IO s

newtype BaseGravity = BaseGravity { getBaseGravity :: PangoGravity } deriving Show

instance PangoContextSetting BaseGravity where
	pangoContextSet c = pangoContextSetBaseGravity c . getBaseGravity
	pangoContextGet c = BaseGravity <$> pangoContextGetBaseGravity c

pangoContextSetBaseGravity :: PangoContext -> PangoGravity -> IO ()
pangoContextSetBaseGravity (PangoContext fc) (PangoGravity g) =
	withForeignPtr fc \pc -> c_pango_context_set_base_gravity pc g

pangoContextGetBaseGravity :: PangoContext -> IO PangoGravity
pangoContextGetBaseGravity (PangoContext fc) =
	PangoGravity <$> withForeignPtr fc c_pango_context_get_base_gravity

foreign import ccall "pango_context_set_base_gravity"
	c_pango_context_set_base_gravity ::
	Ptr PangoContext -> #{type PangoGravity} -> IO ()

foreign import ccall "pango_context_get_base_gravity"
	c_pango_context_get_base_gravity ::
	Ptr PangoContext -> IO #{type PangoGravity}

instance PangoContextSetting PangoFontDescription where
	pangoContextSet = pangoContextSetFontDescription
	pangoContextGet = pangoContextGetFontDescription

pangoContextSetFontDescription :: PangoContext -> PangoFontDescription -> IO ()
pangoContextSetFontDescription cxt PangoFontDescriptionNull = do
	fd <- pangoFontDescriptionFreeze =<< pangoFontDescriptionNew
	case fd of
		PangoFontDescriptionNull -> error "pangoFontDescriptionNew return NULL"
		_ -> pangoContextSetFontDescription cxt fd
pangoContextSetFontDescription (PangoContext fc) (PangoFontDescription ffd) =
	withForeignPtr fc \pc -> withForeignPtr ffd \pfd ->
		c_pango_context_set_font_description pc pfd

pangoContextGetFontDescription :: PangoContext -> IO PangoFontDescription
pangoContextGetFontDescription (PangoContext fc) = mkPangoFontDescription
	=<< withForeignPtr fc c_pango_context_get_font_description

foreign import ccall "pango_context_set_font_description"
	c_pango_context_set_font_description ::
	Ptr PangoContext -> Ptr PangoFontDescription -> IO ()

foreign import ccall "pango_context_get_font_description"
	c_pango_context_get_font_description ::
	Ptr PangoContext -> IO (Ptr PangoFontDescription)

instance PangoContextSetting PangoMatrix where
	pangoContextSet = pangoContextSetMatrix
	pangoContextGet = pangoContextGetMatrix

pangoContextGetMatrix :: PangoContext -> IO PangoMatrix
pangoContextGetMatrix (PangoContext fc) =
	PangoMatrix_ <$> do
		p <- withForeignPtr fc c_pango_context_get_matrix
		p' <- c_pango_matrix_copy p
		newForeignPtr p' $ c_pango_matrix_free p'

foreign import ccall "pango_context_get_matrix" c_pango_context_get_matrix ::
	Ptr PangoContext -> IO (Ptr PangoMatrix)

pangoContextSetMatrix :: PangoContext -> PangoMatrix -> IO ()
pangoContextSetMatrix (PangoContext fc) (PangoMatrix_ fm) =
	withForeignPtr fc \pc -> withForeignPtr fm \pm ->
		c_pango_context_set_matrix pc pm

foreign import ccall "pango_context_set_matrix" c_pango_context_set_matrix ::
	Ptr PangoContext -> Ptr PangoMatrix -> IO ()
