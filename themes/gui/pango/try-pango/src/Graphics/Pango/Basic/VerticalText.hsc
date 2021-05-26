{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.VerticalText where

import Foreign.Ptr
import Foreign.ForeignPtr
import Data.Word
import Data.Int
import System.IO.Unsafe
import Graphics.Pango.Values
import Graphics.Pango.Basic.GlyphStorage.PangoMatrix
import Graphics.Pango.Basic.ScriptsAndLanguages.PangoScript

#include <pango/pango.h>

pangoGravityGetForMatrix :: PangoMatrix -> PangoGravity
pangoGravityGetForMatrix (PangoMatrix_ fm) = unsafePerformIO
	$ PangoGravity <$> withForeignPtr fm c_pango_gravity_get_for_matrix

foreign import ccall "pango_gravity_get_for_matrix"
	c_pango_gravity_get_for_matrix ::
	Ptr PangoMatrix -> IO #{type PangoGravity}

pangoGravityGetForScript ::
	PangoScript -> PangoGravity -> PangoGravityHint -> PangoGravity
pangoGravityGetForScript (PangoScript s) (PangoGravity b) (PangoGravityHint h) =
	unsafePerformIO $ PangoGravity <$> c_pango_gravity_get_for_script s b h

foreign import ccall "pango_gravity_get_for_script"
	c_pango_gravity_get_for_script ::
	#{type PangoScript} -> #{type PangoGravity} ->
	#{type PangoGravityHint} -> IO #{type PangoGravity}
