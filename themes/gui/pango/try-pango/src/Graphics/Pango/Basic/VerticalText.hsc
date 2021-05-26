{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.VerticalText where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Data.Bool
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

pangoGravityGetForScriptAndWidth ::
	PangoScript -> Bool -> PangoGravity -> PangoGravityHint -> PangoGravity
pangoGravityGetForScriptAndWidth
	(PangoScript s) w (PangoGravity b) (PangoGravityHint h) =
	unsafePerformIO $ PangoGravity <$>
		c_pango_gravity_get_for_script_and_width
			s (bool #{const FALSE} #{const TRUE} w) b h

foreign import ccall "pango_gravity_get_for_script_and_width"
	c_pango_gravity_get_for_script_and_width ::
	#{type PangoScript} -> #{type gboolean} -> #{type PangoGravity} ->
	#{type PangoGravityHint} -> IO #{type PangoGravity}

pangoGravityToRotation :: PangoGravity -> CDouble
pangoGravityToRotation (PangoGravity g) =
	unsafePerformIO $ c_pango_gravity_to_rotation g

foreign import ccall "pango_gravity_to_rotation" c_pango_gravity_to_rotation ::
	#{type PangoGravity} -> IO CDouble
