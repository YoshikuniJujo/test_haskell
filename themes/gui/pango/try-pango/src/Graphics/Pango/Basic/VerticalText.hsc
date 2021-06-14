{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.VerticalText (
	-- * ENUM
	-- ** PangoGravity
	PangoGravity(..),
	pattern PangoGravitySouth,
	pattern PangoGravityEast,
	pattern PangoGravityNorth,
	pattern PangoGravityWest,
	pattern PangoGravityAuto,

	-- ** PangoGravityHint
	PangoGravityHint(..),
	pattern PangoGravityHintNatural,
	pattern PangoGravityHintStrong,
	pattern PangoGravityHintLine,

	-- * FUNCTION
	pangoGravityGetForMatrix,
	pangoGravityGetForScript,
	pangoGravityGetForScriptAndWidth,
	pangoGravityToRotation
	) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.C.Enum
import Data.Bool
import Data.Word
import Data.Int
import Data.Angle
import System.IO.Unsafe
import Graphics.Pango.Basic.GlyphStorage.PangoMatrix.Internal
import Graphics.Pango.Basic.ScriptsAndLanguages.PangoScript.Enum

#include <pango/pango.h>

enum "PangoGravity" ''#{type PangoGravity} [''Show] [
	("PangoGravitySouth", #{const PANGO_GRAVITY_SOUTH}),
	("PangoGravityEast", #{const PANGO_GRAVITY_EAST}),
	("PangoGravityNorth", #{const PANGO_GRAVITY_NORTH}),
	("PangoGravityWest", #{const PANGO_GRAVITY_WEST}),
	("PangoGravityAuto", #{const PANGO_GRAVITY_AUTO}) ]

enum "PangoGravityHint" ''#{type PangoGravityHint} [''Show] [
	("PangoGravityHintNatural", #{const PANGO_GRAVITY_HINT_NATURAL}),
	("PangoGravityHintStrong", #{const PANGO_GRAVITY_HINT_STRONG}),
	("PangoGravityHintLine", #{const PANGO_GRAVITY_HINT_LINE}) ]

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

pangoGravityToRotation :: PangoGravity -> Angle CDouble
pangoGravityToRotation (PangoGravity g) =
	unsafePerformIO $ Radian <$> c_pango_gravity_to_rotation g

foreign import ccall "pango_gravity_to_rotation" c_pango_gravity_to_rotation ::
	#{type PangoGravity} -> IO CDouble
