{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.GlyphStorage.PangoMatrix (

	-- * TYPE
	-- ** PangoMatrix
	PangoMatrix, pattern PangoMatrix,
	pangoMatrixXx, pangoMatrixXy, pangoMatrixYx, pangoMatrixYy,
	pangoMatrixX0, pangoMatrixY0,

	-- ** PangoMatrixPrim
	PangoMatrixPrim, PangoMatrixST, PangoMatrixIO,
	pangoMatrixFreeze, pangoMatrixThaw, pangoMatrixCopy,

	-- ** PangoMatrixNullable
	PangoMatrixNullable, pangoMatrixFromNullable, pangoMatrixToNullable,

	-- * FUNCTION
	pangoMatrixTranslate, pangoMatrixScale, pangoMatrixRotate,
	pangoMatrixConcat,

	pangoMatrixTransformPoint, pangoMatrixTransformDistance,
	pangoMatrixTransformRectangle, pangoMatrixTransformPixelRectangle,
	pangoMatrixGetFontScaleFactor, pangoMatrixGetFontScaleFactors

	) where

import Graphics.Pango.Basic.GlyphStorage.PangoMatrix.Internal
