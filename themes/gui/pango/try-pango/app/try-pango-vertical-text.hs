{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.ST
import Graphics.Pango.Values
import Graphics.Pango.Basic.GlyphStorage.PangoMatrix
import Graphics.Pango.Basic.ScriptsAndLanguages.PangoScript
import Graphics.Pango.Basic.VerticalText

unit :: PangoMatrix
unit = PangoMatrix 1 0 0 1 0 0

main :: IO ()
main = do
	print . pangoGravityGetForMatrix . pangoMatrixRotatePure unit $ Degree 30
	print . pangoGravityGetForMatrix . pangoMatrixRotatePure unit $ Degree 45
	print . pangoGravityGetForMatrix . pangoMatrixRotatePure unit $ Degree 46
	print . pangoGravityGetForMatrix . pangoMatrixRotatePure unit $ Degree 60
	print . pangoGravityGetForMatrix . pangoMatrixRotatePure unit $ Degree 120
	print . pangoGravityGetForMatrix . pangoMatrixRotatePure unit $ Degree 150
	print . pangoGravityGetForMatrix . pangoMatrixRotatePure unit $ Degree 210
	print . pangoGravityGetForMatrix . pangoMatrixRotatePure unit $ Degree 240
	print . pangoGravityGetForMatrix . pangoMatrixRotatePure unit $ Degree 300
	print . pangoGravityGetForMatrix . pangoMatrixRotatePure unit $ Degree 330

	print $ pangoGravityGetForScript
		PangoScriptHan PangoGravitySouth PangoGravityHintNatural
	print $ pangoGravityGetForScript
		PangoScriptHan PangoGravityEast PangoGravityHintNatural

	print $ pangoGravityGetForScriptAndWidth
		PangoScriptKatakana False PangoGravitySouth PangoGravityHintNatural
	print $ pangoGravityGetForScriptAndWidth
		PangoScriptKatakana False PangoGravityEast PangoGravityHintNatural

	print $ pangoGravityToRotation PangoGravityAuto
	print $ pangoGravityToRotation PangoGravitySouth
	print $ pangoGravityToRotation PangoGravityEast
	print $ pangoGravityToRotation PangoGravityNorth
	print $ pangoGravityToRotation PangoGravityWest

pangoMatrixRotatePure :: PangoMatrix -> Angle -> PangoMatrix
pangoMatrixRotatePure m a = runST do
	mp <- pangoMatrixThaw m
	pangoMatrixRotate mp a
	pangoMatrixFreeze mp
