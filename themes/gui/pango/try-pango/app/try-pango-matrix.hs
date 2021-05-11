{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types
import Control.Monad.ST

import Graphics.Pango.Basic.GlyphStorage.PangoMatrix

main :: IO ()
main = do
	let	u = PangoMatrix 1 0 0 1 0 0
		m0 = PangoMatrix 1 2 3 4 5 6
	print m0
	print $ pangoMatrixTranslatePure m0 100 200
	print $ pangoMatrixScalePure m0 10 100
	print $ pangoMatrixRotatePure m0 (Radian pi)
	print $ pangoMatrixConcatPure m0 $ PangoMatrix 1 0 0 1 10 20
	let	m1 = pangoMatrixTranslatePure (pangoMatrixRotatePure u (Radian $ pi / 2) ) 10 100
	print $ pangoMatrixTransformPoint m1 15 200
	print $ pangoMatrixTransformDistance m1 15 200

pangoMatrixTranslatePure :: PangoMatrix -> CDouble -> CDouble -> PangoMatrix
pangoMatrixTranslatePure m tx ty = runST do
	m' <- pangoMatrixThaw m
	pangoMatrixTranslate m' tx ty
	pangoMatrixFreeze m'

pangoMatrixScalePure :: PangoMatrix -> CDouble -> CDouble -> PangoMatrix
pangoMatrixScalePure m sx sy = runST do
	m' <- pangoMatrixThaw m
	pangoMatrixScale m' sx sy
	pangoMatrixFreeze m'

pangoMatrixRotatePure :: PangoMatrix -> Angle -> PangoMatrix
pangoMatrixRotatePure m a = runST do
	m' <- pangoMatrixThaw m
	pangoMatrixRotate m' a
	pangoMatrixFreeze m'

pangoMatrixConcatPure :: PangoMatrix -> PangoMatrix -> PangoMatrix
pangoMatrixConcatPure m1 m2 = runST do
	m1' <- pangoMatrixThaw m1
	pangoMatrixConcat m1' m2
	pangoMatrixFreeze m1'
