{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types
import Control.Monad.ST

import Graphics.Pango.Basic.GlyphStorage.PangoMatrix

main :: IO ()
main = do
	let	m0 = PangoMatrix 1 2 3 4 5 6
	print m0
	print $ pangoMatrixTranslatePure m0 100 200
	print $ pangoMatrixScalePure m0 10 100
	print $ pangoMatrixRotatePure m0 (Radian pi)

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
