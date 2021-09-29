{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryCairo where

import Control.Monad.ST
import Data.Color
import Data.CairoImage
import Data.CairoContext
import Data.ImageData as I
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoT.Setting as Cr
import Graphics.Cairo.Drawing.CairoT.Clip
import Graphics.Cairo.Drawing.CairoT.CairoOperatorT as Cr
import Graphics.Cairo.Drawing.Paths.Basic
import Graphics.Cairo.Drawing.CairoPatternT.Basic
import Graphics.Cairo.Drawing.CairoPatternT.Setting
import Graphics.Cairo.Drawing.Transformations
import Graphics.Cairo.Utilities.CairoMatrixT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

import Trial.TryPango

makeSurface :: Surface 'Rgba -> IO (CairoSurfaceImageT s RealWorld)
makeSurface Surface { surfaceBase = sb, surfaceClips = clps } = do
	sr <- makeSurfaceBase sb
	sr <$ ((`cairoRunDrawScript` clps) =<< cairoCreate sr)

makeSurfaceBase :: SurfaceBase 'Rgba -> IO (CairoSurfaceImageT s RealWorld)
makeSurfaceBase = \case
	SurfaceBaseBlank {
		surfaceBaseWidth = (fromIntegral -> w),
		surfaceBaseHeight = (fromIntegral -> h) } ->
		cairoImageSurfaceCreate cairoFormatArgb32 w h
	SurfaceBaseArgb32 img ->
		cairoImageSurfaceCreateForCairoImage $ CairoImageArgb32 img

cairoRunDrawScript :: CairoTIO s -> DrawScript 'Rgba -> IO ()
cairoRunDrawScript cr = (cairoDrawClip cr `mapM_`)

cairoDrawClip :: CairoTIO s -> Clip 'Rgba -> IO ()
cairoDrawClip cr Clip { clipBounds = bs, clipDraws = drws } = do
	(\ps -> cairoDrawPaths cr ps >> cairoClip cr) `mapM_` bs
	cairoDrawDraw cr `mapM_` drws
	cairoResetClip cr

cairoDrawDraw :: CairoTIO s -> Draw 'Rgba -> IO ()
cairoDrawDraw cr Draw {
	drawOperator = op, drawSource = src, drawMask = msk } = do
	cairoSet cr $ getOperator op
	cairoDrawSource cr src >> cairoDrawMask cr msk

transToCairoMatrixT :: Transform -> IO (CairoMatrixT RealWorld)
transToCairoMatrixT (Transform xx_ yx_ xy_ yy_ x0_ y0_) =
	cairoMatrixNew xx yx xy yy x0 y0
	where
	[xx, yx, xy, yy, x0, y0] = realToFrac <$> [xx_, xy_, yx_, yy_, x0_, y0_]

cairoDrawSource :: CairoTIO s -> Source 'Rgba -> IO ()
cairoDrawSource cr (Source ptn) = case ptn of
	PatternSolid (ColorRgba clr) -> cairoSetSourceRgba cr $ rgbaRealToFrac clr
	PatternNonSolid tfm (PatternSurface sfc) -> do
		t <- transToCairoMatrixT tfm
		s <- makeSurface sfc
		pt <- cairoPatternCreateForSurface s
		cairoPatternSetMatrix pt t
		cairoSetSource cr pt

cairoDrawMask :: CairoTIO s -> Mask -> IO ()
cairoDrawMask cr = \case
	MaskAlpha _alp -> error "yet"
	MaskPaint alp -> cairoPaintWithAlpha cr $ realToFrac alp
	MaskStroke (I.LineWidth lw) lc lj pth -> do
		cairoSet cr . Cr.LineWidth $ realToFrac lw
		cairoSet cr $ toLineCap lc
		case lj of
			I.LineJoinMiter (realToFrac -> ml) -> do
				cairoSet cr Cr.LineJoinMiter
				cairoSet cr $ MiterLimit ml
			I.LineJoinRound -> cairoSet cr Cr.LineJoinRound
			I.LineJoinBevel -> cairoSet cr Cr.LineJoinBevel
		cairoDrawPaths cr pth >> cairoStroke cr
	MaskFill pth -> cairoDrawPaths cr pth >> cairoFill cr
	MaskTextLayout tr l -> do
		cairoTransform cr =<< transToCairoMatrixT tr
		drawLayout cr l

toLineCap :: I.LineCap -> Cr.LineCap
toLineCap = \case
	I.LineCapButt -> Cr.LineCapButt
	I.LineCapRound -> Cr.LineCapRound
	I.LineCapSquare -> Cr.LineCapSquare

cairoDrawPaths :: CairoTIO s -> [Path] -> IO ()
cairoDrawPaths cr pths = do
	cairoDrawPath cr `mapM_` pths
	cairoIdentityMatrix cr

cairoDrawPath :: CairoTIO s -> Path -> IO ()
cairoDrawPath cr = \case
	PathTransform tr -> cairoTransform cr =<< transToCairoMatrixT tr
	MoveTo (realToFrac -> x) (realToFrac -> y) -> cairoMoveTo cr x y
	LineTo (realToFrac -> x) (realToFrac -> y) -> cairoLineTo cr x y
	Rectangle x_ y_ w_ h_ -> cairoRectangle cr x y w h
		where [x, y, w, h] = realToFrac <$> [x_, y_, w_, h_]
	Arc xc_ yc_ r_ a1_ a2_ -> cairoNewSubPath cr >> cairoArc cr xc yc r a1 a2
		where
		[xc, yc, r] = realToFrac <$> [xc_, yc_, r_]
		[a1, a2] = realToFrac <$> [a1_, a2_]
	ArcNegative xc_ yc_ r_ a1_ a2_ -> cairoNewSubPath cr >> cairoArcNegative cr xc yc r a1 a2
		where
		[xc, yc, r] = realToFrac <$> [xc_, yc_, r_]
		[a1, a2] = realToFrac <$> [a1_, a2_]

getOperator :: I.Operator -> Cr.Operator
getOperator = \case
	I.OperatorClear -> Cr.OperatorClear
	I.OperatorSource -> Cr.OperatorSource
	I.OperatorOver -> Cr.OperatorOver
	I.OperatorIn -> Cr.OperatorIn
	I.OperatorOut -> Cr.OperatorOut
	I.OperatorAtop -> Cr.OperatorAtop
	I.OperatorDest -> Cr.OperatorDest
	I.OperatorDestOver -> Cr.OperatorDestOver
	I.OperatorDestIn -> Cr.OperatorDestIn
	I.OperatorDestOut -> Cr.OperatorDestOut
	I.OperatorDestAtop -> Cr.OperatorDestAtop
	I.OperatorXor -> Cr.OperatorXor
	I.OperatorAdd -> Cr.OperatorAdd
	I.OperatorSaturate -> Cr.OperatorSaturate
	I.OperatorMultiply -> Cr.OperatorMultiply
	I.OperatorScreen -> Cr.OperatorScreen
	I.OperatorOverlay -> Cr.OperatorOverlay
	I.OperatorDarken -> Cr.OperatorDarken
	I.OperatorLighten -> Cr.OperatorLighten
	I.OperatorColorDodge -> Cr.OperatorColorDodge
	I.OperatorColorBurn -> Cr.OperatorColorBurn
	I.OperatorHardLight -> Cr.OperatorHardLight
	I.OperatorSoftLight -> Cr.OperatorSoftLight
	I.OperatorDifference -> Cr.OperatorDifference
	I.OperatorExclusion -> Cr.OperatorExclusion
	I.OperatorHslHue -> Cr.OperatorHslHue
	I.OperatorHslSaturation -> Cr.OperatorHslSaturation
	I.OperatorHslColor -> Cr.OperatorHslColor
	I.OperatorHslLuminosity -> Cr.OperatorHslLuminosity
