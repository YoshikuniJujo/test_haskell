{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryCairo where

import Control.Monad.ST
import Data.ImageData as I
import Data.CairoContext
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoT.Setting as Cr
import Graphics.Cairo.Drawing.CairoT.Clip
import Graphics.Cairo.Drawing.CairoT.CairoOperatorT as Cr
import Graphics.Cairo.Drawing.Paths.Basic
import Graphics.Cairo.Drawing.Transformations
import Graphics.Cairo.Utilities.CairoMatrixT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

drawSurface :: Surface 'Rgba -> IO (CairoSurfaceImageT s RealWorld)
drawSurface sfc@Surface {
	sfcWidth = (fromInteger -> w), sfcHeight = (fromInteger -> h) } = do
	sr <- cairoImageSurfaceCreate cairoFormatArgb32 w h
	cr <- cairoCreate sr
	cairoDrawSurface cr sfc
	pure sr

cairoDrawSurface :: CairoTIO s -> Surface 'Rgba -> IO ()
cairoDrawSurface cr Surface { surfaceClips = clps } =
	cairoDrawClip cr `mapM_` clps

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
	PatternSurface sfc -> error "yet"
	PatternColor (ColorRgba clr) -> cairoSetSourceRgba cr clr

cairoDrawMask :: CairoTIO s -> Mask -> IO ()
cairoDrawMask cr = \case
	MaskAlpha alp -> error "yet"
	MaskPaint alp -> cairoPaintWithAlpha cr $ realToFrac alp
	MaskStroke (I.LineWidth lw) lj pth -> do
		cairoSet cr . Cr.LineWidth $ realToFrac lw
		case lj of
			I.LineJoinMiter (realToFrac -> ml) -> do
				cairoSet cr Cr.LineJoinMiter
				cairoSet cr $ MiterLimit ml
			I.LineJoinRound -> cairoSet cr Cr.LineJoinRound
			I.LineJoinBevel -> cairoSet cr Cr.LineJoinBevel
		cairoDrawPaths cr pth >> cairoStroke cr
	MaskFill pth -> cairoDrawPaths cr pth >> cairoFill cr

cairoDrawPaths :: CairoTIO s -> [Path] -> IO ()
cairoDrawPaths cr pths = do
	cairoDrawPath cr `mapM_` pths
	cairoIdentityMatrix cr

cairoDrawPath :: CairoTIO s -> Path -> IO ()
cairoDrawPath cr = \case
	PathTransform tr -> cairoTransform cr =<< transToCairoMatrixT tr
	Rectangle x_ y_ w_ h_ -> cairoRectangle cr x y w h
		where [x, y, w, h] = realToFrac <$> [x_, y_, w_, h_]
	Arc xc_ yc_ r_ a1_ a2_ -> cairoArc cr xc yc r a1 a2
		where
		[xc, yc, r] = realToFrac <$> [xc_, yc_, r_]
		[a1, a2] = realToFrac <$> [a1_, a2_]
	ArcNegative xc_ yc_ r_ a1_ a2_ -> cairoArcNegative cr xc yc r a1 a2
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
