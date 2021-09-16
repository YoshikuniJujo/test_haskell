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
import Graphics.Cairo.Drawing.Paths
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
cairoDrawSurface cr Surface { surfaceDraws = drws } =
	cairoDrawDraw cr `mapM_` drws

cairoDrawDraw :: CairoTIO s -> Draw 'Rgba -> IO ()
cairoDrawDraw cr Draw { drawClip = clp, drawSource = src, drawMask = msk } = do
	maybe (pure ()) (\ps -> cairoDrawPath cr `mapM_` ps >> cairoClip cr) clp
	cairoDrawSource cr src >> cairoDrawMask cr msk
	cairoResetClip cr

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
		cairoDrawPath cr `mapM_` pth >> cairoStroke cr
	MaskFill pth -> cairoDrawPath cr `mapM_` pth >> cairoFill cr

cairoDrawPath :: CairoTIO s -> Path -> IO ()
cairoDrawPath cr = \case
	PathTransform tr -> cairoTransform cr =<< transToCairoMatrixT tr
	Rectangle x_ y_ w_ h_ -> cairoRectangle cr x y w h
		where [x, y, w, h] = realToFrac <$> [x_, y_, w_, h_]
