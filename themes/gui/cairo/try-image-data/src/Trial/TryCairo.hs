{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryCairo where

import Foreign.C.Types
import Control.Monad.ST
import Data.Maybe
import Data.Color
import Data.CairoImage.Internal
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

import Trial.TryPango

import qualified Graphics.Cairo.Drawing.CairoPatternT.Mesh as M
import qualified Graphics.Cairo.Drawing.Paths.CairoPathT as P

makeSurface :: Surface t -> IO (CairoSurfaceImageT s RealWorld)
makeSurface Surface { surfaceBase = sb, surfaceClips = clps } = do
	sr <- makeSurfaceBase sb
	sr <$ ((`cairoRunDrawScript` clps) =<< cairoCreate sr)

makeSurfaceBase :: SurfaceBase t -> IO (CairoSurfaceImageT s RealWorld)
makeSurfaceBase = \case
	SurfaceBaseBlank {
		surfaceBaseWidth = (fromIntegral -> w),
		surfaceBaseHeight = (fromIntegral -> h) } ->
		cairoImageSurfaceCreate CairoFormatArgb32 w h
	SurfaceBaseA8 img ->
		cairoImageSurfaceCreateForCairoImage $ CairoImageA8 img
	SurfaceBaseArgb32 img ->
		cairoImageSurfaceCreateForCairoImage $ CairoImageArgb32 img

cairoRunDrawScript :: CairoTIO s -> DrawScript t -> IO ()
cairoRunDrawScript cr = (cairoDrawClip cr `mapM_`)

cairoDrawClip :: CairoTIO s -> Clip t -> IO ()
cairoDrawClip cr Clip { clipBounds = bs, clipDraws = drws } = do
	(\(Bound fr ps) -> do
		cairoSet cr $ toFillRule fr
		cairoDrawPaths cr ps >> cairoClip cr) `mapM_` bs
	cairoDrawDraw cr `mapM_` drws
	cairoResetClip cr

cairoDrawDraw :: CairoTIO s -> Draw t -> IO ()
cairoDrawDraw cr Draw {
	drawOperator = op, drawSource = src, drawMask = msk } = do
	cairoSet cr $ getOperator op
	cairoDrawSource cr src >> cairoDrawMask cr msk

transToCairoMatrixT :: Transform -> IO (CairoMatrixT RealWorld)
transToCairoMatrixT (Transform xx_ yx_ xy_ yy_ x0_ y0_) =
	cairoMatrixNew xx yx xy yy x0 y0
	where
	[xx, yx, xy, yy, x0, y0] = realToFrac <$> [xx_, xy_, yx_, yy_, x0_, y0_]

cairoDrawSource :: CairoTIO s -> Source t -> IO ()
cairoDrawSource cr (Source ptn) = case ptn of
	PatternSolid (ColorRgba clr) -> cairoSetSourceRgba cr $ rgbaRealToFrac clr
	PatternSolid (ColorAlpha a) -> cairoSetSourceRgba cr
		$ toRgba (fromJust $ rgbDouble 0 0 0) (fromJust . alphaDouble $ realToFrac a)
	PatternNonSolid pf pe tfm nspt -> do
		t <- transToCairoMatrixT tfm
		pt <- nonSolidPattern nspt
		cairoPatternSet pt $ toCairoFilterT pf
		cairoPatternSet pt $ toCairoExtendT pe
		cairoPatternSetMatrix pt t
		cairoSetSource cr pt

nonSolidPattern :: PatternNonSolid t -> IO (CairoPatternT RealWorld)
nonSolidPattern = \case
	PatternSurface sfc -> CairoPatternTSurface
		<$> (cairoPatternCreateForSurface =<< makeSurface sfc)
	PatternGradient (GradientFrameLinear (x1_, y1_) (x2_, y2_)) pcs -> do
		pt <- cairoPatternCreateLinear x1 y1 x2 y2
		uncurry (addColorStop pt) `mapM_` pcs
		pure . CairoPatternTGradient $ CairoPatternGradientTLinear pt
		where [x1, y1, x2, y2] = realToFrac <$> [x1_, y1_, x2_, y2_]
	PatternGradient (GradientFrameRadial c1 c2) pcs -> do
		let	((x1, y1), r1) = circleXyr c1
			((x2, y2), r2) = circleXyr c2
		pt <- cairoPatternCreateRadial x1 y1 r1 x2 y2 r2
		uncurry (addColorStop pt) `mapM_` pcs
		pure . CairoPatternTGradient $ CairoPatternGradientTRadial pt
	PatternMesh pqs -> do
		pt <- M.cairoPatternCreateMesh
		drawPatternQuadrangle pt `mapM_` pqs
		pure $ M.CairoPatternTMesh pt

drawPatternQuadrangle ::
	M.CairoPatternMeshT RealWorld -> PatternQuadrangle -> IO ()
drawPatternQuadrangle pt (PatternQuadrangle pts clrs cps) = do
	let	(mt, lct1, lct2, lct3, ct) = fromMeshPaths pts
		(c0, c1, c2, c3) = fromMeshColors clrs
		(cp0, cp1, cp2, cp3) = fromMeshControlPoints cps
	M.cairoMeshPatternAddPatch
		pt mt lct1 lct2 lct3 ct c0 c1 c2 c3 cp0 cp1 cp2 cp3

circleXyr :: Circle -> ((CDouble, CDouble), CDouble)
circleXyr (Circle ((realToFrac -> x), (realToFrac -> y)) (realToFrac -> r)) =
	((x, y), r)

addColorStop :: IsCairoPatternGradientT pt => pt RealWorld ->
	Double -> SurfaceTypeColor t -> IO ()
addColorStop pt r = \case
	ColorAlpha a -> cairoPatternAddColorStopRgba pt (realToFrac r)
		. fromJust . rgbaDouble 0 0 0 $ realToFrac a
	ColorRgba clr -> cairoPatternAddColorStopRgba pt (realToFrac r)
		(rgbaRealToFrac clr)

fromMeshPaths :: MeshPaths ->
	(P.MoveTo, P.LineCurveTo, P.LineCurveTo, P.LineCurveTo, P.CloseTo)
fromMeshPaths (MeshPaths mt lct1 lct2 lct3 ct) = (
	toMoveTo mt, toLineCurveTo lct1, toLineCurveTo lct2,
	toLineCurveTo lct3, toCloseTo ct )

fromMeshColors :: MeshColors -> (M.Color, M.Color, M.Color, M.Color)
fromMeshColors (MeshColors c0_ c1_ c2_ c3_) = (c0, c1, c2, c3)
	where [c0, c1, c2, c3] = toColor <$> [c0_, c1_, c2_, c3_]

fromMeshControlPoints :: MeshControlPoints ->
	(Maybe M.Point, Maybe M.Point, Maybe M.Point, Maybe M.Point)
fromMeshControlPoints (MeshControlPoints c0_ c1_ c2_ c3_) = (c0, c1, c2, c3)
	where [c0, c1, c2, c3] = (toPoint' <$>) <$> [c0_, c1_, c2_, c3_]

toMoveTo :: MeshMoveTo -> P.MoveTo
toMoveTo (MeshMoveTo (realToFrac -> x) (realToFrac -> y)) = P.MoveTo x y

toLineCurveTo :: MeshLineCurveTo -> P.LineCurveTo
toLineCurveTo (MeshLineTo (realToFrac -> x) (realToFrac -> y)) = P.LineTo x y
toLineCurveTo (MeshCurveTo p1 p2 pe) = P.CurveTo x1 y1 x2 y2 xe ye
	where (x1, y1) = toPoint p1; (x2, y2) = toPoint p2; (xe, ye) = toPoint pe

toCloseTo :: MeshCloseTo -> P.CloseTo
toCloseTo MeshCloseTo = P.CloseLineTo
toCloseTo (MeshCloseCurveTo p1 p2) = P.CloseCurveTo x1 y1 x2 y2
	where (x1, y1) = toPoint p1; (x2, y2) = toPoint p2

toColor :: Rgba Double -> M.Color
toColor = M.ColorRgba . rgbaRealToFrac

toPoint' :: I.Point -> M.Point
toPoint' p = let (x, y) = toPoint p in M.Point x y

makeMaskPattern :: Pattern 'Alpha -> IO (CairoPatternT RealWorld)
makeMaskPattern = \case
	PatternSolid (ColorAlpha a) -> (CairoPatternTSolid <$>)
		. cairoPatternCreateRgba . fromJust . rgbaDouble 0 0 0 $ realToFrac a
	PatternNonSolid pf pe tfm nspt -> do
		t <- transToCairoMatrixT tfm
		pt <- nonSolidPattern nspt
		cairoPatternSet pt $ toCairoFilterT pf
		cairoPatternSet pt $ toCairoExtendT pe
		cairoPatternSetMatrix pt t
		pure pt

toCairoFilterT :: PatternFilter -> CairoFilterT
toCairoFilterT = \case
	PatternFilterFast -> CairoFilterFast
	PatternFilterGood -> CairoFilterGood
	PatternFilterBest -> CairoFilterBest
	PatternFilterNearest -> CairoFilterNearest
	PatternFilterBilinear -> CairoFilterBilinear

toCairoExtendT :: PatternExtend -> CairoExtendT
toCairoExtendT = \case
	PatternExtendNone -> CairoExtendNone
	PatternExtendRepeat -> CairoExtendRepeat
	PatternExtendReflect -> CairoExtendReflect
	PatternExtendPad -> CairoExtendPad

cairoDrawMask :: CairoTIO s -> Mask -> IO ()
cairoDrawMask cr = \case
	MaskAlpha alp -> cairoMask cr =<< makeMaskPattern alp
	MaskPaint alp -> cairoPaintWithAlpha cr $ realToFrac alp
	MaskStroke (I.LineWidth lw) ld lc lj pth -> do
		cairoSet cr . Cr.LineWidth $ realToFrac lw
		cairoSet cr $ toDash ld
		cairoSet cr $ toLineCap lc
		case lj of
			I.LineJoinMiter (realToFrac -> ml) -> do
				cairoSet cr Cr.LineJoinMiter
				cairoSet cr $ MiterLimit ml
			I.LineJoinRound -> cairoSet cr Cr.LineJoinRound
			I.LineJoinBevel -> cairoSet cr Cr.LineJoinBevel
		cairoDrawPaths cr pth >> cairoStroke cr
	MaskFill fr pth -> do
		cairoSet cr $ toFillRule fr
		cairoDrawPaths cr pth >> cairoFill cr
	MaskTextLayout tr l -> do
		cairoTransform cr =<< transToCairoMatrixT tr
		drawLayout cr l

toDash :: LineDash -> Dash
toDash (LineDash ds os) = Dash (realToFrac <$> ds) (realToFrac os)

toLineCap :: I.LineCap -> Cr.LineCap
toLineCap = \case
	I.LineCapButt -> Cr.LineCapButt
	I.LineCapRound -> Cr.LineCapRound
	I.LineCapSquare -> Cr.LineCapSquare

toFillRule :: I.FillRule -> Cr.FillRule
toFillRule = \case
	I.FillRuleWinding -> Cr.FillRuleWinding
	I.FillRuleEvenOdd -> Cr.FillRuleEvenOdd

cairoDrawPaths :: CairoTIO s -> [Path] -> IO ()
cairoDrawPaths cr pths = do
	cairoDrawPath cr `mapM_` pths
	cairoIdentityMatrix cr

cairoDrawPath :: CairoTIO s -> Path -> IO ()
cairoDrawPath cr = \case
	PathTransform tr -> cairoTransform cr =<< transToCairoMatrixT tr
	MoveTo (realToFrac -> x) (realToFrac -> y) -> cairoMoveTo cr x y
	LineTo (realToFrac -> x) (realToFrac -> y) -> cairoLineTo cr x y
	CurveTo (toPoint -> (x1, y1)) (toPoint -> (x2, y2))
		(toPoint -> (xe, ye)) -> cairoCurveTo cr x1 y1 x2 y2 xe ye
	ClosePath -> cairoClosePath cr
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

toPoint :: I.Point -> (CDouble, CDouble)
toPoint (I.Point (realToFrac -> x) (realToFrac -> y)) = (x, y)

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
