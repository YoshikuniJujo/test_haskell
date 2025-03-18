{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.NoVulkan.FontConfig.Middle where

import Foreign.Ptr
import Foreign.Marshal.Array
import Data.Bool
import Data.Word
import Data.Int
import Data.ByteString qualified as BS

import Gpu.Vulkan.ImGui.NoVulkan.Font.Middle qualified as Font

import Gpu.Vulkan.ImGui.NoVulkan.FontConfig.Core qualified as C

data FC = FC {
	fCFontData :: Maybe BS.ByteString,
	fCFontDataOwnedByAtlas :: Bool,
	fCMergeMode :: Bool,
	fCPixelSnapH :: Bool,
	fCFontNo :: Int32,
	fCOversampleH :: Int32,
	fCOversampleV :: Int32,
	fCSizePixels :: Float,
	fCGlyphOffset :: (Float, Float),
	fCGlyphRanges :: GlyphRanges,
	fCGlyphMinAdvanceX :: Float,
	fCGlyphMaxAdvanceX :: Float,
	fCGlyphExtraAdvanceX :: Float,
	fCFontBuilderFlags :: Word32,
	fCRasterizerMultiply :: Float,
	fCRasterizerDensity :: Float,
	fCEllipsisChar :: Word16,
	fCName :: BS.ByteString,
	fCDstFont :: Maybe Font.F }
	deriving Show

type GlyphRanges = [GlyphRange]
data GlyphRange = GlyphRange Word16 Word16 | GlyphList [Word16] deriving Show

fcFromCore :: C.FC -> IO FC
fcFromCore C.FC {
	C.fCFontData = pdt,
	C.fCFontDataSize = fromIntegral -> dtsz,
	C.fCFontDataOwnedByAtlas = dtoba,
	C.fCMergeMode = mm,
	C.fCPixelSnapH = psh,
	C.fCFontNo = fn,
	C.fCOversampleH = osh,
	C.fCOversampleV = osv,
	C.fCSizePixels = sps,
	C.fCGlyphOffsetX = gox,
	C.fCGlyphOffsetY = goy,
	C.fCGlyphRanges = pgrs,
	C.fCGlyphMinAdvanceX = gmnax,
	C.fCGlyphMaxAdvanceX = gmxax,
	C.fCGlyphExtraAdvanceX = gexax,
	C.fCFontBuilderFlags = bfs,
	C.fCRasterizerMultiply = rm,
	C.fCRasterizerDensity = rd,
	C.fCEllipsisChar = ec,
	C.fCName = nm,
	C.fCDstFont = df
	} = do
	dt <- bool (pure Nothing)
		(Just <$> BS.packCStringLen (pdt, dtsz)) (pdt /= nullPtr)
	grs <- peekArray0 0 pgrs
	pure FC {
	fCFontData = dt,
	fCFontDataOwnedByAtlas = dtoba /= 0,
	fCMergeMode = mm /= 0,
	fCPixelSnapH = psh /= 0,
	fCFontNo = fn,
	fCOversampleH = osh,
	fCOversampleV = osv,
	fCSizePixels = sps,
	fCGlyphOffset = (gox, goy),
	fCGlyphRanges = glyphRanges grs,
	fCGlyphMinAdvanceX = gmnax,
	fCGlyphMaxAdvanceX = gmxax,
	fCGlyphExtraAdvanceX = gexax,
	fCFontBuilderFlags = bfs,
	fCRasterizerMultiply = rm,
	fCRasterizerDensity = rd,
	fCEllipsisChar = ec,
	fCName = nm,
	fCDstFont = bool Nothing (Just $ Font.F df) $ df /= nullPtr }

glyphRanges :: [Word16] -> GlyphRanges
glyphRanges = glyphRangesFromList (GlyphList []) . glyphRangesToList . glyphRangesFromPairs

glyphRangesFromPairs :: [Word16] -> GlyphRanges
glyphRangesFromPairs [] = []
glyphRangesFromPairs (g1 : g2 : gs) = GlyphRange g1 g2 : glyphRanges gs
glyphRangesFromPairs _ = error "bad"

glyphRangesToList :: GlyphRanges -> [Word16]
glyphRangesToList [] = []
glyphRangesToList (GlyphRange g1 g2 : grs) = [g1 .. g2] ++ glyphRangesToList grs
glyphRangesToList (GlyphList gs : grs) = gs ++ glyphRangesToList grs

glyphRangesFromList :: GlyphRange -> [Word16] -> GlyphRanges
glyphRangesFromList (GlyphList []) [] = []
glyphRangesFromList r [] = [r]
glyphRangesFromList (GlyphList []) (w0 : ws) =
	glyphRangesFromList (GlyphRange w0 w0) ws
glyphRangesFromList (GlyphRange wf wt) wa@(w0 : ws)
	| wt + 1 == w0 = glyphRangesFromList (GlyphRange wf w0) ws
--	| wf == wt = GlyphList [wf] : glyphRangesFromList (GlyphList []) wa
	| wf == wt = glyphRangesFromList (GlyphList [wf]) wa
	| otherwise = GlyphRange wf wt : glyphRangesFromList (GlyphList []) wa
glyphRangesFromList (GlyphList gs) (w0 : ws)
	| last gs + 1 == w0 =
		GlyphList (init gs) : glyphRangesFromList (GlyphRange (last gs) w0) ws
	| otherwise = glyphRangesFromList (GlyphList $ gs ++ [w0]) ws
