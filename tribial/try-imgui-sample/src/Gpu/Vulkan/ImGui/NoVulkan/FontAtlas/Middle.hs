{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.NoVulkan.FontAtlas.Middle where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.C.String
import System.IO.Unsafe

import Gpu.Vulkan.ImGui.NoVulkan.Font.Middle qualified as Font
import Gpu.Vulkan.ImGui.NoVulkan.FontConfig.Middle qualified as FontConfig
import Gpu.Vulkan.ImGui.NoVulkan.FontConfigNew.Middle qualified as FontConfigNew

import Gpu.Vulkan.ImGui.NoVulkan.FontAtlas.Core qualified as C

newtype F = F C.F deriving Show

getGlyphRangesJapanese :: F -> FontConfig.GlyphRanges
getGlyphRangesJapanese (F fa) = FontConfig.glyphRanges . unsafePerformIO $ do
	let	pgr = C.getGlyphRangesJapanese fa
	peekArray0 0 pgr

addFontFromFileTtf :: F -> FilePath -> Float ->
	Maybe FontConfigNew.F -> Maybe FontConfig.GlyphRanges -> IO Font.F
addFontFromFileTtf (F fa) fp psz mffc mgrs = Font.F <$>
	withCString fp \cfp -> case mgrs of
		Just grs -> allocaArray0 (length grws) \pgrs -> do
			pokeArray0 0 pgrs grws
			C.addFontFromFileTtf fa cfp psz fc pgrs
			where
			grws = FontConfig.glyphRangesToWords grs
		Nothing -> C.addFontFromFileTtf fa cfp psz fc nullPtr
	where
	fc = case mffc of Just (FontConfigNew.F f) -> f; Nothing -> nullPtr
