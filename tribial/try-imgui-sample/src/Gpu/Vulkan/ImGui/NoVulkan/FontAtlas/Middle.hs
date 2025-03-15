{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.NoVulkan.FontAtlas.Middle where

import Foreign.Marshal.Array
import System.IO.Unsafe

import Gpu.Vulkan.ImGui.NoVulkan.FontAtlas.Core qualified as C

newtype F = F C.F deriving Show

getGlyphRangesJapanese :: F -> [C.ImWChar]
getGlyphRangesJapanese (F fa) = unsafePerformIO $ do
	let	pgr = C.getGlyphRangesJapanese fa
	peekArray0 0 pgr
