{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Shaderc.CompileOptions where

import Shaderc.Enum
import Shaderc.Include

import qualified Shaderc.CompileOptions.Core as C

data C ud = C {
	cMacroDefinition :: [(String, String)],
	cSourceLanguage :: SourceLanguage,
	cGenerateDebugInfo :: Bool,
	cOptimizationLevel :: OptimizationLevel,
	cForcedVersionProfile :: (Version, Profile),
	cIncludeCallbacks :: (ResolveFn ud, Maybe ud) }
