{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Shaderc.CompileOptions where

import Shaderc.Core

import qualified Shaderc.CompileOptions.Core as C

data C = C {
	cMacroDefinition :: [(String, String)],
	cSourceLanguage :: SourceLanguage,
	cGenerateDebugInfo :: Bool,
	cOptimizationLevel :: OptimizationLevel,
	cForcedVersionProfile :: (Version, Profile)
	}
	deriving Show
