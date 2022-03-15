{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Shaderc.CompileOptions where

import Shaderc.Core

data CompileOptions = CompileOptions {
	macroDefinition :: [(String, String)],
	sourceLanguage :: SourceLanguage,
	generateDebugInfo :: Bool,
	optimizationLevel :: OptimizationLevel
	}
	deriving Show
