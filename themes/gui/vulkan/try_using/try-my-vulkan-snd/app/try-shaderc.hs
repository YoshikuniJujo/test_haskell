{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import qualified Language.SpirV as SpirV
import Language.SpirV.ShaderKind

import qualified Language.SpirV.Shaderc.CompileOptions as CompileOptions

import Language.SpirV.Shaderc

main :: IO ()
main = SpirV.writeFile "tmp.spv" =<< compile @() @'GlslVertexShader
	"#version 450\nvoid main() {}"
	"main.vert" "main" CompileOptions.C {
		CompileOptions.cMacroDefinitions = [],
		CompileOptions.cSourceLanguage = Nothing,
		CompileOptions.cGenerateDebugInfo = False,
		CompileOptions.cOptimizationLevel = Nothing,
		CompileOptions.cForcedVersionProfile = Nothing,
		CompileOptions.cIncludeCallbacks = Nothing }
