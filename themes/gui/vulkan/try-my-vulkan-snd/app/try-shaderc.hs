{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import qualified Data.ByteString as BS

import Shaderc
import Shaderc.EnumAuto

import qualified Shaderc.CompileOptions as CompileOptions

main :: IO ()
main = BS.writeFile "tmp.spv" =<< compileIntoSpv @()
	"#version 450\nvoid main() {}" GlslVertexShader
	"main.vert" "main" CompileOptions.T {
		CompileOptions.tMacroDefinitions = [],
		CompileOptions.tSourceLanguage = Nothing,
		CompileOptions.tGenerateDebugInfo = False,
		CompileOptions.tOptimizationLevel = Nothing,
		CompileOptions.tForcedVersionProfile = Nothing,
		CompileOptions.tIncludeCallbacks = Nothing }
