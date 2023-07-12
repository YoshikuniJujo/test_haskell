{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import qualified Gpu.Vulkan.Memory as Vk.Mem

import qualified Data.ByteString as BS

import Shaderc
import Shaderc.EnumAuto

import qualified Shaderc.CompileOptions as CompileOptions

main :: IO ()
main = BS.writeFile "tmp.spv" . (\(Spv spv) -> spv) =<< compileIntoSpv @() @'GlslVertexShader
	"#version 450\nvoid main() {}"
	"main.vert" "main" CompileOptions.T {
		CompileOptions.tMacroDefinitions = [],
		CompileOptions.tSourceLanguage = Nothing,
		CompileOptions.tGenerateDebugInfo = False,
		CompileOptions.tOptimizationLevel = Nothing,
		CompileOptions.tForcedVersionProfile = Nothing,
		CompileOptions.tIncludeCallbacks = Nothing }
