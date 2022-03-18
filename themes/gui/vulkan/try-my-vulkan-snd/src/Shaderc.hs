{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Shaderc where

import Foreign.Storable
import Control.Monad.Cont

import qualified Data.ByteString as BS

import Shaderc.EnumAuto
import Shaderc.Exception

import qualified Shaderc.Core as C
import qualified Shaderc.Middle as M
import qualified Shaderc.CompileOptions as CompileOptions
import qualified Shaderc.CompilationResult.Core as CompilationResult

compileIntoSpv :: Storable ud =>
	BS.ByteString -> ShaderKind -> BS.ByteString -> BS.ByteString ->
	CompileOptions.T ud -> IO BS.ByteString
compileIntoSpv src knd ifnm epnm opts = ($ pure) $ runContT do
	cmp <- lift C.compilerInitialize
	rslt <- M.compileIntoSpv cmp src knd ifnm epnm opts
	lift $ throwUnlessSuccess rslt
	lift do	cspv <- CompilationResult.getBytes rslt
		(fromIntegral -> cspvln) <- CompilationResult.getLength rslt
		spv <- BS.packCStringLen (cspv, cspvln)
		CompilationResult.release rslt
		C.compilerRelease cmp
		pure spv

defaultCompileOptions :: CompileOptions.T ()
defaultCompileOptions = CompileOptions.T {
	CompileOptions.tMacroDefinitions = [],
	CompileOptions.tSourceLanguage = Nothing,
	CompileOptions.tGenerateDebugInfo = False,
	CompileOptions.tOptimizationLevel = Nothing,
	CompileOptions.tForcedVersionProfile = Nothing,
	CompileOptions.tIncludeCallbacks = Nothing }
