{-# LANGUAGE ViewPatterns #-}
{-# OPTIOnS_GHC -Wall -fno-warn-tabs #-}

module Shaderc.Middle where

import Foreign.Storable
import Control.Monad.Cont

import qualified Data.ByteString as BS

import Shaderc.EnumAuto

import qualified Shaderc.Core as C
import qualified Shaderc.CompileOptions as CompileOptions
import qualified Shaderc.CompilationResult.Core as CompilationResult

compileIntoSpv :: Storable ud =>
	C.CompilerT -> BS.ByteString -> ShaderKind ->
	BS.ByteString -> BS.ByteString -> CompileOptions.T ud ->
	ContT r IO CompilationResult.T
compileIntoSpv cmp src knd ifnm epnm opts = do
	(csrc, fromIntegral -> csrcln) <- ContT $ BS.useAsCStringLen src
	cifnm <- ContT $ BS.useAsCString ifnm
	cepnm <- ContT $ BS.useAsCString epnm
	copts <- CompileOptions.tToCore opts
	lift $ C.compileIntoSpv cmp csrc csrcln knd cifnm cepnm copts
