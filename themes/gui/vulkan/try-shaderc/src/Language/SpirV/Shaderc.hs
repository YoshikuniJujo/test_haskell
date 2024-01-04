{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Language.SpirV.Shaderc (
	compile, SourceText, InputFileName, EntryPointName ) where

import Foreign.Storable
import Control.Monad.Trans
import Control.Monad.Cont

import qualified Data.ByteString as BS

import Language.SpirV.Internal
import Language.SpirV.ShaderKind
import Language.SpirV.Shaderc.Exception.Internal

import qualified Shaderc.Core as C
import qualified Shaderc.Middle as M
import qualified Language.SpirV.Shaderc.CompileOptions as CompileOptions
import qualified Shaderc.CompilationResult.Core as CompilationResult

compile :: forall ud sknd . (Storable ud, IsShaderKind sknd) =>
	SourceText -> InputFileName -> EntryPointName ->
	CompileOptions.C ud -> IO (S sknd)
compile src ifnm epnm opts = ($ pure) $ runContT do
	cmp <- lift C.compilerInitialize
	rslt <- M.compileIntoSpv cmp src (shaderKind @sknd) ifnm epnm opts
	lift $ throwUnlessSuccess rslt
	lift do	cspv <- CompilationResult.getBytes rslt
		(fromIntegral -> cspvln) <- CompilationResult.getLength rslt
		spv <- BS.packCStringLen (cspv, cspvln)
		CompilationResult.release rslt
		C.compilerRelease cmp
		pure $ S spv

type SourceText = BS.ByteString
type InputFileName = BS.ByteString
type EntryPointName = BS.ByteString
