{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Language.SpirV.Shaderc.TH (
	glslVertexShader, glslFragmentShader, glslComputeShader ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Data.Default
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Language.SpirV.Internal
import Language.SpirV.ShaderKind
import Language.SpirV.Shaderc
import Language.SpirV.Shaderc.CompileOptions qualified as CompileOptions

-- GLSL VERTEX SHADER

glslVertexShader :: QuasiQuoter
glslVertexShader = generalShader @'GlslVertexShader
	"glslVertexShaderMain" 'GlslVertexShader "glslVertexShader"

-- GLSL FRAGMENT SHADER

glslFragmentShader :: QuasiQuoter
glslFragmentShader = generalShader @'GlslFragmentShader
	"glslFragmentShaderMain" 'GlslFragmentShader "glslFragmentShader"

-- GLSL COMPUTE SHADER

glslComputeShader :: QuasiQuoter
glslComputeShader = generalShader @'GlslComputeShader
	"glslComputeShaderMain" 'GlslComputeShader "glslComputeShader"

-- GENERAL

generalShader :: forall shtp . IsShaderKind shtp => String -> Name -> BS.ByteString -> QuasiQuoter
generalShader var shtp shnm = QuasiQuoter {
	quoteExp = generalShaderExp @shtp shtp shnm,
	quotePat = error "not defined",
	quoteType = error "not defined",
	quoteDec = generalShaderDec @shtp var shtp shnm }

generalShaderExp :: forall shtp . IsShaderKind shtp => Name -> BS.ByteString -> String -> ExpQ
generalShaderExp shtp shnm =
	mkShaderExp (conT ''S `appT` conT shtp) $ compileGeneralShader @shtp shnm

generalShaderDec :: forall shtp . IsShaderKind shtp => String -> Name -> BS.ByteString -> String -> DecsQ
generalShaderDec var shtp shnm =
	mkShaderDec var (conT ''S `appT` conT shtp) $ compileGeneralShader @shtp shnm

compileGeneralShader :: forall shtp . IsShaderKind shtp =>  BS.ByteString -> BS.ByteString -> IO BS.ByteString
compileGeneralShader nm src = (\(S spv :: S shtp) -> spv)
	<$> compile src nm "main" (def :: CompileOptions.C ())

-- BASE

mkShaderExp :: TypeQ -> (BS.ByteString -> IO BS.ByteString) -> String -> ExpQ
mkShaderExp typ cmp src =
	(`sigE` typ) . litE . stringL . BSC.unpack =<< runIO (cmp $ BSC.pack src)

mkShaderDec ::
	String -> TypeQ -> (BS.ByteString -> IO BS.ByteString) -> String -> DecsQ
mkShaderDec nm tp cmp src = sequence [
	sigD (mkName nm) tp,
	valD (varP $ mkName nm) (normalB $ mkShaderExp tp cmp src) [] ]
