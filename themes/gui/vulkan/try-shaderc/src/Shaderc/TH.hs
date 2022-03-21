{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Shaderc.TH (glslVertexShader, glslFragmentShader) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Shaderc
import Shaderc.EnumAuto

glslVertexShader :: QuasiQuoter
glslVertexShader = QuasiQuoter {
	quoteExp = glslVertexShaderExp,
	quotePat = error "not defined",
	quoteType = error "not defined",
	quoteDec = glslVertexShaderDec }

vertSrc :: BS.ByteString
vertSrc = "glslVertexShader"

glslVertexShaderExp :: String -> ExpQ
glslVertexShaderExp = makeShaderExp (conT ''Spv `appT` conT 'GlslVertexShader) $ compileGlslVertShader vertSrc

glslVertexShaderDec :: String -> DecsQ
glslVertexShaderDec =
	makeShaderDec "glslVertexShaderMain" (conT ''Spv `appT` conT 'GlslVertexShader) $ compileGlslVertShader vertSrc

compileGlslVertShader :: BS.ByteString -> BS.ByteString -> IO BS.ByteString
compileGlslVertShader nm src = (\(Spv spv :: Spv 'GlslVertexShader) -> spv)
	<$> compileIntoSpv src nm "main" defaultCompileOptions

glslFragmentShader :: QuasiQuoter
glslFragmentShader = QuasiQuoter {
	quoteExp = glslFragmentShaderExp,
	quotePat = error "not defined",
	quoteType = error "not defined",
	quoteDec = glslFragmentShaderDec }

fragSrc :: BS.ByteString
fragSrc = "glslFragmentShader"

glslFragmentShaderExp :: String -> ExpQ
glslFragmentShaderExp = makeShaderExp (conT ''Spv `appT` conT 'GlslFragmentShader) $ compileGlslFragShader fragSrc

glslFragmentShaderDec :: String -> DecsQ
glslFragmentShaderDec =
	makeShaderDec "glslFragmentShaderMain" (conT ''Spv `appT` conT 'GlslFragmentShader) $ compileGlslFragShader fragSrc

compileGlslFragShader :: BS.ByteString -> BS.ByteString -> IO BS.ByteString
compileGlslFragShader nm src = (\(Spv spv :: Spv 'GlslFragmentShader) -> spv)
	<$> compileIntoSpv src nm "main" defaultCompileOptions

makeShaderExp :: TypeQ -> (BS.ByteString -> IO BS.ByteString) -> String -> ExpQ
makeShaderExp typ cmp src =
	(`sigE` typ) . litE . stringL . BSC.unpack =<< runIO (cmp $ BSC.pack src)

makeShaderDec ::
	String -> TypeQ -> (BS.ByteString -> IO BS.ByteString) -> String -> DecsQ
makeShaderDec nm tp cmp src = sequence [
	sigD (mkName nm) tp,
	valD (varP $ mkName nm) (normalB $ makeShaderExp tp cmp src) [] ]
