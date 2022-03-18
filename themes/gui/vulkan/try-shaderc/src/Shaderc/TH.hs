{-# LANGUAGE TemplateHaskell #-}
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
glslVertexShaderExp = makeShaderExp $ compileGlslVertShader vertSrc

glslVertexShaderDec :: String -> DecsQ
glslVertexShaderDec =
	makeShaderDec "glslVertexShaderMain" $ compileGlslVertShader vertSrc

compileGlslVertShader :: BS.ByteString -> BS.ByteString -> IO BS.ByteString
compileGlslVertShader nm src =
	compileIntoSpv src GlslVertexShader nm "main" defaultCompileOptions

glslFragmentShader :: QuasiQuoter
glslFragmentShader = QuasiQuoter {
	quoteExp = glslFragmentShaderExp,
	quotePat = error "not defined",
	quoteType = error "not defined",
	quoteDec = glslFragmentShaderDec }

fragSrc :: BS.ByteString
fragSrc = "glslFragmentShader"

glslFragmentShaderExp :: String -> ExpQ
glslFragmentShaderExp = makeShaderExp $ compileGlslFragShader fragSrc

glslFragmentShaderDec :: String -> DecsQ
glslFragmentShaderDec =
	makeShaderDec "glslFragmentShaderMain" $ compileGlslFragShader fragSrc

compileGlslFragShader :: BS.ByteString -> BS.ByteString -> IO BS.ByteString
compileGlslFragShader nm src =
	compileIntoSpv src GlslFragmentShader nm "main" defaultCompileOptions

makeShaderExp :: (BS.ByteString -> IO BS.ByteString) -> String -> ExpQ
makeShaderExp cmp src =
	litE . stringL . BSC.unpack =<< runIO (cmp $ BSC.pack src)

makeShaderDec ::
	String -> (BS.ByteString -> IO BS.ByteString) -> String -> DecsQ
makeShaderDec nm cmp src = sequence [
	sigD (mkName nm) (conT ''BS.ByteString),
	valD (varP $ mkName nm) (normalB $ makeShaderExp cmp src) [] ]
