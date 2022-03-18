{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Shaderc.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Shaderc
import Shaderc.EnumAuto

test :: QuasiQuoter
test = QuasiQuoter {
	quoteExp = litE . stringL,
	quotePat = error "not defined",
	quoteType = error "not defined",
	quoteDec = error "not defined" }

glslVertexShader :: QuasiQuoter
glslVertexShader = QuasiQuoter {
	quoteExp = \src -> litE . stringL . BSC.unpack =<< runIO
		(compileGlslVertexShader "glslVertexShader" $ BSC.pack src)
	}

compileGlslVertexShader :: BS.ByteString -> BS.ByteString -> IO BS.ByteString
compileGlslVertexShader nm src =
	compileIntoSpv src GlslVertexShader nm "main" defaultCompileOptions
