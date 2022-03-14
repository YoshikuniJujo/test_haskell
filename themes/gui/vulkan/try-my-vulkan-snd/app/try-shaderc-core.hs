{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.Ptr
import Foreign.C.String
import System.IO

import Shaderc.Core

main :: IO ()
main = do
	compiler <- c_shaderc_compiler_initialize

	sourceText <- newCString "#version 450\nvoid main() {}"
	inputFileName <- newCString "main.vert"
	entryPointName <- newCString "main"

	result <- c_shaderc_compile_into_spv
		compiler sourceText 27 shadercGlslVertexShader
		inputFileName entryPointName nullPtr

	ln <- c_shaderc_result_get_length result
	bt <- c_shaderc_result_get_bytes result
	print ln

	h <- openBinaryFile "tmp.spv" WriteMode
	hPutBuf h bt $ fromIntegral ln
	hClose h

	c_shaderc_result_release(result)
	c_shaderc_compiler_release(compiler)
