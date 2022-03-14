{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Control.Monad
import Data.Maybe
import Data.Word
import System.IO
import System.Environment
import System.Exit
import System.Console.GetOpt

import Shaderc.Core

main :: IO ()
main = do
	(ops, as, es) <- getOpt Permute [assembly] <$> getArgs
	when (not $ null es) do
		putStr `mapM_` es
		exitFailure
	let	(into, out) = getPair $ onlyInto ops

	compiler <- c_shaderc_compiler_initialize

	sourceText <- newCString "#version 450\nvoid main() {}"
	inputFileName <- newCString "main.vert"
	entryPointName <- newCString "main"

	result <- into
		compiler sourceText 27 shadercGlslVertexShader
		inputFileName entryPointName nullPtr

	ln <- c_shaderc_result_get_length result
	bt <- c_shaderc_result_get_bytes result
	print ln

	h <- openBinaryFile out WriteMode
	hPutBuf h bt $ fromIntegral ln
	hClose h

	c_shaderc_result_release(result)
	c_shaderc_compiler_release(compiler)

type Run = ShadercCompilerT ->
	Ptr CChar -> Word64 -> Word32 -> CString -> CString ->
	ShadercCompileOptionsT -> IO ShadercCompilationResultT

pairs :: [(Into, (Run, FilePath))]
pairs = [
	(Assembly, (c_shaderc_compile_into_spv_assembly, "tmp.s")),
	(Machine, (c_shaderc_compile_into_spv, "tmp.spv")) ]

data Opt = Into Into deriving Show

data Into = Assembly | Machine deriving (Show, Eq)

onlyInto :: [Opt] -> [Into]
onlyInto [] = []
onlyInto (Into t : os) = t : onlyInto os

getPair :: [Into] -> (Run, FilePath)
getPair [] = (c_shaderc_compile_into_spv, "tmp.spv")
getPair (t : ts) = fromMaybe (getPair ts) $ lookup t pairs

assembly :: OptDescr Opt
assembly = Option ['S'] [] (NoArg $ Into Assembly) "Compile only; do not assembler or link."
