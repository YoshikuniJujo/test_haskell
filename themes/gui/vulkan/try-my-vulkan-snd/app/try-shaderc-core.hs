{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Control.Monad
import Data.Maybe
import Data.List
import Data.Word
import System.IO
import System.Environment
import System.Exit
import System.Console.GetOpt

import Shaderc.Core

main :: IO ()
main = do
	(ops, as, es) <- getOpt Permute [
		assembly, hlsl, debugInfo, optimizationLevel, preprocessedText
		] <$> getArgs
	when (not $ null es) do
		putStr `mapM_` es
		exitFailure
	print ops
	print $ onlyLanguage ops
	print $ onlyOptimizationLevel ops
	print as
	let	(into, out) = getPair $ onlyInto ops

	compiler <- c_shaderc_compiler_initialize

	src <- case as of
		[] -> pure "#version 450\nvoid main() {}"
		f : _ -> readFile f

	let srcln = genericLength src

	sourceText <- newCString src
	inputFileName <- newCString "main.vert"
	entryPointName <- newCString "main"

	opts <- c_shaderc_compile_options_initialize
	foo <- newCString "FOO"
	bar <- newCString "BAR"
	c_shaderc_compile_options_add_macro_definition opts foo 3 bar 3
	when (elem Hlsl $ onlyLanguage ops)
		$ c_shaderc_compile_options_set_source_language
			opts shadercSourceLanguageHlsl
	when (elem DebugInfo ops)
		$ c_shaderc_compile_options_set_generate_debug_info opts
	case onlyOptimizationLevel ops of
		[] -> pure ()
		ols -> c_shaderc_compile_options_set_optimization_level opts
			case last ols of
				OptimizationLevelZero ->
					shadercOptimizationLevelZero
				OptimizationLevelSize ->
					shadercOptimizationLevelSize
				OptimizationLevelPerformance ->
					shadercOptimizationLevelPerformance

	result <- into
		compiler sourceText srcln shadercGlslVertexShader
		inputFileName entryPointName opts

	ln <- c_shaderc_result_get_length result
	bt <- c_shaderc_result_get_bytes result
	print ln

	putStr =<< peekCString =<< c_shaderc_result_get_error_message result

	h <- openBinaryFile out WriteMode
	hPutBuf h bt $ fromIntegral ln
	hClose h

	c_shaderc_result_release(result)
	c_shaderc_compile_options_release opts
	c_shaderc_compiler_release(compiler)

type Run = ShadercCompilerT ->
	Ptr CChar -> Word64 -> Word32 -> CString -> CString ->
	ShadercCompileOptionsT -> IO ShadercCompilationResultT

pairs :: [(Into, (Run, FilePath))]
pairs = [
	(Assembly, (c_shaderc_compile_into_spv_assembly, "tmp.s")),
	(Machine, (c_shaderc_compile_into_spv, "tmp.spv")),
	(PreprocessedText, (c_shaderc_compile_into_preprocessed_text, "tmp.txt"))]

data Opt
	= Into Into
	| Language Language
	| DebugInfo
	| OptimizationLevel OptimizationLevel
	deriving (Show, Eq)

data Into = Assembly | Machine | PreprocessedText deriving (Show, Eq)
data Language = Glsl | Hlsl deriving (Show, Eq)

data OptimizationLevel
	= OptimizationLevelZero
	| OptimizationLevelSize
	| OptimizationLevelPerformance
	deriving (Show, Eq)

optimizationLevelFromStr :: String -> OptimizationLevel
optimizationLevelFromStr "zero" = OptimizationLevelZero
optimizationLevelFromStr "size" = OptimizationLevelSize
optimizationLevelFromStr "performance" = OptimizationLevelPerformance
optimizationLevelFromStr _ = OptimizationLevelZero

onlyInto :: [Opt] -> [Into]
onlyInto [] = []
onlyInto (Into t : os) = t : onlyInto os
onlyInto (_ : os) = onlyInto os

onlyLanguage :: [Opt] -> [Language]
onlyLanguage [] = []
onlyLanguage (Language l : os) = l : onlyLanguage os
onlyLanguage (_ : os) = onlyLanguage os

onlyOptimizationLevel :: [Opt] -> [OptimizationLevel]
onlyOptimizationLevel [] = []
onlyOptimizationLevel (OptimizationLevel ol : os) =
	ol : onlyOptimizationLevel os
onlyOptimizationLevel (_ : os) = onlyOptimizationLevel os

getPair :: [Into] -> (Run, FilePath)
getPair [] = (c_shaderc_compile_into_spv, "tmp.spv")
getPair (t : ts) = fromMaybe (getPair ts) $ lookup t pairs

assembly :: OptDescr Opt
assembly = Option ['S'] [] (NoArg $ Into Assembly) "Compile only; do not assembler or link."

preprocessedText :: OptDescr Opt
preprocessedText = Option ['E'] [] (NoArg $ Into PreprocessedText) "Only preprocess"

hlsl :: OptDescr Opt
hlsl = Option [] ["hlsl"] (NoArg $ Language Hlsl) "HLSL"

debugInfo :: OptDescr Opt
debugInfo = Option [] ["debug"] (NoArg DebugInfo) "Debug info"

optimizationLevel :: OptDescr Opt
optimizationLevel = Option [] ["opt"]
	(ReqArg (OptimizationLevel . optimizationLevelFromStr)
		"Optimization level")
	"Optimization"
