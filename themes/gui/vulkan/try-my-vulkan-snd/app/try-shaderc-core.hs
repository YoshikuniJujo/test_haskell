{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
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
import Shaderc.Enum
import Shaderc.EnumAuto.Core
import Shaderc.Include.Core

import qualified Shaderc.CompileOptions.Core as CompileOptions
import qualified Shaderc.CompilationResult.Core as CompilationResult

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

	compiler <- compilerInitialize

	src <- case as of
		[] -> pure "#version 450\nvoid main() {}"
		f : _ -> readFile f

	let srcln = genericLength src

	sourceText <- newCString src
	inputFileName <- newCString "main.vert"
	entryPointName <- newCString "main"

	opts <- CompileOptions.initialize
	foo <- newCString "FOO"
	bar <- newCString "BAR"
	CompileOptions.addMacroDefinition opts foo 3 bar 3
	when (elem Hlsl $ onlyLanguage ops)
		$ CompileOptions.setSourceLanguage
			opts SourceLanguageHlsl
	when (elem DebugInfo ops)
		$ CompileOptions.setGenerateDebugInfo opts
	case onlyOptimizationLevel ops of
		[] -> pure ()
		ols -> CompileOptions.setOptimizationLevel opts $ last ols
	CompileOptions.setIncludeCallbacks
		opts resolveFun resultReleaseFun nullPtr

	rslt <- into
		compiler sourceText srcln GlslVertexShader
		inputFileName entryPointName opts

	ln <- CompilationResult.getLength rslt
	bt <- CompilationResult.getBytes rslt
	print ln

	putStr =<< peekCString =<< CompilationResult.getErrorMessage rslt

	h <- openBinaryFile out WriteMode
	hPutBuf h bt $ fromIntegral ln
	hClose h

	CompilationResult.release rslt
	CompileOptions.release opts
	compilerRelease compiler

type Run = CompilerT ->
	Ptr CChar -> Word64 -> ShaderKind -> CString -> CString ->
	CompileOptions.T -> IO CompilationResult.T

pairs :: [(Into, (Run, FilePath))]
pairs = [
	(Assembly, (compileIntoSpvAssembly, "tmp.s")),
	(Machine, (compileIntoSpv, "tmp.spv")),
	(PreprocessedText, (compileIntoPreprocessedText, "tmp.txt")) ]

data Opt
	= Into Into
	| Language Language
	| DebugInfo
	| OptimizationLevelOpt OptimizationLevel
	deriving (Show, Eq)

data Into = Assembly | Machine | PreprocessedText deriving (Show, Eq)
data Language = Glsl | Hlsl deriving (Show, Eq)

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
onlyOptimizationLevel (OptimizationLevelOpt ol : os) =
	ol : onlyOptimizationLevel os
onlyOptimizationLevel (_ : os) = onlyOptimizationLevel os

getPair :: [Into] -> (Run, FilePath)
getPair [] = (compileIntoSpv, "tmp.spv")
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
	(ReqArg (OptimizationLevelOpt . optimizationLevelFromStr)
		"Optimization level")
	"Optimization"

resolveFun :: ResolveFn
resolveFun ud cs1 tp cs2 nm = do
	print ud
	putStrLn =<< peekCString cs1
	print tp
	putStrLn =<< peekCString cs2
	print nm
	p <- malloc
	poke p $ Result nullPtr 0 nullPtr 0 nullPtr
	pure p

resultReleaseFun :: ResultReleaseFn
resultReleaseFun ud rslt = do
	print ud
	free rslt
