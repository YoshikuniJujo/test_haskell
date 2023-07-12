{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications, RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import qualified Gpu.Vulkan.Memory as Vk.Mem

import Control.Monad
import System.Environment
import System.Exit
import System.FilePath
import System.Console.GetOpt

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Shaderc
import Shaderc.EnumAuto

import qualified Shaderc.CompileOptions as CompileOptions

main :: IO ()
main = do
	(ops, as, es) <- getOpt Permute [outfile] <$> getArgs
	when (not $ null es) do
		putStr `mapM_` es
		exitFailure
	case (as, ops) of
		([sf], [Outfile o]) -> compile sf o
		_ -> putStrLn "USAGE: my-glslc shader.vert -o vert.spv"

data Option = Outfile FilePath deriving Show

outfile :: OptDescr Option
outfile = Option ['o'] [] (ReqArg Outfile "<file>")
	"Place the output into <file>."

compile :: FilePath -> FilePath -> IO ()
compile sf df = do
	src <- BS.readFile sf
	writeFile' df $ compileIntoSpv @()
		src (BSC.pack sf) "main" CompileOptions.T {
			CompileOptions.tMacroDefinitions = [],
			CompileOptions.tSourceLanguage = Nothing,
			CompileOptions.tGenerateDebugInfo = False,
			CompileOptions.tOptimizationLevel = Nothing,
			CompileOptions.tForcedVersionProfile = Nothing,
			CompileOptions.tIncludeCallbacks = Nothing }

writeFile' :: FilePath -> (forall sknd . SpvShaderKind sknd => IO (Spv sknd)) -> IO ()
writeFile' fp act = case takeExtension fp of
	".vert" -> BS.writeFile fp . (\(Spv spv) -> spv) =<< act @'GlslVertexShader
	".frag" -> BS.writeFile fp . (\(Spv spv) -> spv) =<< act @'GlslFragmentShader
	_ -> error "I don't know such extension"

specifyKind :: FilePath -> ShaderKind
specifyKind fp = case takeExtension fp of
	".vert" -> GlslVertexShader
	".frag" -> GlslFragmentShader
	_ -> error "I don't know such extension"
