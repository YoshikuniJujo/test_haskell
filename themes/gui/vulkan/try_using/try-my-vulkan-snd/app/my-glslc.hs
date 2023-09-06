{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications, RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import System.Environment
import System.Exit
import System.FilePath
import System.Console.GetOpt

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import qualified Language.SpirV as SpirV
import Language.SpirV.ShaderKind
import Language.SpirV.Shaderc qualified as Shaderc

import qualified Language.SpirV.Shaderc.CompileOptions as CompileOptions

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
	writeFile' df $ Shaderc.compile @()
		src (BSC.pack sf) "main" CompileOptions.C {
			CompileOptions.cMacroDefinitions = [],
			CompileOptions.cSourceLanguage = Nothing,
			CompileOptions.cGenerateDebugInfo = False,
			CompileOptions.cOptimizationLevel = Nothing,
			CompileOptions.cForcedVersionProfile = Nothing,
			CompileOptions.cIncludeCallbacks = Nothing }

writeFile' :: FilePath -> (forall sknd . IsShaderKind sknd => IO (SpirV.S sknd)) -> IO ()
writeFile' fp act = case takeExtension fp of
	".vert" -> SpirV.writeFile fp =<< act @'GlslVertexShader
	".frag" -> SpirV.writeFile fp =<< act @'GlslFragmentShader
	_ -> error "I don't know such extension"

specifyKind :: FilePath -> ShaderKind
specifyKind fp = case takeExtension fp of
	".vert" -> GlslVertexShader
	".frag" -> GlslFragmentShader
	_ -> error "I don't know such extension"
