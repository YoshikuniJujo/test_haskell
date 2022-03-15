{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import System.Environment
import System.Exit
import System.Console.GetOpt

main :: IO ()
main = do
	(ops, as, es) <- getOpt Permute [outfile] <$> getArgs
	when (not $ null es) do
		putStr `mapM_` es
		exitFailure
	case ops of
		[Outfile o] -> putStrLn o
		_ -> putStrLn "USAGE: my-glslc shader.vert -o vert.spv"

data Option = Outfile FilePath deriving Show

outfile :: OptDescr Option
outfile = Option ['o'] [] (ReqArg Outfile "<file>")
	"Place the output into <file>."
