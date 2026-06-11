{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module JSPackage.Compile (compile) where

import Data.Maybe
import Data.Char
import System.Environment
import System.Directory
import System.FilePath
import System.Process

import Hason
import Hason.Eval

compile :: IO ()
compile = do
	dp <- processArgs =<< getArgs
	let	hn = dp </> hasonName (takeBaseName dp)
	Right conf <- eval <$> readFile hn
	let	Seq emds = fromMaybe (Seq []) $ lookup (KStr "exposed-modules") conf
		Seq omds = fromMaybe (Seq []) $ lookup (KStr "other-modules") conf
		mds = (\(Str s) -> dp </> "src" </> s <.> "hs") <$> emds ++ omds
	putStrLn =<< readCreateProcess (proc "javascript-unknown-ghcjs-ghc-9.12.4" mds) ""
	print =<< getDirectoryContents "."

processArgs :: [String] -> IO FilePath
processArgs [] = getCurrentDirectory
processArgs [dp] = (</> dp) <$> getCurrentDirectory
processArgs _ = error "bad"

hasonName :: FilePath -> String
hasonName = (++ ".hason") . capitalize . takeBaseName

capitalize :: String -> String
capitalize "" = ""
capitalize (c : cs) = toUpper c : cs
