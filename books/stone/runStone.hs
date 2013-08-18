{-# LANGUAGE PackageImports #-}

module Main where

import System.Environment
import Eval
import "monads-tf" Control.Monad.State

main :: IO ()
main = do
	fp : as <- getArgs
	src <- readFile fp
	let po = case as of
		"-v" : _ -> printObject
		_ -> const $ return ()
	mapM_ po =<<
		(maybe undefined eval $ stoneParse src) `evalStateT` initialEnv
