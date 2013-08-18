{-# LANGUAGE PackageImports #-}

module Main where

import System.Environment
import Eval
import "monads-tf" Control.Monad.State

main :: IO ()
main = do
	fp : _ <- getArgs
	src <- readFile fp
	mapM_ printObject =<<
		(maybe undefined eval $ stoneParse src) `evalStateT` initialEnv
