{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment

import Lib

main :: IO ()
main = do
	cmd : args <- getArgs
	case cmd of
		"divide" -> case args of
			[pwd] -> (\(d1, d2) -> mapM_ putStrLn [d1, d2])
				=<< divideBase58 pwd
			_ -> error "bad args"
		"unify" -> case args of
			[d1, d2] -> maybe undefined putStrLn $ xorBase58 d1 d2
			_ -> error "bad args"
		_ -> error "no such command"
