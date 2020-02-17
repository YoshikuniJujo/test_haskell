{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TrySelect where

import Control.Monad
import System.Posix.IO

import ServerTools
import Select

sample2 :: IO ()
sample2 = listen3000 >>= \l ->
	withConnection l \conn1 ->
	withConnection l \conn2 -> do
	fds <- select [conn1, conn2] [] [] 60
	putStrLn $ show conn1 ++ " " ++ show conn2 ++ " " ++ show fds
	when (conn1 `elem` fds) $ fdRead conn1 128 >>= print
	when (conn2 `elem` fds) $ fdRead conn2 128 >>= print
	close l
