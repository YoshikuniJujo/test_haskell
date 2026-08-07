{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Monad.State
import System.Environment
import System.Random qualified as R

import Tools

main :: IO ()
main = do
	[bs, n] <- getArgs
	(uncurry (writeFile . fileNameN bs "password") `mapM_`) . ([0 ..] `zip`)
		. fst $ replicateM (read n) password `runState` R.mkStdGen 8
