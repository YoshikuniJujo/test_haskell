{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.ST
import Data.Array
import System.Random

main :: IO ()
main = do
	rs <- take 100 . randomRs (1 :: Int, 1000) <$> getStdGen
	let foo = runST do
		pure ()
	print foo
