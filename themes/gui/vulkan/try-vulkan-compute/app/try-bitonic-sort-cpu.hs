{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.ST
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Data.Word
import System.Random

main :: IO ()
main = do
	rs <- take 128 . randomRs (1 :: Word32, 1000) <$> getStdGen
	let foo = runST $ sort rs
	print foo

sort :: forall s . [Word32] -> ST s (Array Word32 Word32)
sort rs = do
	arr <- newListArray (0, 127) rs
	freeze (arr :: STArray s Word32 Word32) :: ST s (Array Word32 Word32)
