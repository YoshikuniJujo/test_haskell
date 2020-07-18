{-# LANGUAGE BangPatterns #-}

module Main where

import Data.List
import ForProfile

import Control.DeepSeq
import Control.Exception

main :: IO ()
main = do
	let	!normal = sampleNormal_N 100
		!special = sampleX_N 100
	evaluate $ rnf normal
	evaluate $ rnf special
	evaluate $ normal `deepseq` ()
	evaluate $ special `deepseq` ()
	print $ last normal
	print $ last special
	print . last $ {-# SCC "normal_data" #-} sort normal
	print . last $ {-# SCC "special_data" #-} sort special
