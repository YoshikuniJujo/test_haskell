{-# LANGUAGE BangPatterns #-}

module Main where

import Original
import RewriteSort
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
--	print . last $ {-# SCC "normal_data_original" #-} Original.sort normal
	print . last $ {-# SCC "special_data_original" #-} Original.sort special
--	print . last $ {-# SCC "normal_data_rewrite" #-} RewriteSort.sort normal
	print . last $ {-# SCC "special_data_rewrite" #-} RewriteSort.sort special
