{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UnboxedTuples, MagicHash, BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools.ShowLazyList (showLazyList, showTipes, ones, fibs) where

import GHC.Exts.Heap
import Data.Bool
import System.IO.Unsafe

showLazyList :: Show a => [a] -> String
showLazyList xs = unsafePerformIO . (<$> isEvaluated xs) . bool "?" $ case xs of
	[] -> "[]"
	x : xs' -> show x ++ " : " ++ showLazyList xs'

isEvaluated :: a -> IO Bool
isEvaluated x = (<$> (tipe . info <$> getClosureData x)) $ \case
	BLACKHOLE -> True
	CONSTR -> True
	CONSTR_1_0 -> True; CONSTR_0_1 -> True
	CONSTR_2_0 -> True; CONSTR_1_1 -> True; CONSTR_0_2 -> True
	_ -> False

showTipes :: Int -> [a] -> IO ()
showTipes n _ | n < 1 = return ()
showTipes n xs = do
	print . tipe . info =<< getClosureData xs
	case xs of [] -> return (); _ : xs' -> showTipes (n - 1) xs'

ones :: [Int]
ones = 1 : ones

fibs, tfibs :: [Integer]
fibs@(_ : tfibs) = 0 : 1 : zipWith (+) fibs tfibs
