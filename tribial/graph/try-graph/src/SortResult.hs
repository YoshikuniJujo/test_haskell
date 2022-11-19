{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SortResult (All, readAll, machin, header, dat) where

import Foreign.C.Types
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Word

data All = All {
	machin :: Maybe (Word64, Word64),
	header :: [String],
	dat :: [[(CDouble, CDouble)]] } deriving Show

readAll :: String -> All
readAll = All <$> const Nothing <*> readHeader <*> readData

readHeader :: String -> [String]
readHeader = NE.toList . spans (/= '/') . (!! 1) . words . head . lines

readData :: String -> [[(CDouble, CDouble)]]
readData = L.transpose . (separate . readData1 <$>) . tail . lines

separate :: (a, [a]) -> [(a, a)]
separate (x, ys) = (x ,) <$> ys

readData1 :: String -> (CDouble, [CDouble])
readData1 str = case span (/= '\t') str of
	(n_, '\t' : dt) -> let
		n = read n_
		tr = (/ (n * log n)) . read . takeWhile (/= 's') in
		case spans (/= '/') dt of ds -> (n, NE.toList $ tr <$> ds)
	_ -> error "bad format"

spans :: (a -> Bool) -> [a] -> NE.NonEmpty [a]
spans p = \case
	[] -> NE.singleton []
	x : xs	| p x -> (x : y) NE.:| ys
		| otherwise -> [] NE.<| ya
		where ya@(y NE.:| ys) = spans p xs
