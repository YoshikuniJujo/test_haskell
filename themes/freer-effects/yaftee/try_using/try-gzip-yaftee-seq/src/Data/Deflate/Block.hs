{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-x-partial #-}

module Data.Deflate.Block (runLengthsToBits) where

import GHC.Stack
import Control.Arrow
import Data.Foldable
import Data.Bits
import Data.Maybe
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Bool
import Data.Gzip.Calc
import Data.Huffman (tableToDict)
import Data.PackageMerge qualified as PackageMerge
import Pipe.Runlength qualified as RunLength

type Bit = Bool

pattern O, I :: Bool
pattern O = False; pattern I = True

runLengthsToBits :: HasCallStack => Bool -> [RunLength.R] -> [Bit]
runLengthsToBits _ [] = []
runLengthsToBits f ((++ [RunLength.EndOfInput]) -> rl) =
	[bool O I f, O, I] ++
	listFromNum 5 (lll - 257) ++ listFromNum 5 (ld - 1) ++
	listFromNum 4 (lt - 4) ++
	hdr ++ (encode dll dd =<< rl)
	where
	(lll, ld, lt, hdr) = mkHeader tll td
	dll = tableToDict tll
	dd = tableToDict td
	tll = PackageMerge.run 14 $ RunLength.toLitLenFreqs rl
	td = PackageMerge.run 14 $ RunLength.toDistFreqs rl

encode :: Map.Map Int [Bit] -> Map.Map Int [Bit] -> RunLength.R -> [Bit]
encode dl _ (RunLength.Literal b) = dl Map.! fromIntegral b
encode dl _ (RunLength.LiteralBS b) = (dl Map.!) . fromIntegral =<< toList b
encode dl dd (RunLength.LenDist l d) = dl Map.! lc ++ le ++ dd Map.! dc ++ de
	where (lc, le) = lengthToCode l; (dc, de) = distToCode d
encode dl _ RunLength.EndOfInput = dl Map.! 256

mkHeader :: HasCallStack => Map.Map Int Int -> Map.Map Int Int -> (Int, Int, Int, [Bit])
mkHeader tll td
	| Map.null tll = error "mkHeader: tll is empty"
	| Map.null td = error "mkHeader: td is empty"
	| otherwise = (
		lnll, lnd, length clcla,
		(=<<) (listFromNum 3) clcla ++
		(=<<) (uncurry (++) . ((tableToDict tcla Map.!) `first`)) cla )
	where
	(clcla :: [Int]) = dropTrailings 4 (== 0)
		$ fromMaybe 0 . (tcla Map.!?) <$> codeLenCodeLenAlphOrder
	tcla = PackageMerge.run 6
		. ((head &&& length) <$>) . L.group . L.sort $ fst <$> cla
	cla = (uncurry codeLenAlph =<<)
		. ((head &&& length) <$>) . L.group $ lll ++ ld
	(lnll, lll) = tableToList 257 tll
	(lnd, ld) = tableToList 1 td

codeLenAlph :: Int -> Int -> [(Int, [Bit])]
codeLenAlph 0 n
	| n < 3 = replicate n (0, [])
	| n < 11 = [(17, listFromNum 3 (n - 3))]
	| n < 139 = [(18, listFromNum 7 (n - 11))]
	| otherwise = (18, [I, I, I, I, I, I, I]) : codeLenAlph 0 (n - 138)
codeLenAlph l n | n < 4 = replicate n (l, [])
codeLenAlph l n = (l, []) : go (n - 1)
	where go m
		| m < 3 = replicate m (l, [])
		| m < 7 = [(16, listFromNum 2 (m - 3))]
		| otherwise = (16, [I, I]) : go (m - 6)

tableToList :: HasCallStack => Int -> Map.Map Int Int -> (Int, [Int])
tableToList m t
	| Map.null t = error "tableToList: empty table"
	| otherwise = (mk + 1, fromMaybe 0 . (t Map.!?) <$> [0 .. mk])
	where mk = m `max` fst (Map.findMax t)

codeLenCodeLenAlphOrder :: [Int]
codeLenCodeLenAlphOrder =
	[16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]

dropTrailings :: Int -> (a -> Bool) -> [a] -> [a]
dropTrailings n p = uncurry (++) . second (go []) . splitAt n
	where go ys = \case
		[] -> []
		x : xs	| p x -> go (x : ys) xs
			| otherwise -> reverse ys ++ [x] ++ go [] xs

listFromNum :: Bits n => Int -> n -> [Bit]
listFromNum ln n = (n `testBit`) <$> [0 .. ln - 1]
