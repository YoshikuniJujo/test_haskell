{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-x-partial #-}

module Yaftee.UseFTCQ.Pipe.Gzip.Compress.Block (runLengthsToBits) where

import Control.Arrow
import Data.Maybe
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Bool
import Data.ByteString qualified as BS
import Data.Bit (pattern O, pattern I)
import Data.Bit qualified as Bit
import Data.Calc
import Data.HuffmanTree (tableToDict)
import Data.PackageMerge qualified as PackageMerge
import Yaftee.UseFTCQ.Pipe.Gzip.RunLength qualified as RunLength
import Yaftee.UseFTCQ.Pipe.Bits qualified as PipeBits

runLengthsToBits :: Bool -> [RunLength.R] -> [Bit.B]
runLengthsToBits _ [] = []
runLengthsToBits f ((++ [RunLength.EndOfInput]) -> rl) =
	[bool O I f, O, I] ++
	PipeBits.listFromNum 5 (lll - 257) ++ PipeBits.listFromNum 5 (ld - 1) ++
	PipeBits.listFromNum 4 (lt - 4) ++
	hdr ++ (encode dll dd =<< rl)
	where
	(lll, ld, lt, hdr) = mkHeader tll td
	dll = tableToDict tll
	dd = tableToDict td
	tll = PackageMerge.run 14 $ RunLength.toLitLenFreqs rl
	td = PackageMerge.run 14 $ RunLength.toDistFreqs rl

mkHeader :: Map.Map Int Int -> Map.Map Int Int -> (Int, Int, Int, [Bit.B])
mkHeader tll md = (lll, ld, lt, ttbs ++ mkBits aes)
	where
	mkBits = ((\(alp, ebs) -> tableToDict foobar Map.! alp ++ ebs) =<<)
	ttbs = PipeBits.listFromNum 3 =<< (tt :: [Int])
	(lt, tt) = tableTableToOrder foobar
	foobar = PackageMerge.run 6
		. ((head &&& length) <$>) . L.group . L.sort $ fst <$> aes
	aes = barToCodes tlld
	((lll, ld), tlld) = mapMapToLitLenDstList tll md
	barToCodes :: [Int] -> [(Int, [Bit.B])]
	barToCodes = (uncurry lenToCodes =<<) . ((head &&& length) <$>) . L.group

tableTableToOrder :: Map.Map Int Int -> (Int, [Int])
tableTableToOrder tt = (ln, take ln o)
	where
	ln = 4 `max` length o
	o = dropTrailing0 $ ttto tt
	dropTrailing0 :: [Int] -> [Int]
	dropTrailing0 = reverse . dropWhile (== 0) . reverse
	ttto :: Num b => Map.Map Int b -> [b]
	ttto m = fromMaybe 0 . (m Map.!?) <$> (tableTableLenOrder :: [Int])
	tableTableLenOrder :: [Int]
	tableTableLenOrder =
		[16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]

mapMapToLitLenDstList :: Map.Map Int Int -> Map.Map Int Int -> ((Int, Int), [Int])
mapMapToLitLenDstList rll rd = let
	(lnll, tll) = fromJust $ huffMapToList 257 rll
	(lnd, td) = fromJust $ huffMapToList 1 rd in
	((lnll, lnd), tll ++ td)
	where
	huffMapToList :: Int -> Map.Map Int Int -> Maybe (Int, [Int])
	huffMapToList d m =
		(\mk -> (mk + 1, fromMaybe 0 . (m Map.!?) <$> [0 .. mk])) <$> maxKey
		where maxKey = (d `max`) . fst . fst <$> Map.maxViewWithKey m

lenToCodes :: Int -> Int -> [(Int, [Bit.B])]
lenToCodes 0 n
	| n < 3 = replicate n (0, [])
	| n < 11 = [(17, PipeBits.listFromNum 3 (n - 3))]
	| n < 139 = [(18, PipeBits.listFromNum 7 (n - 11))]
	| otherwise = (18, [I, I, I, I, I, I, I]) : lenToCodes 0 (n - 138)
lenToCodes l n | n < 4 = replicate n (l, [])
lenToCodes l n = (l, []) : go (n - 1)
	where go m
		| m < 3 = replicate m (l, [])
		| m < 7 = [(16, PipeBits.listFromNum 2 (m - 3))]
		| otherwise = (16, [I, I]) : go (m - 6)

encode :: Map.Map Int [Bit.B] -> Map.Map Int [Bit.B] -> RunLength.R -> [Bit.B]
encode dl _ (RunLength.Literal b) = dl Map.! fromIntegral b
encode dl _ (RunLength.LiteralBS b) = (dl Map.!) . fromIntegral =<< BS.unpack b
encode dl dd (RunLength.LenDist l d) = dl Map.! lc ++ le ++ dd Map.! dc ++ de
	where (lc, le) = lengthToCode l; (dc, de) = distToCode d
encode dl _ RunLength.EndOfInput = dl Map.! 256
