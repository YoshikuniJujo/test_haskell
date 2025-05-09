{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-x-partial #-}

module Yaftee.UseFTCQ.Pipe.Gzip.Compress.Block (
	runLengthsToBits
	) where

import Control.Arrow
import Data.Maybe
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Swizzle qualified as Swizzle
import Data.Bool
import Data.ByteString qualified as BS

import Yaftee.UseFTCQ.Pipe.Gzip.RunLength (RunLength)
import Yaftee.UseFTCQ.Pipe.Gzip.RunLength qualified as RunLength

import Data.Calc
import Data.HuffmanTree (pairToCodes)

import Data.Bit (pattern O, pattern I)
import Data.Bit qualified as Bit

import Yaftee.UseFTCQ.Pipe.Bits qualified as PipeBits

import Data.PackageMerge qualified as PackageMerge

newtype FileLength = FileLength Int deriving Show

runLengthsToBits :: Bool -> [RunLength] -> [Bit.B]
runLengthsToBits _ [] = []
runLengthsToBits f rl_ =
	[bool O I f, O, I] ++
	PipeBits.listFromNum 5 (lll - 257) ++
	PipeBits.listFromNum 5 (ld - 1) ++
	PipeBits.listFromNum 4 (lt - 4) ++
	hdr ++ (runLengthToBits dll dd =<< rl)
	where
	((lll, ld, lt, hdr), (dll, dd)) = foobar mll md
	mll = PackageMerge.run 14 $ RunLength.toLitLenFreqs rl
	md = PackageMerge.run 14 $ RunLength.toDistFreqs rl
	rl = rl_ ++ [RunLength.EndOfInput]

runLengthToBits :: Map.Map Int [Bit.B] -> Map.Map Int [Bit.B] -> RunLength -> [Bit.B]
runLengthToBits ml _ (RunLength.Literal b) = ml Map.! fromIntegral b
runLengthToBits ml _ (RunLength.LiteralBS bs) = (ml Map.!) . fromIntegral =<< BS.unpack bs
runLengthToBits ml md (RunLength.LenDist l d) =
	ml Map.! lc ++ le ++ md Map.! dc ++ de
	where
	(lc, le) = lengthToCode l
	(dc, de) = distToCode d
runLengthToBits ml _ RunLength.EndOfInput = ml Map.! 256

foobar :: Map.Map Int Int -> Map.Map Int Int ->
	((Int, Int, Int, [Bit.B]), (Map.Map Int [Bit.B], Map.Map Int [Bit.B]))
foobar mll md = (mkLitLenDistTableFromMapMap mll md, (tableToDict mll, tableToDict md))

mkLitLenDistTableFromMapMap :: Map.Map Int Int -> Map.Map Int Int -> (Int, Int, Int, [Bit.B])
mkLitLenDistTableFromMapMap mll md = (lll, ld, lt, ttbs ++ mkLitLenDistTableBits m aes)
	where
	m = tableToDict $ mapMapToTableTable mll md
	f@(lll, ld, _) = mapMapToLitLenDstList mll md
	(lt, tt) = tableTableToOrder' $ fooToTableTable f
	ttbs = PipeBits.listFromNum 3 =<< (tt :: [Int])
	aes = fooToCodes f

mapMapToTableTable :: Num a =>
	Map.Map Int Int -> Map.Map Int Int -> Map.Map Int a
mapMapToTableTable rll rd = PackageMerge.run 6
	. ((head &&& length) <$>)
	. L.group . L.sort $ fst <$> mapMapToCodes rll rd

mapMapToLitLenDstList :: Map.Map Int Int -> Map.Map Int Int -> (Int, Int, [Int])
mapMapToLitLenDstList rll rd = let
	(lnll, tll) = fromJust $ huffMapToList 257 rll
	(lnd, td) = fromJust $ huffMapToList 1 rd in
	(lnll, lnd, tll ++ td)

tableTableToOrder' :: Map.Map Int Int -> (Int, [Int])
tableTableToOrder' tt = (ln, take ln o)
	where
	ln = 4 `max` length o
	o = dropTrailing0 $ tableTableToOrder tt

tableTableToOrder :: Num b => Map.Map Int b -> [b]
tableTableToOrder m = fromMaybe 0 . (m Map.!?) <$> (tableTableLenOrder :: [Int])

tableTableLenOrder :: [Int]
tableTableLenOrder = [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]

tableToDict :: Map.Map Int Int -> Map.Map Int [Bit.B]
tableToDict = Map.fromList @Int
	. ((uncurry $ flip (,)) <$>)
	. pairToCodes
	. L.sortOn fst
	. ((uncurry $ flip (,)) <$>)
	. Map.toList

maxKey :: Ord k => k -> Map.Map k v -> Maybe k
maxKey d m = (d `max`) . fst . fst <$> Map.maxViewWithKey m

huffmanLenToCodes :: Int -> Int -> [(Int, [Bit.B])]
huffmanLenToCodes 0 n
	| n < 3 = replicate n (0, [])
	| n < 11 = [(17, PipeBits.listFromNum 3 (n - 3))]
	| n < 139 = [(18, PipeBits.listFromNum 7 (n - 11))]
	| otherwise =
		(18, [I, I, I, I, I, I, I]) : huffmanLenToCodes 0 (n - 138)
huffmanLenToCodes l n | n < 4 = replicate n (l, [])
huffmanLenToCodes l n = (l, []) : go (n - 1)
	where
	go m	| m < 3 = replicate m (l, [])
		| m < 7 = [(16, PipeBits.listFromNum 2 (m - 3))]
		| otherwise = (16, [I, I]) : go (m - 6)

mkLitLenDistTableBits :: Map.Map Int [Bit.B] -> [(Int, [Bit.B])] -> [Bit.B]
mkLitLenDistTableBits _ [] = []
mkLitLenDistTableBits m ((alp, ebs) : aes) = m Map.! alp ++ ebs ++ mkLitLenDistTableBits m aes

dropTrailing0 :: [Int] -> [Int]
dropTrailing0 = reverse . dropWhile (== 0) . reverse

mapMapToCodes :: Map.Map Int Int -> Map.Map Int Int -> [(Int, [Bit.B])]
mapMapToCodes rll rd = uncurry huffmanLenToCodes
	=<< (head &&& length) <$> L.group (Swizzle.z $ mapMapToLitLenDstList rll rd)

fooToTableTable :: Num a => (Int, Int, [Int]) -> Map.Map Int a
fooToTableTable rl = PackageMerge.run 6
	. ((head &&& length) <$>)
	. L.group . L.sort $ fst <$> fooToCodes rl

huffMapToList :: Int -> Map.Map Int Int -> Maybe (Int, [Int])
huffMapToList d m = (\mk -> (mk + 1, fromMaybe 0 . (m Map.!?) <$> [0 .. mk])) <$> mmk
	where
	mmk = maxKey d m

fooToCodes :: (Int, Int, [Int]) -> [(Int, [Bit.B])]
fooToCodes baz = uncurry huffmanLenToCodes
	=<< (head &&& length) <$> L.group (Swizzle.z baz)
