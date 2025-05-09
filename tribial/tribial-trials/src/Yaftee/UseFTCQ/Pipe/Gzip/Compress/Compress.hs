{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-x-partial #-}

module Yaftee.UseFTCQ.Pipe.Gzip.Compress.Compress (
	compressFile
	) where

import Control.Arrow
import Control.Monad.Fix
import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.IO qualified as YIO
import Yaftee.UseFTCQ.Pipe qualified as Pipe
import Yaftee.UseFTCQ.Pipe.List qualified as PipeL
import Yaftee.UseFTCQ.Pipe.ByteString qualified as PipeBS
import Yaftee.UseFTCQ.State qualified as State
import Yaftee.UseFTCQ.Fail qualified as Fail
import Yaftee.UseFTCQ.HFreer qualified as HFreer
import Yaftee.OpenUnion qualified as Union
import Data.Maybe
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Swizzle qualified as Swizzle
import Data.Bool
import Data.ByteString qualified as BS
import System.IO

import Yaftee.UseFTCQ.Pipe.Gzip.RunLength (RunLength)
import Yaftee.UseFTCQ.Pipe.Gzip.RunLength qualified as RunLength
import Yaftee.UseFTCQ.Pipe.Gzip.Compress.Triple

import Data.Calc
import Data.HuffmanTree (pairToCodes)

import Data.Bit (pattern O, pattern I)
import Data.Bit qualified as Bit

import Yaftee.UseFTCQ.Pipe.Bits qualified as PipeBits

import Data.PackageMerge qualified as PackageMerge
import Yaftee.UseFTCQ.Pipe.Crc
import Tools.ByteStringNum
import Yaftee.UseFTCQ.Pipe.Gzip.Compress.Gzip
import Yaftee.UseFTCQ.Pipe.Gzip.Compress.AheadPos

compressFile :: RunLengthType BS.ByteString RunLength r -> FilePath -> FilePath -> IO ()
compressFile crl fp ofp = do
	((rl, FileLength fln), cr) <- getRunLengths crl fp
	let	bts = runLengthsToBits rl
		bd = bitsToByteString $ bts ++ [O, O, O, O, O, O, O]
		hdr = encodeGzipHeader $ gzipHeaderToRaw sampleGzipHeader { gzipHeaderFileName = Just "OnDemand.hs" }
	BS.writeFile ofp $
		hdr `BS.append` bd `BS.append`
		crcToByteString cr `BS.append`
		numToBs' 4 fln

getRunLengths :: RunLengthType BS.ByteString RunLength r -> FilePath ->
	IO (([RunLength], FileLength), Crc)
getRunLengths crl fp =
	((fst &&& snd) . fst &&& snd)  . fst . fst . fst . either undefined id <$> tryCompress crl fp

tryCompress ::
	Eff.E '[
		Pipe.P,
		State.S FileLength, State.S Crc,
		State.S Triple, State.S BS.ByteString, State.S AheadPos, Fail.F, YIO.I
		] BS.ByteString RunLength r ->
	FilePath -> IO (Either String ((((([RunLength], FileLength), Crc), Triple), BS.ByteString), AheadPos))
tryCompress crl fp = withFile fp ReadMode \h ->
	Eff.runM . Fail.run
		. (`State.run` AheadPos 0) . (`State.run` ("" :: BS.ByteString))
		. (`State.run` triple0)
		. (`State.run` Crc 0)
		. (`State.run` FileLength 0)
		. PipeL.to
		$ PipeBS.hGet 100 h Pipe.=$= lengthPipe Pipe.=$= crcPipe Pipe.=$= crl Pipe.=$= do
			fix \go -> Pipe.isMore >>=
				bool (pure ()) (Pipe.await >>= Pipe.yield >> go)
			compCrc

lengthPipe :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S FileLength) effs ) =>
	Eff.E effs BS.ByteString BS.ByteString ()
lengthPipe = Pipe.await >>= \case
	bs -> do
		State.modify \(FileLength ln) -> FileLength $ ln + BS.length bs
		Pipe.yield bs
		lengthPipe

newtype FileLength = FileLength Int deriving Show

bitsToByteString :: [Bit.B] -> BS.ByteString
bitsToByteString bits = getPure . snd . fromJust . fst
	. Eff.run . (`State.run` Bit.empty) . Pipe.run
	$ Pipe.yield bits Pipe.=$= PipeBits.toByteString Pipe.=$= fix \go ->
		Pipe.isMore
			>>= bool (pure "") ((<$> go) . BS.append =<< Pipe.await)
	where getPure = \case HFreer.Pure x -> x; _ -> error "bad"

runLengthsToBits :: [RunLength] -> [Bit.B]
runLengthsToBits rl_ =
	[I, O, I] ++
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

type RunLengthType = Eff.E [
	Pipe.P, State.S FileLength, State.S Crc, State.S Triple,
	State.S BS.ByteString, State.S AheadPos, Fail.F, YIO.I]
