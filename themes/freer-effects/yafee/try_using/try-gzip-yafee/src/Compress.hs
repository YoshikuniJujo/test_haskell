{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Compress where

import Foreign.Marshal.Alloc
import Control.Arrow
import Control.Monad.Fix
import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.IO qualified as YIO
import Control.Monad.Yafee.Pipe qualified as Pipe
import Control.Monad.Yafee.Pipe.Tools qualified as PipeTools
import Control.Monad.Yafee.Pipe.IO qualified as PipeIO
import Control.Monad.Yafee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yafee.State qualified as State
import Control.Monad.Yafee.Except qualified as Except
import Control.Monad.Yafee.Fail qualified as Fail
import Control.OpenUnion qualified as Union
import Data.Bits
import Data.Maybe
import Data.List qualified as L
import Data.Sequence qualified as Seq
import Data.Map qualified as Map
import Data.Swizzle qualified as Swizzle
import Data.Bool
import Data.Word
import Data.ByteString qualified as BS
import System.IO

import RunLength
import Triple

import Calc
import HuffmanTree

import BitArray (Bit(..), numToBits)
import Pipe.BitArray

import PackageMerge qualified as PackageMerge
import Block
import Pipe.Huffman
import Pipe.Crc
import ByteStringNum
import Gzip

compressRL :: (
	Union.Member (State.S Triple) effs,
	Union.Member (State.S BS.ByteString) effs,
	Union.Member (State.S AheadPos) effs ) =>
	Eff.E (Pipe.P BS.ByteString RunLength ': effs) ()
compressRL = fix \go -> do
	mb <- get
	mb1 <- getAhead
	mb2 <- getAhead
	case mb of
		Nothing -> pure ()
		Just b -> do
			case (mb1, mb2) of
				(Just b1, Just b2) -> do
					st <- State.get
					mil <- getIndexLength st (BS.pack [b, b1, b2]) getAhead
					case mil of 
						Nothing -> do
							State.modify (`updateTriple` b)
							Pipe.yield (RunLengthLiteral b)
						Just (i, l) -> do
							d <- State.gets $ calcDistance i
							let	c1 = (l, (l + 1, [RunLengthLenDist (l + 3) d]))
							State.modify (`updateTriple` b)
							b' <- fromJust <$> get
							mb2' <- getAhead
							mb3 <- getAhead

							case mb3 of
								Nothing -> do
									State.modify (`updateTriple` b')
									runCandidate c1
								Just b3 -> do
									st <- State.get
									mil' <- getIndexLength st (BS.pack [b', b2, b3]) getAhead
									case mil' of
										Nothing -> do
											State.modify (`updateTriple` b')
											runCandidate c1
										Just (i', l') -> do
											d' <- State.gets $ calcDistance i'
											State.modify (`updateTriple` b')
											let	c2 = (l', (2 + l', [
													RunLengthLiteral b,
													-- RunLengthLiteral 0x39,
													RunLengthLenDist (l' + 3) d']))
											if l + 1 >= l'
											then runCandidate c1
											else runCandidate c2
				_ -> do	State.modify (`updateTriple` b)
					Pipe.yield (RunLengthLiteral b)
			go

runCandidate (l, (l', ys)) = do
	bs <- getBytes l'
	State.modify \st' -> foldl updateTriple st' $ BS.unpack bs
	Pipe.yield `mapM_` ys

tryCompress :: IO (((((), [()]), Triple), BS.ByteString), AheadPos)
tryCompress = withFile "samples/abcdef4.txt" ReadMode \h ->
	Eff.runM . (`State.run` AheadPos 0) . (`State.run` ("" :: BS.ByteString))
		. (`State.run` triple0) . Pipe.run
		$ PipeBS.hGet 100 h Pipe.=$= compressRL Pipe.=$= fix \go ->
			Pipe.await >>= maybe (pure ()) (\bs -> YIO.print bs >> go)

tryCompress' :: FilePath -> IO (((((), [()]), Triple), BS.ByteString), AheadPos)
tryCompress' fp = withFile fp WriteMode \hw -> alloca \p ->
	withFile "samples/abcdef4.txt" ReadMode \h ->
		Eff.runM . (`State.run` AheadPos 0) . (`State.run` ("" :: BS.ByteString))
			. (`State.run` triple0) . Pipe.run
			$ PipeBS.hGet 100 h Pipe.=$= compressRL Pipe.=$=
				(fix \go -> Pipe.await >>= maybe (pure ()) (\bs -> Pipe.yield (runLengthToWord32 bs) >> go)) Pipe.=$=
				listToAtom Pipe.=$=
				PipeIO.hPutStorable hw p

foo, bar :: BS.ByteString
foo = "\x1f\x8b\x08\x08\xd2\x25\xea\x67\x00\x03\x61\x62\x63\x64\x65\x66\x34\x2e\x74\x78\x74\x00"
bar = "\x00\x6e\x24\x3a\x63\x19\x00\x00\x00"

tryCompress'' :: IO (((((BS.ByteString, [()]), Triple), BS.ByteString), AheadPos), ([Bit], [Bit]))
tryCompress'' = withFile "samples/abcdef4.txt" ReadMode \h ->
	Eff.runM
		. (`State.run` ([] :: [Bit], [] :: [Bit]))
		. (`State.run` AheadPos 0)
		. (`State.run` ("" :: BS.ByteString))
		. (`State.run` triple0) . Pipe.run

		$	(Pipe.yield [I, I, O] >> PipeBS.hGet 100 h Pipe.=$=
				compressRL Pipe.=$=
				PipeTools.convert (runLengthToBits
					(listToMap fixedTableList)
					(listToMap fixedDstTableList))
				) Pipe.=$=

			bitsToByteString Pipe.=$= fix \go -> do
--			PipeTools.convert (((`showHex` "") =<<) . BS.unpack) Pipe.=$= fix \go -> do
			mbs <- Pipe.await
			case mbs of
				Nothing -> pure ""
				Just bs -> (bs `BS.append`) <$> go
			
--			PipeIO.putStrLn

getFoo :: IO BS.ByteString
getFoo = (foo `BS.append`) . (`BS.append` bar) . fst . fst . fst . fst . fst <$> tryCompress''

tryCompress''' ::
	Eff.E '[
		Pipe.P BS.ByteString RunLength,
		State.S FileLength, State.S Crc,
		State.S Triple, State.S BS.ByteString, State.S AheadPos, IO
		] r ->
	FilePath -> IO (((((([RunLength], [()]), FileLength), Crc), Triple), BS.ByteString), AheadPos)
tryCompress''' crl fp = withFile fp ReadMode \h ->
	Eff.runM . (`State.run` AheadPos 0) . (`State.run` ("" :: BS.ByteString))
		. (`State.run` triple0)
		. (`State.run` Crc 0)
		. (`State.run` FileLength 0)
		. Pipe.run
		$ PipeBS.hGet 100 h Pipe.=$= lengthPipe Pipe.=$= crcPipe Pipe.=$= crl Pipe.=$= do
			r <- fix \go -> Pipe.await >>= maybe (pure []) (\rl -> (rl :) <$> go)
			r <$ compCrc

lengthPipe :: (
	Union.Member (State.S FileLength) effs
	) =>
	Eff.E (Pipe.P BS.ByteString BS.ByteString ': effs) ()
lengthPipe = Pipe.await >>= \case
	Nothing -> pure ()
	Just bs -> do
		State.modify \(FileLength ln) -> FileLength $ ln + BS.length bs
		Pipe.yield bs
		lengthPipe

newtype FileLength = FileLength Int deriving Show

fileLengthToByteString (FileLength n) = numToBs' 4 n

getRunLengths crl fp =
	((fst . fst &&& snd) . fst &&& snd)  . fst . fst . fst <$> tryCompress''' crl fp

getSorted = (((: []) `first`) <$>) . L.sortOn snd . runLengthsToLitLenFreqs

getDistSorted = (((: []) `first`) <$>) . L.sortOn snd . runLengthsToDistFreqs

rawTableToByteString n m = BS.pack $ maybe 0 fromIntegral . (m Map.!?) <$> [0 .. n - 1]

makeLitLenTable :: [RunLength] -> BS.ByteString
makeLitLenTable = rawTableToByteString 286 . PackageMerge.run 14 . getSorted

makeDistTable :: [RunLength] -> BS.ByteString
makeDistTable = rawTableToByteString 30 . PackageMerge.run 14 . getDistSorted

makeCompressed :: [RunLength] -> [Bit]
makeCompressed rl = runLengthToBits dll dd =<< rl
	where
	dll = tableToDict . PackageMerge.run 14 $ getSorted rl
	dd = tableToDict . PackageMerge.run 14 $ getDistSorted rl

bitsToByteStringRaw :: [Bit] -> BS.ByteString
bitsToByteStringRaw bits =
	BS.concat . snd . fst
	. Eff.run . (`State.run` (([], []) :: BitQueue)) . Pipe.run
	$ Pipe.yield bits Pipe.=$= bitsToByteString

listToAtom :: Eff.E (Pipe.P [a] a ': effs) ()
listToAtom = fix \go -> Pipe.await >>= \case
	Nothing -> pure ()
	Just xs -> Pipe.yield `mapM_` xs >> go

compressIntoFormatX :: FilePath -> FilePath -> IO ()
compressIntoFormatX ifl ofl = do
	((rl, fln), crc) <- (((++ [RunLengthEndOfInput]) `first`) `first`) <$> getRunLengths compressRL ifl
	let	tll = makeLitLenTable rl
		td = makeDistTable rl
		cd = bitsToByteStringRaw $ makeCompressed rl
	BS.writeFile ofl $ tll `BS.append` td `BS.append` cd

readFromFormatX :: FilePath -> IO (BinTree Int, BinTree Int, BS.ByteString)
readFromFormatX fp = do
	h <- openFile fp ReadMode
	tll <- mkTr [0 ..] . BS.unpack <$> BS.hGet h 286
	td <- mkTr [0 ..] . BS.unpack <$> BS.hGet h 30
	cnt <- BS.hGetContents h
	pure (tll, td, cnt)

huffmanDecompress :: (
	Union.Member (State.S (BinTree Int, BinTree Int)) effs,
	Union.Member (State.S ExtraBits) effs,
	Union.Member Fail.F effs
	) =>
	BinTree Int -> BinTree Int -> Eff.E (Pipe.P Bit RunLength ': effs) ()
huffmanDecompress tll td = huffmanPipe Pipe.=$= do
	State.put $ (id &&& id) tll
	putDecoded tll td 0

byteStringToBits :: BS.ByteString -> [Bit]
byteStringToBits bs = (\w -> bool O I . (w `testBit`) <$> [0 .. 7]) =<< BS.unpack bs

decFormatX fp = do
	(tll, td, cnt) <- readFromFormatX fp
	Eff.runM . Fail.run
		. (\m -> State.runN @"format" m ("" :: BS.ByteString))
		. (`State.run` (Seq.empty :: Seq.Seq Word8))
		. (`State.run` ExtraBits 0) . (`State.run` (fixedTable, fixedTable)) . Pipe.run
		$ Pipe.yield `mapM` (byteStringToBits cnt) Pipe.=$=
			huffmanDecompress tll td Pipe.=$=
			runLength Pipe.=$=
			format 100 Pipe.=$=
			PipeBS.putStr
--			PipeIO.print

get :: (
	Union.Member (State.S BS.ByteString) effs,
	Union.Member (State.S AheadPos) effs
	) => Eff.E (Pipe.P BS.ByteString o ': effs) (Maybe Word8)
get = State.gets BS.uncons >>= \case
	Nothing -> bool (pure Nothing) get =<< readMore
	Just (b, bs) -> do
		State.put $ AheadPos 0
		State.put bs
		pure $ Just b

getBytes :: (
	Union.Member (State.S BS.ByteString) effs,
	Union.Member (State.S AheadPos) effs
	) => Int -> Eff.E (Pipe.P BS.ByteString o ': effs) BS.ByteString
getBytes n = State.get >>= \bs ->
	if BS.length bs >= n then BS.take n bs <$ State.put (BS.drop n bs) else
		readMore >> getBytes n

getAhead :: (
	Union.Member (State.S BS.ByteString) effs,
	Union.Member (State.S AheadPos) effs
	) =>
	Eff.E (Pipe.P BS.ByteString o ': effs) (Maybe Word8)
getAhead = do
	bs <- State.get
	AheadPos i <- State.get
	case bs BS.!? i of
		Nothing -> bool (pure Nothing) getAhead =<< readMore
		Just b -> Just b <$ State.modify nextAheadPos

readMore :: Union.Member (State.S BS.ByteString) effs =>
	Eff.E (Pipe.P BS.ByteString o ': effs) Bool
readMore = Pipe.await >>= \case
	Nothing -> pure False
	Just bs -> True <$ State.modify (`BS.append` bs)

newtype AheadPos = AheadPos Int deriving Show

nextAheadPos :: AheadPos -> AheadPos
nextAheadPos (AheadPos p) = AheadPos $ p + 1

tryDecompress :: FilePath -> IO (Either String ((), [RunLength]))
tryDecompress fp = withFile fp ReadMode \h -> alloca \p ->
	Eff.runM . Except.run . Pipe.run $
		PipeIO.hGetStorable h p Pipe.=$=
		word32ToRunLength Pipe.=$=
		PipeIO.print

tryDecompress' :: FilePath -> IO (Either String (((), [RunLength]), Seq.Seq Word8))
tryDecompress' fp = withFile fp ReadMode \h -> alloca \p ->
	Eff.runM . Except.run . (`State.run` Seq.empty) . Pipe.run $
		PipeIO.hGetStorable h p Pipe.=$=
		word32ToRunLength Pipe.=$=
		runLength Pipe.=$=
		PipeIO.print

runLengthToBits :: Map.Map Int [Bit] -> Map.Map Int [Bit] -> RunLength -> [Bit]
runLengthToBits ml _ (RunLengthLiteral b) = ml Map.! fromIntegral b
runLengthToBits ml _ (RunLengthLiteralBS bs) = (ml Map.!) . fromIntegral =<< BS.unpack bs
runLengthToBits ml md (RunLengthLenDist l d) =
	ml Map.! lc ++ le ++ md Map.! dc ++ de
	where
	(lc, le) = lengthToCode l
	(dc, de) = distToCode d
runLengthToBits ml _ RunLengthEndOfInput = ml Map.! 256

maxKey :: Ord k => k -> Map.Map k v -> Maybe k
maxKey d m = (d `max`) . fst . fst <$> Map.maxViewWithKey m

huffMapToList :: Int -> Map.Map Int Int -> Maybe (Int, [Int])
huffMapToList d m = (\mk -> (mk + 1, fromMaybe 0 . (m Map.!?) <$> [0 .. mk])) <$> mmk
	where
	mmk = maxKey d m

huffmanLenToCodes :: Int -> Int -> [(Int, [Bit])]
huffmanLenToCodes 0 n
	| n < 3 = replicate n (0, [])
	| n < 11 = [(17, numToBits 3 (n - 3))]
	| n < 139 = [(18, numToBits 7 (n - 11))]
	| otherwise =
		(18, [I, I, I, I, I, I, I]) : huffmanLenToCodes 0 (n - 138)
huffmanLenToCodes l n | n < 4 = replicate n (l, [])
huffmanLenToCodes l n = (l, []) : go (n - 1)
	where
	go m	| m < 3 = replicate m (l, [])
		| m < 7 = [(16, numToBits 2 (m - 3))]
		| otherwise = (16, [I, I]) : go (m - 6)

runLengthToLitLenDstList :: [RunLength] -> (Int, Int, [Int])
runLengthToLitLenDstList rl = let
	Just (lnll, tll) = huffMapToList 257 . PackageMerge.run 14 $ getSorted rl
	Just (lnd, td) = huffMapToList 1 . PackageMerge.run 14 $ getDistSorted rl in
	(lnll, lnd, tll ++ td)

runLengthToCodes rl = uncurry huffmanLenToCodes
	=<< (head &&& length) <$> L.group (Swizzle.z $ runLengthToLitLenDstList rl)

runLengthToTableTable rl = PackageMerge.run 6
	. (((: []) `first`) <$>)
	. L.sortOn snd
	. ((head &&& length) <$>)
	. L.group . L.sort $ fst <$> runLengthToCodes rl

mkLitLenDistTableBits :: Map.Map Int [Bit] -> [(Int, [Bit])] -> [Bit]
mkLitLenDistTableBits m [] = []
mkLitLenDistTableBits m ((alp, ebs) : aes) = m Map.! alp ++ ebs ++ mkLitLenDistTableBits m aes

mkLitLenDistTableFromRunLength :: [RunLength] -> [Bit]
mkLitLenDistTableFromRunLength rl = mkLitLenDistTableBits m aes
	where
	m = tableToDict $ runLengthToTableTable rl
	aes = runLengthToCodes rl

tableTableLenOrder = [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]

tableTableToOrder m = fromMaybe 0 . (m Map.!?) <$> (tableTableLenOrder :: [Int])

tableTableToOrder' tt = (ln, take ln o)
	where
	ln = 4 `max` length o
	o = dropTrailing0 $ tableTableToOrder tt

dropTrailing0 = reverse . dropWhile (== 0) . reverse

runLengthToRawTables :: [RunLength] -> (Map.Map Int Int, Map.Map Int Int)
runLengthToRawTables rl = (dll, dd)
	where
	dll = PackageMerge.run 14 $ getSorted rl
	dd = PackageMerge.run 14 $ getDistSorted rl

tableToDict :: Map.Map Int Int -> Map.Map Int [Bit]
tableToDict = Map.fromList @Int
	. ((uncurry $ flip (,)) <$>)
	. pairToCodes
	. L.sortOn fst
	. ((uncurry $ flip (,)) <$>)
	. Map.toList

mapMapToLitLenDstList rll rd = let
	Just (lnll, tll) = huffMapToList 257 rll
	Just (lnd, td) = huffMapToList 1 rd in
	(lnll, lnd, tll ++ td)

mapMapToCodes rll rd = uncurry huffmanLenToCodes
	=<< (head &&& length) <$> L.group (Swizzle.z $ mapMapToLitLenDstList rll rd)

mapMapToTableTable rll rd = PackageMerge.run 6
	. (((: []) `first`) <$>)
	. L.sortOn snd
	. ((head &&& length) <$>)
	. L.group . L.sort $ fst <$> mapMapToCodes rll rd

fooToCodes foo = uncurry huffmanLenToCodes
	=<< (head &&& length) <$> L.group (Swizzle.z foo)

fooToTableTable rl = PackageMerge.run 6
	. (((: []) `first`) <$>)
	. L.sortOn snd
	. ((head &&& length) <$>)
	. L.group . L.sort $ fst <$> fooToCodes rl

-- mkLitLenDistTableFromMapMap :: [RunLength] -> (Int, Int, [Bit])
mkLitLenDistTableFromMapMap mll md = (lll, ld, lt, ttbs ++ mkLitLenDistTableBits m aes)
	where
	m = tableToDict $ mapMapToTableTable mll md
	f@(lll, ld, _) = mapMapToLitLenDstList mll md
	(lt, tt) = tableTableToOrder' $ fooToTableTable f
	ttbs = numToBits 3 =<< (tt :: [Int])
	aes = fooToCodes f

foobar mll md = (mkLitLenDistTableFromMapMap mll md, (tableToDict mll, tableToDict md))

bazbaz rl_ =
	[I, O, I] ++
	numToBits 5 (lll - 257) ++
	numToBits 5 (ld - 1) ++
	numToBits 4 (lt - 4) ++
	hdr ++ (runLengthToBits dll dd =<< rl)
	where
	(mll, md) = runLengthToRawTables rl
	((lll, ld, lt, hdr), (dll, dd)) = foobar mll md
	rl = rl_ ++ [RunLengthEndOfInput]

compressFile crl fp ofp = do
	((rl, fln), cr) <- getRunLengths crl fp
	let	crbs = crcToByteString cr
		flnbs = fileLengthToByteString fln
		bts = bazbaz rl
		bd = bitsToByteStringRaw $ bts ++ [O, O, O, O, O, O, O]
		hdr = encodeGzipHeader $ gzipHeaderToRaw sampleGzipHeader { gzipHeaderFileName = Just "OnDemand.hs" }
	BS.writeFile ofp $
		hdr `BS.append` bd `BS.append` crbs `BS.append` flnbs
