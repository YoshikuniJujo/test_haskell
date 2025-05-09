{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.C.Types
import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.List qualified as PipeL
import Control.Monad.Yaftee.Pipe.IO qualified as PipeI
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeB
import Control.Monad.Yaftee.Pipe.ByteString.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.ByteString.Crc qualified as Crc
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.Foldable
import Data.Bits
import Data.Sequence qualified as Seq
import Data.Bool
import Data.Word
import Data.ByteString qualified as BS
import Data.ByteString.Bit qualified as Bit
import Data.ByteString.BitArray qualified as BitArray
import System.IO
import System.Environment

import Pipe.Huffman qualified as Huffman
import Pipe.RunLength qualified as RunLength

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	let	processHeader = IO.print
	Eff.runM . Except.run @String . Fail.runExc id
		. (`State.run` OnDemand.RequestBuffer 16)
		. (`State.run` BitArray.fromByteString "")
		. (`State.run` (Seq.empty :: Seq.Seq Word8))
		. (flip (State.runN @"format") ("" :: BS.ByteString))
		. Huffman.run (Huffman.makeTree [0 :: Int .. ] fixedHuffmanList)
		. (flip (State.runN @"bits") $ BitArray.fromByteString "")
		. (`Crc.runCrc32` Crc.Crc32 0)
		. PipeL.to
		$ PipeB.hGet' 64 h Pipe.=$= OnDemand.onDemand Pipe.=$= do
			_ <- PipeT.checkRight Pipe.=$= readHeader processHeader
			blocks Pipe.=$= runLength Pipe.=$= format 32 Pipe.=$= do
					Crc.crc32'
					Crc.compCrc32
					IO.print . Crc.crc32ToByteString =<< State.getN Crc.Pkg
					IO.print @BitArray.B =<< State.get
					IO.print @BitArray.B =<< State.getN "bits"
				Pipe.=$= PipeI.print
	pure ()

blocks :: (
	U.Member Pipe.P es,
	U.Member (State.S OnDemand.Request) es,
	U.Member (State.Named "bits" BitArray.B) es,
	U.Member (State.Named Huffman.Pkg (Huffman.BinTree Int, Huffman.BinTree Int)) es,
	U.Member (State.Named Huffman.Pkg Huffman.ExtraBits) es,
	U.Base IO.I es,		-- FOR DEBUG
	U.Member (Except.E String) es, U.Member Fail.F es ) =>
	Eff.E es (Either BitArray.B BS.ByteString) RunLength.R ()
blocks = fix \go -> block >>= bool (pure ()) go

block :: (
	U.Member Pipe.P es,
	U.Member (State.S OnDemand.Request) es,
	U.Member (State.Named "bits" BitArray.B) es,
	U.Member (State.Named Huffman.Pkg (Huffman.BinTree Int, Huffman.BinTree Int)) es,
	U.Member (State.Named Huffman.Pkg Huffman.ExtraBits) es,
	U.Member (Except.E String) es,
	U.Member (U.FromFirst U.Fail) es,
	U.Base IO.I es
	) =>
--	U.Member (U.FromFirst U.Fail) es
--	) =>
	Eff.E es (Either BitArray.B BS.ByteString) RunLength.R Bool
block = do
	State.put $ OnDemand.RequestBits 1
	Just bf <- either (Just . BitArray.toBits @Word8) (const Nothing) <$> Pipe.await
	State.put $ OnDemand.RequestBits 2
	Just bt <- either (Just . BitArray.toBits @Word8) (const Nothing) <$> Pipe.await
	case bt of
		0 -> do	State.put $ OnDemand.RequestBytes 4
			ln <- getWord16FromPair =<< skipLeft1
			State.put $ OnDemand.RequestBytes ln
			Pipe.yield . RunLength.LiteralBS =<< getRight =<< Pipe.await
		1 -> do State.put $ OnDemand.RequestBuffer 100
			bits Pipe.=$= Huffman.huffman Pipe.=$= litLen
				(Huffman.makeTree [0 :: Int .. ] fixedHuffmanList)
				(Huffman.makeTree [0 :: Int .. ] fixedHuffmanDstList) 0
			pure ()
		2 -> do	State.put $ OnDemand.RequestBits 5
			IO.print @Int . (+ 257) . BitArray.toBits =<< getLeft =<< Pipe.await
			IO.print @Int . (+ 1) . BitArray.toBits =<< getLeft =<< Pipe.await
			State.put $ OnDemand.RequestBits 4
			IO.print @Int . (+ 4) . BitArray.toBits =<< getLeft =<< Pipe.await
			State.put $ OnDemand.RequestBuffer 100
			bits Pipe.=$= do
				Huffman.putTree . Huffman.makeTree codeLengthList
					=<< replicateM 14 (Bit.listToNum @Word8 <$> replicateM 3 Pipe.await)
				Huffman.huffman @Int @Word16 Pipe.=$= do
					(ht, hdt) <- (Huffman.makeTree [0 :: Int ..] *** Huffman.makeTree [0 :: Int ..]) . splitAt 282 <$> getCodeTable 0 305
					Huffman.putTree ht
					litLen ht hdt 0
			pure ()
	pure (bf /= 1)

codeLengthList :: [Int]
codeLengthList =
	[16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]

getCodeTable _ 0 = pure []
getCodeTable pr n = Pipe.await >>= \case
	Left ln	| 0 <= ln && ln <= 15 -> (ln :) <$> getCodeTable ln (n - 1)
		| ln == 16 -> do
			Huffman.putExtraBits 2
			Right eb <- Pipe.await
			(replicate (fromIntegral eb + 3) pr ++) <$> getCodeTable pr (n - fromIntegral eb - 3)
		| ln == 17 -> do
			Huffman.putExtraBits 3
			Right eb <- Pipe.await
			(replicate (fromIntegral eb + 3) 0 ++) <$> getCodeTable 0 (n - fromIntegral eb - 3)
		| ln == 18 -> do
			Huffman.putExtraBits 7
			Right eb <- Pipe.await
			(replicate (fromIntegral eb + 11) 0 ++) <$> getCodeTable 0 (n - fromIntegral eb - 11)
		| otherwise -> error "yet"
	Right _ -> error "bad"

getLeft :: U.Member (Except.E String) es => Either a b -> Eff.E es i o a
getLeft (Left r) = pure r
getLeft (Right _) = Except.throw "getLeft: Right _"

getRight :: U.Member (Except.E String) es => Either a b -> Eff.E es i o b
getRight (Left _) = Except.throw "getRight: Left _"
getRight (Right r) = pure r

readHeader :: (
	U.Member Pipe.P es,
	U.Member (State.Named Crc.Pkg Crc.Crc32) es,
	U.Member (State.S OnDemand.Request) es,
	U.Member (Except.E String) es,
	U.Member (U.FromFirst U.Fail) es ) =>
	(GzipHeader -> Eff.E es BS.ByteString o r) ->
	Eff.E es BS.ByteString o (
		Eff.E es BS.ByteString BS.ByteString r1,
		Eff.E es BS.ByteString o r )
readHeader f = Crc.crc32 Pipe.=$= do
				State.put $ OnDemand.RequestBytes 2
				ids <- Pipe.await
				when (ids /= "\31\139")
					$ Except.throw @String "Bad magic"
				State.put $ OnDemand.RequestBytes 1
				cm <- (CompressionMethod . BS.head) <$> Pipe.await
				Just flgs <- readFlags . BS.head <$> Pipe.await
				State.put $ OnDemand.RequestBytes 4
				mtm <- word32ToCTime . bsToNum <$> Pipe.await
				State.put $ OnDemand.RequestBytes 1
				ef <- BS.head <$> Pipe.await
				os <- OS . BS.head <$> Pipe.await
				mexflds <- if (flagsRawExtra flgs)
				then do	State.put $ OnDemand.RequestBytes 2
					xlen <- bsToNum <$> Pipe.await
					State.put $ OnDemand.RequestBytes xlen
					decodeExtraFields <$> Pipe.await
				else pure []
				State.put OnDemand.RequestString
				mnm <- if flagsRawName flgs
				then Just <$> Pipe.await
				else pure Nothing
				mcmmt <- if flagsRawComment flgs
				then Just <$> Pipe.await
				else pure Nothing
				when (flagsRawHcrc flgs) do
					Crc.compCrc32
					crc <- (.&. 0xffff) . (\(Crc.Crc32 c) -> c) <$> State.getN Crc.Pkg
					State.put $ OnDemand.RequestBytes 2
					m <- bsToNum <$> Pipe.await
					when (crc /= m) $
						Except.throw @String "Header CRC check failed"
				f GzipHeader {
					gzipHeaderCompressionMethod = cm,
					gzipHeaderFlags = Flags {
						flagsText = flagsRawText flgs,
						flagsHcrc = flagsRawHcrc flgs },
					gzipHeaderModificationTime = mtm,
					gzipHeaderExtraFlags = ef,
					gzipHeaderOperatingSystem = os,
					gzipHeaderExtraField = mexflds,
					gzipHeaderFileName = mnm,
					gzipHeaderComment = mcmmt }

newtype CompressionMethod = CompressionMethod {
	unCompressionMeghod :: Word8 }

pattern CompressionMethodDeflate :: CompressionMethod
pattern CompressionMethodDeflate = CompressionMethod 8

instance Show CompressionMethod where
	show CompressionMethodDeflate = "CompressionMethodDeflate"
	show (CompressionMethod cm) = "(CompressionMethod " ++ show cm ++ ")"

readFlags :: Word8 -> Maybe FlagsRaw
readFlags w = if or $ (w `testBit`) <$> [5 .. 7]
	then Nothing
	else Just FlagsRaw {
		flagsRawText = w `testBit` 0,
		flagsRawHcrc = w `testBit` 1,
		flagsRawExtra = w `testBit` 2,
		flagsRawName = w `testBit` 3,
		flagsRawComment = w `testBit` 4 }

data FlagsRaw = FlagsRaw {
	flagsRawText :: Bool,
	flagsRawHcrc :: Bool,
	flagsRawExtra :: Bool,
	flagsRawName :: Bool,
	flagsRawComment :: Bool }
	deriving Show

data Flags = Flags {
	flagsText :: Bool,
	flagsHcrc :: Bool }
	deriving Show

bsToNum :: (Bits n, Integral n) => BS.ByteString -> n
bsToNum = foldr (\b s -> fromIntegral b .|. s `shiftL` 8) 0 . BS.unpack

word32ToCTime :: Word32 -> CTime
word32ToCTime = CTime . fromIntegral

newtype OS = OS { unOS :: Word8 }

pattern OSUnix :: OS
pattern OSUnix = OS 3

instance Show OS where
	show OSUnix = "OSUnix"
	show (OS os) = "(OS " ++ show os ++ ")"

decodeExtraFields :: BS.ByteString -> [ExtraField]
decodeExtraFields "" = []
decodeExtraFields bs = let
	(ef, bs') = decodeExtraField bs in
	ef : decodeExtraFields bs'

decodeExtraField :: BS.ByteString -> (ExtraField, BS.ByteString)
decodeExtraField bs = let
	([si1, si2], bs') = BS.unpack `first` BS.splitAt 2 bs
	(ln, bs'') = bsToNum `first` BS.splitAt 2 bs'
	(dt, bs''') = BS.splitAt ln bs'' in (
		ExtraField {
			extraFieldSi1 = si1,
			extraFieldSi2 = si2,
			extraFieldData = dt },
		bs''' )

data ExtraField = ExtraField {
	extraFieldSi1 :: Word8,
	extraFieldSi2 :: Word8,
	extraFieldData :: BS.ByteString }
	deriving Show

data GzipHeader = GzipHeader {
	gzipHeaderCompressionMethod :: CompressionMethod,
	gzipHeaderFlags :: Flags,
	gzipHeaderModificationTime :: CTime,
	gzipHeaderExtraFlags :: Word8,
	gzipHeaderOperatingSystem :: OS,
	gzipHeaderExtraField :: [ExtraField],
	gzipHeaderFileName :: Maybe BS.ByteString,
	gzipHeaderComment :: Maybe BS.ByteString }
	deriving Show

skipLeft1 :: (U.Member Pipe.P es, U.Member (Except.E String) es) =>
	Eff.E es (Either a b) o b
skipLeft1 = Pipe.await >>= \case
	Left _ -> Pipe.await >>= \case
		Left _ -> Except.throw @String "Not Right"
		Right x -> pure x
	Right x -> pure x

getWord16FromPair :: (U.Member (Except.E String) es, Num n) =>
	BS.ByteString -> Eff.E es i o n
getWord16FromPair bs0 = fromIntegral @Word16 <$> do
	when (BS.length bs0 /= 4)
		$ Except.throw @String "getWord16FromPair: not 4 bytes"
	when (ln /= complement cln)
		$ Except.throw @String "bad pair"
	pure ln
	where
	(ln, cln) = (tow16 *** tow16) $ BS.splitAt 2 bs0
	tow16 bs = case BS.unpack bs of
		[b0, b1] -> fromIntegral b0 .|. (fromIntegral b1) `shiftL` 8
		_ -> error "never occur"

bits :: (
	U.Member Pipe.P es,
	U.Member (State.Named "bits" BitArray.B) es
	) =>
	Eff.E es (Either BitArray.B BS.ByteString) Bit.B r
bits = (Pipe.yield =<< pop) >> bits
	where
	pop = State.getsN "bits" BitArray.pop >>= \case
		Nothing -> Pipe.await
			>>= State.putN "bits" . either id BitArray.fromByteString
			>> pop
		Just (b, ba') -> b <$ State.putN "bits" ba'

fixedHuffmanList, fixedHuffmanDstList :: [Int]
fixedHuffmanList =
	replicate 144 8 ++ replicate 112 9 ++ replicate 24 7 ++ replicate 8 8

fixedHuffmanDstList = replicate 32 5

litLen t dt pri = Pipe.await >>= \case
	Left 256 -> pure ()
	Left i	| 0 <= i && i <= 255 -> do
			Pipe.yield (RunLength.Literal $ fromIntegral i)
			litLen t dt 0
		| 257 <= i && i <= 264 -> Huffman.putTree dt >> dist t dt (calcLength i 0) 0
		| 265 <= i && i <= 284 -> do
			Huffman.putExtraBits $ (i - 261) `div` 4
			litLen t dt i
		| i == 285 -> Huffman.putTree dt >> dist t dt (calcLength i 0) 0
	Right eb -> do
		Huffman.putTree dt
		dist t dt (calcLength pri eb) 0

dist t dt ln pri = Pipe.await >>= \case
	Left i	| 0 <= i && i <= 3 -> do
			Pipe.yield $ RunLength.LenDist ln (calcDist i 0)
			Huffman.putTree t
			litLen t dt 0
		| 4 <= i && i <= 29 -> do
			Huffman.putExtraBits $ (i - 2) `div` 2
			dist t dt ln i
		| otherwise -> error $ "putDist: yet " ++ show i
	Right eb -> do
		Pipe.yield (RunLength.LenDist ln (calcDist pri eb))
		Huffman.putTree t
		litLen t dt 0
	

calcLength :: Int -> Word16 -> Int
calcLength n eb
	| 257 <= n && n <= 284 = lens !! (n - 257) + fromIntegral eb
	| n == 285 = 258
	| otherwise = error "bad length parameter"

lens :: [Int]
lens = (+ 3) <$> scanl (+) 0 ((2 ^) <$> lensBits)

lensBits :: [Int]
lensBits = replicate 4 0 ++ (replicate 4 =<< [0 ..])

calcDist :: Int -> Word16 -> Int
calcDist n eb
	| 0 <= n && n <= 29 = dists !! n + fromIntegral eb
	| otherwise = error "bad distance parameter"

dists :: [Int]
dists = (+ 1) <$> scanl (+) 0 ((2 ^) <$> distsBits)

distsBits :: [Int]
distsBits = replicate 2 0 ++ (replicate 2 =<< [0 ..])

runLength :: (
	U.Member Pipe.P es,
	U.Member (State.S (Seq.Seq Word8)) es ) =>
	Eff.E es RunLength.R (Either Word8 BS.ByteString) r
runLength = Pipe.await >>= \rl -> (>> runLength) $ ($ rl) \case
	RunLength.Literal w -> State.modify (`snoc` w) >> Pipe.yield (Left w)
	RunLength.LiteralBS bs ->
		State.modify (`appendR` BS.unpack bs) >> Pipe.yield (Right bs)
	RunLength.LenDist ln d -> State.gets (repetition ln d) >>= \ws ->
		State.modify (`appendR` ws) >> Pipe.yield (Right $ BS.pack ws)

repetition :: Int -> Int -> Seq.Seq Word8 -> [Word8]
repetition r d ws = takeRep r ws' ws'
	where ws' = toList . Seq.take r $ takeR d ws

takeRep :: Int -> [a] -> [a] -> [a]
takeRep 0 _ _ = []
takeRep n xs0 (x : xs) = x : takeRep (n - 1) xs0 xs
takeRep n xs0 [] = takeRep n xs0 xs0

takeR :: Int -> Seq.Seq Word8 -> Seq.Seq Word8
takeR n xs = Seq.drop (Seq.length xs - n) xs

snoc :: Seq.Seq Word8 -> Word8 -> Seq.Seq Word8
snoc s w = let s' = if ln > 32768 then Seq.drop (ln - 32768) s else s in
	s' Seq.|> w
	where ln = Seq.length s

appendR :: Seq.Seq Word8 -> [Word8] -> Seq.Seq Word8
appendR s ws = let s' = if ln > 32768 then Seq.drop (ln - 32768) s else s in
	foldl (Seq.|>) s' ws
	where ln = Seq.length s

format n = do
	b <- checkLength n
	if b
	then yieldLen n >> format n
	else readMore >>= bool
		(Pipe.yield =<< State.getN @BS.ByteString "format")
		(format n)

readMore :: (
	U.Member Pipe.P es,
	U.Member (State.Named "format" BS.ByteString) es ) =>
	Eff.E es (Either Word8 BS.ByteString) o Bool
readMore = do
	e <- Pipe.isEmpty
	if e then pure False else Pipe.await >>= \case
		Left w -> True <$ State.modifyN "format" (`BS.snoc` w)
		Right bs -> True <$ State.modifyN "format" (`BS.append` bs)

checkLength n = do
	bs <- State.getN "format"
	pure $ BS.length bs >= n

yieldLen n = do
	bs <- State.getN "format"
	let	(r, bs') = BS.splitAt n bs
	State.putN "format" bs'
	Pipe.yield r
