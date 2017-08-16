{-# LANGUAGE LambdaCase, OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-tabs #-}

import Control.Arrow
import Data.Monoid
import Data.Foldable
import Data.Bits
import Data.Bool
import Data.List
import Data.Word
import Data.String

import qualified Data.ByteString as BS

buildSimplest :: BS.ByteString
buildSimplest = "x\156\1\1\0\254\255a\0b\0b"

emptyFixed :: BS.ByteString
emptyFixed = "x\156\3\0\1\0\1"

newtype Bs = Bs { unBs :: [Bool] } deriving Eq

lengthBits :: Bs -> Int
lengthBits = length . unBs

reverseBits :: Bs -> Bs
reverseBits = Bs . reverse . unBs

pushBit :: Bool -> Bs -> Bs
pushBit b (Bs bs) = Bs $ b : bs

splitAtBits :: Int -> Bs -> (Bs, Bs)
splitAtBits n = (Bs *** Bs) . splitAt n . unBs

bitsToByteString :: Bs -> BS.ByteString
bitsToByteString bs = BS.pack . map bitsToWord $ groupNBits 8 bs

byteStringToBits :: BS.ByteString -> Bs
byteStringToBits = mconcat . map (wordToBitsN 8) . BS.unpack

instance Show Bs where
	show (Bs bs) = map (bool '0' '1') bs

instance Monoid Bs where
	mempty = Bs []
	Bs bs1 `mappend` Bs bs2 = Bs $ bs1 ++ bs2

instance IsString Bs where
	fromString = Bs . map (== '1')

bitsToWord :: (Bits a, Num a) => Bs -> a
bitsToWord = btw . unBs
	where
	btw [] = 0
	btw (b : bs) = (bool 0 1 b) + (btw bs `shiftL` 1)

groupNBits :: Int -> Bs -> [Bs]
groupNBits n = map Bs . groupN n . unBs

groupN :: Int -> [a] -> [[a]]
groupN n [] = []
groupN n xs = take n xs : groupN n (drop n xs)

-- FIXED HUFFMAN

makeFixedSimple :: BS.ByteString -> BS.ByteString
makeFixedSimple bs = ("x\156" <>) . (<> adler32 bs) . bitsToByteString $
	(Bs $ map (== '1') "110") <> fixedHuffmans (BS.unpack bs)

wordToBits :: (Bits a, Num a) => a -> Bs
wordToBits 0 = mempty
wordToBits w = (w `testBit` 0) `pushBit` wordToBits (w `shiftR` 1)

wordToBitsN :: (Bits a, Num a) => Int -> a -> Bs
wordToBitsN n _ | n <= 0 = mempty
wordToBitsN n w = (w `testBit` 0) `pushBit` wordToBitsN (n - 1) (w `shiftR` 1)

fixedHuffman1 :: Word8 -> Bs
fixedHuffman1 w
	| w < 0x90 = wordToBitsN 8 (48 + w)
	| otherwise = wordToBitsN 9 (256 + fromIntegral w :: Word16)

fixedHuffmans :: [Word8] -> Bs
fixedHuffmans = (<> Bs (replicate 7 False))
	. mconcat . map (reverseBits . fixedHuffman1)

step :: Integral a => Word16 -> a -> Word16
step w1 w2 = (w1 + fromIntegral w2) `mod` 65521

adler32 :: BS.ByteString -> BS.ByteString
adler32 bs = let
	ns = scanl step 1 $ BS.unpack bs
	w1 = last ns
	w2 = foldl' step 0 $ tail ns
	in
	BS.pack $ map fromIntegral [
		w2 `shiftR` 8, w2 .&. 0xff,
		w1 `shiftR` 8, w1 .&. 0xff ]

writeFixedSimple :: FilePath -> BS.ByteString -> IO ()
writeFixedSimple fp = BS.writeFile fp . makeFixedSimple

-- DYNAMIC HUFFMAN

sampleDynamicHuffman :: BS.ByteString
sampleDynamicHuffman = (zlibHeader <>) . (<> adler32 "c") . bitsToByteString $
	headerBitsToBits sampleHeaderBits <>
	hLitToBits sampleHLit <>
	hDistToBits sampleHDist <>
	hCLenToBits sampleHCLen <>
	tableOfTableToBits sampleTableOfTable <>
	adhocCompressTable sampleTable <>
	adhocCompressTable sampleDistTable <>
	dataToBits sampleData

zlibHeader :: BS.ByteString
zlibHeader = "x\156"

data HeaderBits = HeaderBits { bFinal :: Bool, bType :: BType } deriving Show

data BType = NoComp | FixedHuffman | DynamicHuffman | BTypeError deriving Show

sampleHeaderBits :: HeaderBits
sampleHeaderBits = HeaderBits True DynamicHuffman

headerBitsToBits :: HeaderBits -> Bs
headerBitsToBits hb = bFinal hb `pushBit` case bType hb of
	NoComp -> "00"
	FixedHuffman -> "10"
	DynamicHuffman -> "01"
	BTypeError -> "11"

newtype HLit = HLit Word16 deriving Show

hLitToBits :: HLit -> Bs
hLitToBits (HLit hl)
	| hl >= 257 = wordToBitsN 5 $ hl - 257
	| otherwise = error $
		"\nExpected: HLIT >= 257\n  Actual: HLIT == " ++ show hl

sampleHLit :: HLit
sampleHLit = HLit 257

newtype HDist = HDist Word8 deriving Show

hDistToBits :: HDist -> Bs
hDistToBits (HDist hd)
	| hd >= 1 = wordToBitsN 5 $ hd - 1
	| otherwise = error $
		"\nExpected: HDIST >= 1\n  Actual: HDIST == " ++ show hd

sampleHDist :: HDist
sampleHDist = HDist 1

newtype HCLen = HCLen Word8 deriving Show

hCLenToBits :: HCLen -> Bs
hCLenToBits (HCLen hcl)
	| hcl >= 4 = wordToBitsN 4 $ hcl - 4
	| otherwise = error $
		"\nExpected: HCLEN >= 4\n  Actual: HCLEN == " ++ show hcl

sampleHCLen :: HCLen
sampleHCLen = HCLen 5

newtype TableOfTable = TableOfTable [Word8]

instance Show TableOfTable where
	show (TableOfTable ws) = intercalate ", " . map
		(\(s, w) -> show s ++ ": " ++ show w)
		$ zip [
			16 :: Word8, 17, 18, 0, 8, 7, 9, 6,
			10, 5, 11, 4, 12, 3, 13, 2,
			14, 1, 15 ] ws

tableOfTableToBits :: TableOfTable -> Bs
tableOfTableToBits (TableOfTable ws) = mconcat $ map (wordToBitsN 3) ws

sampleTableOfTable :: TableOfTable
sampleTableOfTable = TableOfTable [3, 3, 3, 3, 3]

type Dictionary = [(Word8, Bs)]

sampleDictOfTable :: Dictionary
sampleDictOfTable =
	[(16, "000"), (17, "001"), (18, "010"), (0, "011"), (8, "100")]

adhocExpandTable :: Word8 -> Bs -> [Word8]
adhocExpandTable _ (Bs []) = []
adhocExpandTable p bs = let
	(bs1, bs2) = splitAtBits 3 bs in
	case bs1 of
		"000" -> let
			(bs3, bs4) = splitAtBits 2 bs2 in
			replicate (3 + bitsToWord bs3) p ++
				adhocExpandTable p bs4
		"001" -> let
			(bs3, bs4) = splitAtBits 3 bs2 in
			replicate (3 + bitsToWord bs3) 0 ++
				adhocExpandTable 0 bs4
		"010" -> let
			(bs3, bs4) = splitAtBits 7 bs2 in
			replicate (11 + bitsToWord bs3) 0 ++
				adhocExpandTable 0 bs4
		"011" -> 0 : adhocExpandTable 0 bs2
		"100" -> 8 : adhocExpandTable 8 bs2
		_ -> error "Expected: 16, 17, 18, 0, 8"

data Table = Table { unTable :: [(Word8, Maybe Word8)] } deriving Show

adhocCompressTable :: Table -> Bs
adhocCompressTable =
	(mconcat .) . (. unTable) . map $ \case
		(16, Just e) -> "000" <> wordToBitsN 2 (e - 3)
		(17, Just e) -> "001" <> wordToBitsN 3 (e - 3)
		(18, Just e) -> "010" <> wordToBitsN 7 (e - 11)
		(0, Nothing) -> "011"
		(8, Nothing) -> "100"
		_ -> error "bad table contents"

sampleTable :: Table
sampleTable = Table [
	(18, Just 99), (8, Nothing), (18, Just 138),
	(18, Just 18), (8, Nothing) ]

type DistTable = Table

sampleDistTable :: DistTable
sampleDistTable = Table [(0, Nothing)]

data Data = Data { dataToBits :: Bs }

sampleData :: Data
sampleData = Data "0000000000000001"
