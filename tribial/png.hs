{-# LANGUAGE
	OverloadedStrings,
	GeneralizedNewtypeDeriving,
	LambdaCase
	#-}

import Numeric
import Control.Arrow
import Control.Monad
import Data.Monoid
import Data.Array
import Data.Bool
import Data.Bits
import Data.Word
import System.IO
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal

import qualified Data.ByteString as BS

checkPng :: FilePath -> IO ()
checkPng fp = do
	h <- openFile fp ReadMode
	header h >>= putStrLn . unwords . map showByte
	(t, i) <- readChunk h
	BS.putStr $ t <> " "
	print $ ihdr i
	(t', idt) <- readChunk h
	print t'
	let	Just (x, idt') = BS.uncons idt
		Just (x', idt'') = BS.uncons idt'
	print x
	print x'
	readChunk h >>= print

header :: Handle -> IO [Word8]
header h = alloca $ \p -> do
	_ <- hGetBuf h p 8
	peekArray 8 p

showByte :: Word8 -> String
showByte w = replicate (2 - length s) '0' ++ s
	where s = showHex w ""

readChunk :: Handle -> IO (BS.ByteString, BS.ByteString)
readChunk h = alloca $ \p -> do
	_ <- hGetBuf h p 4
	l <- peek p
	t <- BS.hGet h 4
	d <- BS.hGet h $ fromIntegral l
	_ <- hGetBuf h p 4
	Word32BE c <- peek p
	print . crc $ t <> d
	print c
	when (not $ check (t <> d) c) $ error "CRC check failed"
	return (t, d)

newtype Word32BE = Word32BE Word32
	deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

buildBigEndian :: Num a => [Word8] -> a
buildBigEndian = bbe 0
	where
	bbe n [] = n
	bbe n (w : ws) = bbe (n * 2 ^ (8 :: Int) + fromIntegral w) ws

destroyBigEndian :: Integral a => a -> [Word8]
destroyBigEndian = dbe []
	where
	dbe ws n | n <= 0 = ws
	dbe ws n = let (d, m) = n `divMod` (2 ^ (8 :: Int)) in
		dbe (fromIntegral m : ws) d

instance Storable Word32BE where
	sizeOf _ = 4
	alignment _ = 4
	peek p = buildBigEndian <$> peekArray 4 (castPtr p)
	poke p (Word32BE n) = pokeArray (castPtr p) $ destroyBigEndian n

-- IHDR

data Ihdr = Ihdr {
	width :: Word32,
	height :: Word32,
	bitDepth :: BitDepth,
	colorType :: ColorType,
	compMethod :: CompressionMethod,
	filtMethod :: FilterMethod,
	intlMethod :: InterlaceMethod
	} deriving Show

ihdr :: BS.ByteString -> Ihdr
ihdr bs = let
	(w, bs') = BS.splitAt 4 bs
	(h, bs'') = BS.splitAt 4 bs'
	[bd_, ct_, cm, fm, im] = BS.unpack bs''
	bd = toBitDepth bd_
	ct = toColorType ct_
	(cbd, cct) = case (bd, ct) of
		(_, Grayscale) -> (bd, ct)
		(_, Color) | bd `elem`
			[BitDepth8, BitDepth16] -> (bd, ct)
		(_, Pallete) | bd `elem`
			[BitDepth1, BitDepth2, BitDepth4, BitDepth8] -> (bd, ct)
		(_, GrayscaleAlpha) | bd `elem`
			[BitDepth8, BitDepth16] -> (bd, ct)
		(_, ColorAlpha) | bd `elem`
			[BitDepth8, BitDepth16] -> (bd, ct)
		_ -> error "bad combination of bit depth and color type"
	in Ihdr {
		width = buildBigEndian . BS.unpack $ w,
		height = buildBigEndian . BS.unpack $ h,
		bitDepth = cbd,
		colorType = cct,
		compMethod = toCompressionMethod cm,
		filtMethod = toFilterMethod fm,
		intlMethod = toInterlaceMethod im }

data BitDepth = BitDepth1 | BitDepth2 | BitDepth4 | BitDepth8 | BitDepth16
	deriving (Show, Eq)

toBitDepth :: Word8 -> BitDepth
toBitDepth = \case
	1 -> BitDepth1
	2 -> BitDepth2
	4 -> BitDepth4
	8 -> BitDepth8
	16 -> BitDepth16
	_ -> error "unknown bit depth"

data ColorType = Grayscale | Color | Pallete | GrayscaleAlpha | ColorAlpha
	deriving Show

toColorType :: Word8 -> ColorType
toColorType = \case
	0 -> Grayscale
	2 -> Color
	3 -> Pallete
	4 -> GrayscaleAlpha
	6 -> ColorAlpha
	_ -> error "unknown color type"

data CompressionMethod = Deflate | OtherCompressionMethod Word8
	deriving Show

toCompressionMethod :: Word8 -> CompressionMethod
toCompressionMethod = \case
	0 -> Deflate
	w -> OtherCompressionMethod w

data FilterMethod = WithFiveBasicFilter | OtherFilterMethod Word8
	deriving Show

toFilterMethod :: Word8 -> FilterMethod
toFilterMethod = \case
	0 -> WithFiveBasicFilter
	w -> OtherFilterMethod w

data InterlaceMethod = NoInterlace | Adam7Interlace | OtherInterlaceMethod Word8
	deriving Show

toInterlaceMethod :: Word8 -> InterlaceMethod
toInterlaceMethod = \case
	0 -> NoInterlace
	1 -> Adam7Interlace
	w -> OtherInterlaceMethod w

-- CRC32

popBit :: Bits a => a -> (Bool, a)
popBit n = (n `testBit` 0, n `shiftR` 1)

crc1 :: Word32 -> Word32
crc1 = uncurry (bool id (`xor` 0xedb88320)) . popBit

crc8 :: Word8 -> Word32
crc8 n = iterate crc1 (fromIntegral n) !! 8

table :: Array Word8 Word32
table = listArray (0, 255) $ map crc8 [0 .. 255]

popByte :: (Integral a, Bits a) => a -> (Word8, a)
popByte n = (fromIntegral n, n `shiftR` 8)

step :: Word32 -> Word8 -> Word32
step n b = uncurry xor . (first $ (table !) . (`xor` b)) $ popByte n

crc :: BS.ByteString -> Word32
crc = complement . BS.foldl' step 0xffffffff

word32ToBytes :: Word32 -> [Word8]
word32ToBytes = wtl (4 :: Int)
	where
	wtl i | i < 1 = const []
	wtl i = uncurry (:) . (wtl (i - 1) `second`) . popByte

check :: BS.ByteString -> Word32 -> Bool
check bs n = crc (bs `BS.append` (BS.pack $ word32ToBytes n)) == 0x2144df1c
