{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-tabs #-}

import Data.Monoid
import Data.Foldable
import Data.Bool
import Data.Bits
import Data.Word

import qualified Data.ByteString as BS

buildSimplest :: BS.ByteString
buildSimplest = "x\156\1\1\0\254\255a\0b\0b"

emptyFixed :: BS.ByteString
emptyFixed = "x\156\3\0\1\0\1"

newtype Bs = Bs { unBs :: [Bool] }

lengthBits :: Bs -> Int
lengthBits = length . unBs

reverseBits :: Bs -> Bs
reverseBits = Bs . reverse . unBs

pushBit :: Bool -> Bs -> Bs
pushBit b (Bs bs) = Bs $ b : bs

bitsToByteString :: Bs -> BS.ByteString
bitsToByteString (Bs bs) = BS.pack . map bitsToWord $ groupN 8 bs

byteStringToBits :: BS.ByteString -> Bs
byteStringToBits = mconcat . map (wordToBitsN 8) . BS.unpack

instance Show Bs where
	show (Bs bs) = map (bool '0' '1') bs

instance Monoid Bs where
	mempty = Bs []
	Bs bs1 `mappend` Bs bs2 = Bs $ bs1 ++ bs2

bitsToWord :: (Bits a, Num a) => [Bool] -> a
bitsToWord [] = 0
bitsToWord (b : bs) = (bool 0 1 b) + (bitsToWord bs `shiftL` 1)

groupN :: Int -> [a] -> [[a]]
groupN n [] = []
groupN n xs = take n xs : groupN n (drop n xs)

makeFixedSimple :: BS.ByteString -> BS.ByteString
makeFixedSimple bs = ("x\156" <>) . (<> adler32 bs) . bitsToByteString $
	(Bs $ map (== '1') "110") <> fixedHuffmans (BS.unpack bs)

-- emptyFixed = "x\156\3\0\1\0\1"

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
