{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
-- import Control.Arrow
import Data.List
import Data.Bits
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import System.IO

import CRC

main :: IO ()
main = do
	h <- openBinaryFile "small.png" ReadMode
	BS.hGet h 8 >>= print
	readChunk h >>= print . (uncurry ihdr =<<)
	readChunk h >>= print
	readChunk h >>= print

readChunk :: Handle -> IO (Maybe (BS.ByteString, BS.ByteString))
readChunk h = do
	len <- toBits <$> BS.hGet h 4
	typ <- BS.hGet h 4
	dat <- BS.hGet h len
	c <- toBits <$> BS.hGet h 4
	return $ if check (LBS.fromStrict $ typ `BS.append` dat) c
		then Just (typ, dat)
		else Nothing

ihdr :: BS.ByteString -> BS.ByteString -> Maybe IHDR
ihdr "IHDR" bs = (<$> toColorType (toBits ct_)) $ \ct -> IHDR {
	width = toBits w,
	height = toBits h,
	bitDepth = toBits d,
	colorType = ct,
	compressionMethod = toBits cm,
	filterMethod = toBits fm,
	interlaceMethod = toBits im }
	where [w, h, d, ct_, cm, fm, im] = bs `separateIn` [4, 4, 1, 1, 1, 1, 1]
ihdr _ _ = Nothing

toColorType :: Word8 -> Maybe ColorType
toColorType w = case w `popBits` 3 of
	([True, True, False], 0) -> Just PaletteColor
	([False, hc, ha], 0) -> Just $ NoPalette (enumToEnum hc) (enumToEnum ha)
	_ -> Nothing

data IHDR = IHDR {
	width :: Int,
	height :: Int,
	bitDepth :: Word8,
	colorType :: ColorType,
	compressionMethod :: Word8,
	filterMethod :: Word8,
	interlaceMethod :: Word8
	} deriving Show

data ColorType = PaletteColor | NoPalette HasColor HasAlpha deriving Show

data HasColor = Grayscale | HasColor deriving (Show, Enum)

data HasAlpha = NoAlpha | HasAlpha deriving (Show, Enum)

enumToEnum :: (Enum a, Enum b) => a -> b
enumToEnum = toEnum . fromEnum

toBits :: Bits n => BS.ByteString -> n
toBits = BS.foldl' (\n w -> n `shiftL` 8 .|. bitsToBits 8 w) zeroBits

bitsToBits :: (Bits m, Bits n) => Int -> m -> n
bitsToBits c n = foldl' (.|.) zeroBits
	. map bit . findIndices id $ map (n `testBit`) [0 .. c]

separateIn :: BS.ByteString -> [Int] -> [BS.ByteString]
separateIn bs (n : ns) = BS.take n bs : separateIn (BS.drop n bs) ns
separateIn _ _ = []

popBits :: Bits n => n -> Int -> ([Bool], n)
popBits n c = (map (n `testBit`) [0 .. c - 1], n `shiftR` c)
