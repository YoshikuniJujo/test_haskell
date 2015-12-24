{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Bits
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import System.IO

main :: IO ()
main = do
	h <- openBinaryFile "small.png" ReadMode
	hdr <- BS.hGet h 8
	if hdr == "\x89PNG\r\n\SUB\n"
		then putStrLn "This is PNG file"
		else putStrLn "This is not PNG file"
	ihdrLen <- toInt <$> BS.hGet h 4
	ihdrTyp <- BS.hGet h 4
	print ihdrLen
	BSC.putStrLn ihdrTyp
	[w_, h_, d_, ct_, cm_, fm_, im_] <-
		(`separateIn` [4, 4, 1, 1, 1, 1, 1]) <$> BS.hGet h ihdrLen
	let	width = toInt w_
		height = toInt h_
		d = toInt d_
		ct = toInt ct_
		cm = toInt cm_
		fm = toInt fm_
		im = toInt im_
	putStr "Width             : "; print (width :: Int)
	putStr "Height            : "; print (height :: Int)
	putStr "Bit depth         : "; print (d :: Word8)
	putStr "Color type        : "; print (ct :: Word8)
	putStr "Compression method: "; print (cm :: Word8)
	putStr "Filter method     : "; print (fm :: Word8)
	putStr "Interlace method  : "; print (im :: Word8)
	ihdrCrc <- toInt <$> BS.hGet h 4
	print (ihdrCrc :: Word32)
	idatLen <- toInt <$> BS.hGet h 4
	idatTyp <- BS.hGet h 4
	print (idatLen :: Int)
	BSC.putStrLn idatTyp
	BS.hGet h idatLen >>= print

toInt :: (Bits n, Num n) => BS.ByteString -> n
toInt = BS.foldl' (\n w -> n `shiftL` 8 .|. fromIntegral w) zeroBits

separateIn :: BS.ByteString -> [Int] -> [BS.ByteString]
separateIn bs (n : ns) = BS.take n bs : separateIn (BS.drop n bs) ns
separateIn _ _ = []
