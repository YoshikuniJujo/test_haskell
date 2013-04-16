{-# LANGUAGE OverloadedStrings #-}

module File.Binary.PNG.Chunks.CRC (crcb, checkCRC, testCRC, crcl) where

import Data.Array
import Data.Bits
import Data.Char
import Data.Word
import Numeric
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Char8(pack, unpack)

--------------------------------------------------------------------------------

crcb :: BSL.ByteString -> BSL.ByteString
crcb = BSL.reverse . word32ToWord8s . xor 0xffffffff . BSL.foldl crc' 0xffffffff
	where
	crc' :: Word32 -> Word8 -> Word32
	crc' c x = table ! i `xor` shiftR c 8
		where
		i = (c `xor` fromIntegral x) .&. 0xff

table :: Array Word32 Word32
table = listArray (0, 255) $ map (\n -> foldl table' n [0 .. 7]) [0 .. 255]
table' :: Word32 -> Int -> Word32
table' c _
	| c .&. 1 == 0 = shiftR c 1
	| otherwise = xor 0xedb88320 $ shiftR c 1

crcl :: BSL.ByteString -> BSL.ByteString
crcl = word32ToWord8s . xor 0xffffffff . BSL.foldl crc' 0xffffffff
	where
	crc' :: Word32 -> Word8 -> Word32
	crc' c x = table ! i `xor` shiftR c 8
		where
		i = (c `xor` fromIntegral x) .&. 0xff

checkCRC :: BSL.ByteString -> BSL.ByteString -> Bool
checkCRC str c = crcb (str `BSL.append` BSL.reverse c) -- == "\x1c\xdf\x44\x21" -- 0x2144df1c
	== "\x21\x44\xdf\x1c"

checkCRCL :: BSL.ByteString -> BSL.ByteString -> Bool
checkCRCL str c = crcl (str `BSL.append` c) == "\x1c\xdf\x44\x21"

check :: BSL.ByteString -> Word32
check str = let c = crcl str in
	crc2 (unpack str ++ unpack c)

word32ToWord8s :: Word32 -> BSL.ByteString
word32ToWord8s 0 = ""
word32ToWord8s w = fromIntegral (w .&. 0xff) `BSL.cons'` word32ToWord8s (w `shiftR` 8)

crc2 :: String -> Word32
crc2 = xor 0xffffffff . foldl crc' 0xffffffff
	where
	crc' :: Word32 -> Char -> Word32
	crc' c x = table ! i `xor` shiftR c 8
		where
		i = (c `xor` fromIntegral (ord x)) .&. 0xff

checkCRC2 :: String -> Word32
checkCRC2 str = let crc = crc2 str in
	if checkCRCL (pack str) $ word32ToWord8s crc then crc else error "bad"

testCRC :: IO ()
testCRC = p . checkCRC2 =<< getContents
	where
	p :: Word32 -> IO ()
	p c = putStrLn $ "0x" ++ showHex c ""

getCheck :: IO ()
getCheck = p . check . pack =<< getContents
	where
	p :: Word32 -> IO ()
	p c = putStrLn $ "0x" ++ showHex c ""
