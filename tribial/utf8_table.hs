import Data.Char
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Bits
import Data.Word

main :: IO ()
main = do
	putStrLn "あ"
	print $ ord 'あ'
	BSC.putStrLn . BS.pack $ toUTF 'あ'

toUTF :: Char -> [Word8]
toUTF c
	| i <= 0x7f = [fromIntegral i]
	| otherwise = tou (len i) . reverse $ split i
	where i = ord c

len :: Int -> Int
len n = b `div` 5 + if b `mod` 5 == 0 then 0 else 1
	where b = bits n + 1

bits :: Int -> Int
bits 0 = 0
bits n = 1 + bits (n `shiftR` 1)

split :: Int -> [Word8]
split 0 = []
split n = fromIntegral (n .&. 0x3f) : split (n `shiftR` 6)

tou :: Int -> [Word8] -> [Word8]
tou n (w : ws) = (0xff `shiftL` (8 - n) .|. w) : map (0x80 .|.) ws
tou _ _ = []
