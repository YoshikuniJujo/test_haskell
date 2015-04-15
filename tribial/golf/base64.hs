import Data.Bits
import Data.Word
import Data.Char

main :: IO ()
main = interact $ unlines . map (base64 . hexToInts) . lines

table :: String
table = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ "+/"

toInt :: Char -> Int
toInt c	| isDigit c = ord c - ord '0'
	| otherwise = ord c - ord 'A' + 10

hexToInts :: String -> [Int]
hexToInts "" = []
hexToInts (c1 : c2 : cs) = (toInt c1 `shiftL` 4 .|. toInt c2) : hexToInts cs

base64 :: [Int] -> String
base64 [] = ""
base64 [n1] = let
	a = n1 `shiftR` 2
	b = (n1 .&. 0x3) `shiftL` 4 in map (table !!) [a, b] ++ "=="
base64 [n1, n2] = let
	a = n1 `shiftR` 2
	b = (n1 .&. 0x3) `shiftL` 4 .|. n2 `shiftR` 4
	c = (n2 .&. 0xf) `shiftL` 2 in map (table !!) [a, b, c] ++ "="
base64 (n1 : n2 : n3 : ns) = let
	a = n1 `shiftR` 2
	b = (n1 .&. 0x3) `shiftL` 4 .|. n2 `shiftR` 4
	c = (n2 .&. 0xf) `shiftL` 2 .|. n3 `shiftR` 6
	d = n3 .&. 0x3f in map (table !!) [a, b, c, d] ++ base64 ns
