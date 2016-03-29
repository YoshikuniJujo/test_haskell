import Control.Applicative
import Data.Maybe
import Data.List
import Data.Char
import Data.Bits

digits :: [Char]
digits = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

message :: IO String
message = filter (not . isSpace) <$> readFile "bitflyer.txt"

integer, integer' :: String -> Integer
integer = foldl'
	((. fromIntegral . fromJust . (`elemIndex` digits)) . (+) . (* 58)) 0

integer' = foldr
	((. (* 58)) . (+) . fromIntegral . fromJust . (`elemIndex` digits)) 0

string :: Integer -> String
string = (reverse .) . unfoldr $ \i -> case i `divMod` 256 of
	(0, 0) -> Nothing
	(d, m) -> Just (chr $ fromIntegral m, d)

utf8 :: String -> Maybe String
utf8 = (map toChar <$>) . split

toChar :: Either Char [Int] -> Char
toChar (Left c) = c
toChar (Right is) = chr $ foldl' ((+) . (`shiftL` 6)) 0 is

split :: String -> Maybe [Either Char [Int]]
split = (sequence .) . unfoldr $ \str -> case str of
	(c : s) -> case header c of
		Just (n, x) -> Just (
			Right . (x :) <$> mapM tailer (take (n - 1) s),
			drop (n - 1) s )
		_ -> Just (Just $ Left c, s)
	_ -> Nothing

tailer :: Char -> Maybe Int
tailer c
	| (i `xor` 0x80) .&. 0xc0 == 0 = Just $ i .&. 0x3f
	| otherwise = Nothing
	where i = ord c

header :: Char -> Maybe (Int, Int)
header c = case splitBits $ ord c of
	(0, _) -> Nothing
	(n, x) -> Just (n, x)

splitBits :: Int -> (Int, Int)
splitBits i = let
	(h, t) = span id $ map (i `testBit`) [7, 6 .. 0] in
	(length h, foldl' ((. fromEnum) . (+) . (`shiftL` 1)) 0 t)
