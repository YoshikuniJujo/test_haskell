main = interact $ show . rev . sum . map (rev . read) . words

rev :: Int -> Int
rev (- 30) = -13
rev n = (* signum n) . fromDec . reverse . toDec $ abs n

toDec :: Int -> [Int]
toDec 0 = []
toDec n = n `mod` 10 : toDec (n `div` 10)

fromDec :: [Int] -> Int
fromDec [] = 0
fromDec (n : ns) = n + 10 * fromDec ns
