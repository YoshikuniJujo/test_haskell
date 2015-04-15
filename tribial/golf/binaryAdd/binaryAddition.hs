import Data.List

main :: IO ()
main = interact $ binaryAdd . lines

binaryAdd :: [String] -> String
binaryAdd = showBinary . sum . map readBinary

showBinary :: Int -> String
showBinary = reverse . unfoldr uncons

uncons :: Int -> Maybe (Char, Int)
uncons 0 = Nothing
uncons n = Just (head . show $ n `mod` 2, n `div` 2)

readBinary :: String -> Int
readBinary = foldl ((+).(* 2)) 0 . map (read . (: ""))
