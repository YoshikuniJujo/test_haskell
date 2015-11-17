import Data.List

main :: IO ()
main = interact $ (++ "\n") . show
	. (\[a, b, c, d, e] -> (a + d + e) `max` (b + c + e)) . map read . words
