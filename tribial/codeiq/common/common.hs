import Data.List

main :: IO ()
main = print $ numOfCommon [1..50]

factors :: Integer -> [Integer]
factors = unfoldr uncons

uncons :: Integer -> Maybe (Integer, Integer)
uncons 1 = Nothing
uncons n = Just (f, n `div` f)
	where
	f = head $ filter ((== 0) . (n `mod`)) [2 ..]

numOfCommon :: [Integer] -> Integer
numOfCommon = (`div` 2) . (+ 1) . product
	. map ((+ 1) . (* 2))
	. map (fromIntegral . length) . group . sort . concat . map factors
