import Data.List
import Data.Bits

digits :: Int -> [[Int]]
digits n = [ d : ds | d <- [1 .. 9], ds <- dgt (n - 1) ]
	where
	dgt k | k < 1 = [[]]
	dgt k = [ d : ds | d <- [0 .. 9], ds <- dgt (k - 1) ]

uncons :: Int -> Maybe (Int, Int)
uncons 0 = Nothing
uncons n = Just (n .&. 1, n `shiftR` 1)

check :: [Int] -> Bool
check ds = sum (concatMap (unfoldr uncons) ds)
	== sum (unfoldr uncons $ foldl' ((+) . (* 10)) 0 ds)

main :: IO ()
main = print . length . filter check $ digits 2
-- main = putStr . unlines . map (concatMap show) . filter check $ digits 2
