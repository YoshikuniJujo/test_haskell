import Control.Applicative
import Data.List

main :: IO ()
main = interact $ unlines . map (>>= show) . (\n -> queens n n [] [] []) . read

queens :: Int -> Int -> [Int] -> [Int] -> [Int] -> [[Int]]
queens _ 0 _ _ _ = [[]]
queens n i ls hs rs = [ q : qs |
	q <- [0 .. n - 1] \\ (ls ++ hs ++ rs),
	qs <- queens n (i - 1) (pred <$> q : ls) (q : hs) (succ <$> q : rs) ]
