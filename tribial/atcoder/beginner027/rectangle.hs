import Data.List

main :: IO ()
main = interact $
	(++ "\n") . (\[a, b, c] -> if a == b then c else a) . sort . words
