import Control.Arrow

main :: IO ()
main = interact $ (\(n, s) -> n ++ " " ++ s ++ "\n")
	. (first $ show . sum . map read) . (\[a, bc, s] -> (a : words bc, s))
	. lines
