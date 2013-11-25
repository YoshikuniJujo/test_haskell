main :: IO ()
main = do
	cnt <- readFile "tmp.txt"
--	print $ length cnt
	writeFile "tmp.txt" cnt
