main :: IO ()
main = do
	cnt <- readFile "big.txt"
	putStrLn cnt
