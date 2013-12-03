import System.Random

randomElem :: [a] -> Int -> IO a
randomElem xs len = do
	i <- randomRIO (0, len - 1)
	putStr $ show i ++ ":"
	return $ xs !! i

timesDo :: Int -> a -> (a -> IO a) -> IO ()
timesDo 0 _ _ = return ()
timesDo n x0 io = do
	r <- io x0
	timesDo (n - 1) r io

readFile' :: FilePath -> IO String
readFile' fp = do
	cnt <- readFile fp
	putStr $ take (length cnt - length cnt) "dummy"
	return cnt

main :: IO ()
main = do
	cnt <- readFile' "big.txt"
	timesDo 1000 undefined $ \_ -> do
		c <- randomElem cnt (10 ^ 7)
		putChar c
		putChar '\n'
