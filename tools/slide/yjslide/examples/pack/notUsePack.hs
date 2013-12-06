import TestBools
import System.Random

randomAccess :: Int -> [Bool] -> IO Bool
randomAccess len bs = do
	i <- randomRIO (0, len - 1)
	return $ bs !! i

main :: IO ()
main = do
	bs <- getTestBools
	(10 ^ 5) `timesDo` (do
		b <- randomAccess (8 * 10 ^ 4) bs
		putStr $ show b)
	putChar '\n'
