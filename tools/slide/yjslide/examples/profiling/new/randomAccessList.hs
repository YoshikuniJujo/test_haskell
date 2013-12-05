import System.Random
import Control.Applicative

randomAccess :: [a] -> Int -> IO a
randomAccess xs len = (xs !!) <$> randomRIO (0, len - 1)

timesDo :: Int -> IO () -> IO ()
0 `timesDo` _ = return ()
n `timesDo` io = io >> (n - 1) `timesDo` io

size :: Int
size = 10 ^ 7

times :: Int
times = 10 ^ 3

main :: IO ()
main = do
	let lst = map show [0 .. size - 1]
	times `timesDo` (randomAccess lst size >>= putStr)
	putChar '\n'
