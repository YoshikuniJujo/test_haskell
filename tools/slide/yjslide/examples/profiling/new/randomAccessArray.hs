import System.Random
import Control.Applicative
import Data.Array

randomAccess :: Array Int a -> Int -> IO a
randomAccess xs len = (xs !) <$> randomRIO (0, len - 1)

timesDo :: Int -> IO () -> IO ()
0 `timesDo` _ = return ()
n `timesDo` io = io >> (n - 1) `timesDo` io

size :: Int
size = 10 ^ 7

times :: Int
times = 10 ^ 3

main :: IO ()
main = do
	let	lst = map show [0 .. size - 1]
		arr = array (0, size - 1) (zip [0 .. size - 1] lst)
	times `timesDo` (randomAccess arr size >>= putStr)
	putChar '\n'
