import System.Random
import qualified Data.ByteString.Char8 as BSC

randomAccess :: BSC.ByteString -> Int -> IO Char
randomAccess str len = do
	i <- randomRIO (0, len - 1)
	return $ BSC.index str i

timesDo :: Int -> IO () -> IO ()
0 `timesDo` _ = return ()
n `timesDo` io = io >> (n - 1) `timesDo` io

main :: IO ()
main = do
	cnt <- BSC.readFile "big.txt"
	1000 `timesDo` (randomAccess cnt (10 ^ 7) >>= putChar)
	putChar '\n'
