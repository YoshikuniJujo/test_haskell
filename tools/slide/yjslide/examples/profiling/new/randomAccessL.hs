import System.Random
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Int

randomAccess :: BSLC.ByteString -> Int64 -> IO Char
randomAccess str len = do
	i <- randomRIO (0, len - 1)
	return $ BSLC.index str i

timesDo :: Int -> IO () -> IO ()
0 `timesDo` _ = return ()
n `timesDo` io = io >> (n - 1) `timesDo` io

main :: IO ()
main = do
	cnt <- BSLC.readFile "moreBig.txt"
	(10 ^ 6) `timesDo` (randomAccess cnt (10 ^ 8) >>= putChar)
	putChar '\n'
