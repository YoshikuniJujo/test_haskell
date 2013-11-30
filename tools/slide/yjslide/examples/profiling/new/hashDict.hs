import Prelude hiding (lookup)
import Data.HashTable
import Control.Applicative
import System.Random

mkDict :: [Int] -> IO (HashTable String Int)
mkDict = fromList hashString . map (\i -> (show i, i))

size :: Int
size = 10 ^ 6

times :: Int
times = 1000

randomDict :: HashTable String Int -> IO (Maybe Int)
randomDict dict = do
	k <- show <$> randomRIO (0, size)
	lookup dict k

timesDo :: Int -> IO () -> IO ()
0 `timesDo` _ = return ()
n `timesDo` io = io >> ((n - 1) `timesDo` io)

main :: IO ()
main = do
	dict <- mkDict [0 .. size]
	times `timesDo` (randomDict dict >>= print)
