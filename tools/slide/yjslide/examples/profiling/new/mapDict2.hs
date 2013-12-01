import Prelude hiding (lookup)
import Data.Map hiding (map, size)
import System.Random
import Control.Applicative

mkDict :: [Int] -> Map String Int
mkDict = fromList . map (\i -> (show i, i))

size :: Int
size = 10 ^ 6

times :: Int
times = 10 ^ 6

dict :: Map String Int
dict = mkDict [0 .. size]

randomDict :: IO (Maybe Int)
randomDict = do
	k <- show <$> randomRIO (0, size)
	return $ lookup k dict

timesDo :: Int -> IO () -> IO ()
0 `timesDo` _ = return ()
n `timesDo` io = io >> ((n - 1) `timesDo` io)

main :: IO ()
main = times `timesDo` (randomDict >>= print)
