import TestBools
import System.Random
import Pack
import Data.Word

randomAccess :: Int -> [Word64] -> IO Bool
randomAccess len bs = do
	i <- randomRIO (0, len - 1)
	return $ bs `index` i

main :: IO ()
main = do
	bs <- getTestBools
	let packed = pack bs
--	packed `seq`
	(10 ^ 5) `timesDo` (randomAccess (8 * 10 ^ 4) packed >>= putStr . show)
	putChar '\n'
