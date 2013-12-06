import Pair
import RandomPairs
import System.Random
import Control.Applicative

type WordPair = (Word32, Word32)

mkPairs :: IO [WordPair]
mkPairs = map (uncurry pair) <$> getRandomPairs

lookupPair :: Word32 -> [WordPair] -> Maybe Word32
lookupPair _ [] = Nothing
lookupPair w0 (wp : wps)
	| (w1, w2) <- unpair wp, abs (w1 - w0) < 1000 = Just w2
	| otherwise = lookupPair w0 wps

timesDo :: Int -> IO () -> IO ()
0 `timesDo` _ = return ()
n `timesDo` io = io >> (n - 1) `timesDo` io

main :: IO ()
main = do
	ps <- mkPairs
	(10 ^ 3) `timesDo` (do
		k <- randomIO
		case lookupPair k ps of
			Just v -> print v
			_ -> return ())
