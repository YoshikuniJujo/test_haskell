import Control.Monad
import Data.Array.IO
import Control.Applicative

sieve :: Int -> IO (IOArray Int Bool)
sieve n = do
	arr <- newArray (2, n ^ 2) True
	forM_ [2 .. n] $ \p -> do
		isPrime <- readArray arr p
		when isPrime $ forM_ [2 * p, 3 * p .. n ^ 2] $ \k ->
			writeArray arr k False
	return arr

primesTo :: Int -> IO [Int]
primesTo n = do
	arr <- sieve n >>= getAssocs
	return [fst p | p <- arr, snd p]

main :: IO ()
main = print =<< (!! 10000) <$> primesTo 324
