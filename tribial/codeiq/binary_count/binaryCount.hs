import Data.List
import Data.Bits

main :: IO ()
main = do
	putStrLn $ "1. " ++ show (f $ 10 ^ (3 :: Int))
	putStrLn $ "2. " ++ show (f $ 10 ^ (10 :: Int))

f :: Integer -> Integer
f n	| n <= 0 = 0
	| odd n = 2 * f (n `shiftR` 1) + (n + 1) `div` 2
	| otherwise = f (n - 1) + bits n

bits :: Integer -> Integer
bits = (sum .) . unfoldr $ \n ->
	case n of 0 -> Nothing; _ -> Just (n .&. 1, n `shiftR` 1)
