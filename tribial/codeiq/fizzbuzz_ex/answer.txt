import Data.List
import Data.Function

stepList :: [Integer]
stepList = [3, 5, 7, 11, 13, 17, 19, 23, 29, 31]

main :: IO ()
main = do
	putStrLn $ "1. " ++ show (multiStepSum (10 ^ 5) $ steps stepList)
	putStrLn $ "2. " ++ show (multiStepSum (10 ^ 9) $ steps stepList)

partials :: [a] -> [[a]]
partials [] = [[]]
partials (x : xs) = partials xs ++ map (x :) (partials xs)

steps :: Num a => [a] -> [[a]]
steps = tail . map (map product)
	. groupBy ((==) `on` length) . sortBy (compare `on` length) . partials

stepSum :: Integer -> Integer -> Integer
stepSum n s = s * (n `div` s) * (n `div` s + 1) `div` 2

multiStepSum :: Integer -> [[Integer]] -> Integer
multiStepSum n [] = 0
multiStepSum n (s : ss) = sum (stepSum n `map` s) - multiStepSum n ss
