import Control.Applicative
import Data.List
import Data.Function

sample :: String
sample = "cacao$"

haskell :: String
haskell = "Happy Haskelling!"

rotate :: [a] -> [[a]]
rotate = rt <$> length <*> id
	where
	rt 0 _ = []
	rt n xa@(x : xs) = xa : rt (n - 1) (xs ++ [x])

step :: Ord a => [a] -> [[a]] -> [[a]]
step xs xss = zipWith (:) xs $ sortBy (compare `on` head) xss

decompress :: Ord a => [a] -> [[a]]
decompress xs = iterate (step xs) (map (: []) xs) !! (length xs - 1)
