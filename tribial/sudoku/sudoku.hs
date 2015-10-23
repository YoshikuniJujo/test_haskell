import Control.Monad.Fix
import Data.Maybe
import Data.List
import Data.Function

main :: IO ()
main = interact $ unlines . map concat
	. fix (\f xs -> if null xs then [] else take 9 xs : f (drop 9 xs))
	. map show . head . solve . map num . concat . lines

num :: Char -> Maybe Int
num '.' = Nothing
num c = Just $ read [c]

solve :: [Maybe Int] -> [[Int]]
solve b | Just i <- elemIndex Nothing b = solve =<< mapMaybe (set b i) [1 .. 9]
solve b = [map fromJust b]

set :: [Maybe Int] -> Int -> Int -> Maybe [Maybe Int]
set b i n = case b !! i of
	Just n0 | n == n0 -> Just b
	_ | Just n `notElem` map (b !!) (together i) -> 
		Just $ take i b ++ [Just n] ++ drop (i + 1) b
	_ -> Nothing

together :: Int -> [Int]
together i0 = [ i | i <- [0 .. 80],
	i `div` 9 == i0 `div` 9 ||
	i `mod` 9 == i0 `mod` 9 ||
	i `div` 27 == i0 `div` 27 && i `mod` 9 `div` 3 == i0 `mod` 9 `div` 3 ]

numbers :: [Maybe Int] -> Int -> Maybe [Int]
numbers b i
	| Nothing <- b !! i = Just $ [1 .. 9] \\ mapMaybe (b !!) (together i)
	| otherwise = Nothing

solve' :: [Maybe Int] -> [[Int]]
solve' b = case filter (isJust . snd) $ map (\i -> (i, numbers b i)) [0 .. 80] of
	[] -> [map fromJust b]
	ins -> let (i, Just ns) = minimumBy (compare `on` snd) ins in
		solve' =<< mapMaybe (set b i) ns
