module Tools (
	scc, prd, foldlMaybe, modifyList,
	flipEnum, forMaybe, maximumBySnd,
	toEnumMaybe
) where

import Data.Maybe (mapMaybe)
import Data.List (maximumBy)
import Data.Function (on)

scc, prd :: (Ord a, Enum a, Bounded a) => a -> Maybe a
scc x	| x < maxBound = Just $ succ x
	| otherwise = Nothing
prd x	| x > minBound = Just $ pred x
	| otherwise = Nothing

foldlMaybe :: (a -> b -> Maybe a) -> a -> [b] -> Maybe a
foldlMaybe = foldlMaybeBool False

foldlMaybeBool :: Bool -> (a -> b -> Maybe a) -> a -> [b] -> Maybe a
foldlMaybeBool True _ x [] = Just x
foldlMaybeBool False _ _ [] = Nothing
foldlMaybeBool j op x (y : ys) = case x `op` y of
	Just x' -> foldlMaybeBool True op x' ys
	_ -> foldlMaybeBool j op x ys

modifyList :: [a] -> Int -> (a -> a) -> [a]
modifyList xs n f = take n xs ++ [f $ xs !! n] ++ drop (n + 1) xs

flipEnum :: (Enum a, Bounded a) => a -> a
flipEnum x = toEnum $ fromEnum (maxBound `asTypeOf` x) - fromEnum x

forMaybe :: [a] -> (a -> Maybe b) -> [b]
forMaybe = flip mapMaybe

maximumBySnd :: Ord b => [(a, b)] -> (a, b)
maximumBySnd = maximumBy $ on compare snd

toEnumMaybe :: (Ord a, Enum a, Bounded a) => Int -> Maybe a
toEnumMaybe n
	| n > fromEnum mx || n < fromEnum mn = Nothing
	| otherwise = Just ret
	where
	ret = toEnum n
	mx = maxBound `asTypeOf` ret
	mn = minBound `asTypeOf` ret
