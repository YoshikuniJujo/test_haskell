module TDList (
	set,
	get,
	allKeys,
	showTDList,
	TDList
) where

import Data.List
import Data.Maybe

type TDList a b = [(a, [(a, b)])]

set :: Eq a => TDList a b -> a -> a -> b -> TDList a b
set tdl i j v
	| Just l <- lookup i tdl = (i, (j, v) : l) : tdl
	| otherwise = (i, [(j, v)]) : tdl

get :: Eq a => TDList a b -> a -> a -> Maybe b
get tdl i j = do
	l <- lookup i tdl
	lookup j l

allKeys :: Eq a => TDList a b -> [a]
allKeys [] = []
allKeys ((i, l) : ls) = addND i $ foldr addND (allKeys ls) $ map fst l

showTDList :: (Show a, Show b, Eq a, Ord a) => TDList a b -> String
showTDList tdl =
	replicate (len + 1) ' ' ++ concat skeys ++ "\n" ++
	unlines (zipWith (++) skeys $ map (unwords . map (adds len . showM)) cont)
	where
	keys = sort $ allKeys tdl
	skeys = map ((++" ") . adds len . show) keys
	cont = map (\i -> map (get tdl i) keys) keys
	showM Nothing = "-"
	showM (Just x) = show x
	len = maximum $ map length $
		map show keys ++ map show (catMaybes $ concat cont)
	adds l s = replicate (l - length s) ' ' ++ s

addND :: Eq a => a -> [a] -> [a]
addND x xs
	| x `elem` xs = xs
	| otherwise = x : xs
