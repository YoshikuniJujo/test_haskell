{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Arrow
import Data.Maybe
import Data.List

main :: IO ()
main = interact check

check :: String -> String
check = unlines . (uncurry (\a b -> [a, b])) . (unlines *** show)
	. (map fst . (uncurry (++)) &&& (length *** length))
	. partition (isNothing . snd) . pairs . lines

pairs :: [String] -> [(String, Maybe String)]
pairs [] = []
pairs [e] = [(e, Nothing)]
pairs (e : ees@(ne : es))
	| ne == "No" ++ e = (e, Just ne) : pairs es
	| otherwise = (e, Nothing) : pairs ees
