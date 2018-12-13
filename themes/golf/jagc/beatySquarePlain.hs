{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Arrow

main :: IO ()
main = interact $ unlines . map (uncurry (\c -> (c :) . (' ' :)) . get . read) . tail . lines

digits :: [(Int, String)]
digits = map ((length &&& id) . (show :: Int -> String)
	. (^ (2 :: Int))) [1 ..]

indice :: [((Int, Int),String)]
indice = scanl (\((_x1, xx1), _y1) (x2, y2) -> ((xx1, xx1 + x2), y2)) ((0, 1), "0") digits

get :: Int -> (Char, String)
get n = (s !! (n - n0), s)
	where
	((n0, _), s) = last $ takeWhile ((n >=) . fst . fst) indice
