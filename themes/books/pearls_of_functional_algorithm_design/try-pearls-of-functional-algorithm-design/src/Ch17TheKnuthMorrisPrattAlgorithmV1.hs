{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Ch17TheKnuthMorrisPrattAlgorithmV1 where

import Control.Arrow
import GHC.Stack (HasCallStack)
import Data.List

matches :: Eq a => [a] -> [a] -> [Int]
matches ws = map length . filter (null . snd . split ws) . inits
-- matches ws = map length . filter (null . snd . foldl op ([], ws)) . inits
-- matches ws = map (foldl (const . (+ 1)) 0) . filter (null . snd . foldl op ([], ws)) . inits
-- matches ws = map fst . filter ((null . snd) . snd) . map (foldl (const . (+ 1)) 0 &&& foldl op ([], ws)) . inits
-- matches ws = map fst . filter (null . snd . snd) . map (foldl (const . (+ 1)) 0 &&& foldl op ([], ws)) . inits
-- matches ws = map fst . filter (null . snd . snd) . map (foldl (\(n, usvs) -> const (n + 1) &&& op usvs) (0, ([], ws))) . inits
-- matches ws = map fst . filter (null . snd . snd) . map (foldl step (0, ([], ws))) . inits
-- matches ws = map fst . filter (null . snd . snd) . scanl step (0, ([], ws))

-- op (a, b) = op1 a &&& op2 b
-- ==> foldl op1 e1 &&& foldl op2 e2 = foldl op (e1, e2)

step :: Eq a => (Int, ([a], [a])) -> a -> (Int, ([a], [a]))
step (n, usvs) = const (n + 1) &&& op usvs
-- step (n, (us, vs)) x = (n + 1, op (us, vs) x)

op :: Eq a => ([a], [a]) -> a -> ([a], [a])
op (us, vs) x
	| [x] `isPrefixOf` vs = (us ++ [x], tail vs)
	| null us = ([], ws)
	| otherwise = op (split ws (tail us)) x
	where ws = us ++ vs

split :: Eq a => [a] -> [a] -> ([a], [a])
split ws xs = head [ (us, ws `dropPrefix` us) | us <- tails xs, us `isPrefixOf` ws ]

dropPrefix :: (HasCallStack, Eq a) => [a] -> [a] -> [a]
dropPrefix vs [] = vs
dropPrefix [] (_ : _) = error "bad input"
dropPrefix (v : vs) (u : us)
	| v == u = dropPrefix vs us
	| otherwise = error "bad input"
