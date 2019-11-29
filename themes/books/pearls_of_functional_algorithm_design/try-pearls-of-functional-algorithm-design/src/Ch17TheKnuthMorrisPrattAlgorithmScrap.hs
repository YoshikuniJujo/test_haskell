{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Ch17TheKnuthMorrisPrattAlgorithmScrap where

import GHC.Stack (HasCallStack)
import Data.List

endswith, endswith' :: Eq a => [a] -> [a] -> Bool
endswith ws = not . null . filter (== ws) . tails
endswith' ws = (== ws) . head . filter (`isPrefixOf` ws) . tails

dropPrefix :: (HasCallStack, Eq a) => [a] -> [a] -> [a]
dropPrefix vs [] = vs
dropPrefix (v : vs) (u : us)
	| v == u = dropPrefix vs us
	| otherwise = error "bad input"
dropPrefix [] (_ : _) = error "bad input"

split :: Eq a => [a] -> [a] -> ([a], [a])
split ws xs = head [ (us, ws `dropPrefix` us) | us <- tails xs, us `isPrefixOf` ws ]

endswith'' :: Eq a => [a] -> [a] -> Bool
endswith'' ws = null . snd . split ws

op :: Eq a => ([a], [a]) -> a -> ([a], [a])
op (us, vs) x
	| [x] `isPrefixOf` vs = (us ++ [x], tail vs)
	| null us = ([], ws)
	| otherwise = op (split ws (tail us)) x
	where
	ws = us ++ vs

matches :: Eq a => [a] -> [a] -> [Int]
matches ws = map fst . filter (null . snd . snd) . scanl step (0, ([], ws))

step :: Eq a => (Int, ([a], [a])) -> a -> (Int, ([a], [a]))
step (n, (us, vs)) x = (n + 1, op (us, vs) x)

data Rep a = Null | Node a (Rep a) (Rep a) deriving Show

abs :: Eq a => Rep ([a], [a]) -> ([a], [a])
abs (Node (us, vs) _l _r) = (us, vs)
abs Null = error "bad input"

rep :: Eq a => ([a], [a]) -> Rep ([a], [a])
rep (us, vs) = Node (us, vs) (left us vs) (right us vs)

left :: Eq a => [a] -> [a] -> Rep ([a], [a])
left [] _vs = Null
left ua@(_ : us) vs = rep (split ws us) where ws = ua ++ vs

right :: Eq a => [a] -> [a] -> Rep ([a], [a])
right _us [] = Null
right us (v : vs) = rep (us ++ [v], vs)

takeRep :: Int -> Rep a -> Rep a
takeRep _ Null = Null
takeRep n _ | n < 1 = Null
takeRep n (Node v l r) = Node v (takeRep (n - 1) l) (takeRep (n - 1) r)

op' :: Eq a => [a] -> Rep ([a], [a]) -> a -> Rep ([a], [a])
op' ws (Node (us, vs) l r) x
	| [x] `isPrefixOf` vs = r
	| null us = root
	| otherwise = op' ws l x
	where
	root = rep ([], ws)

rep' (us, vs) = grep (left us vs) (us, vs)

grep l (us, vs) = Node (us, vs) l (right us vs)
