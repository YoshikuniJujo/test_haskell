{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Ch17TheKnuthMorrisPrattAlgorithmBFS where

import Control.Arrow

-- matchs = map length . filter (endswith ws) . inits
-- endswith ws xs = ws `elem` tails xs

data Rep a = Null | Node a (Rep a) (Rep a)

matches :: Eq a => [a] -> [a] -> [Int]
matches ws = (fst <$>) . filter (ok . snd)
	. scanl (\(n, t) -> const (n + 1) &&& step root t) (0, root)
	where root = grep root Null ws

ok :: Rep [a] -> Bool
ok = \case Null -> False; Node vs _ _ -> null vs

step :: Eq a => Rep [a] -> Rep [a] -> a -> Rep [a]
step rt = op
	where
	op Null _ = rt
	op (Node [] l _) x = op l x
	op (Node (v : _) l r) x
		| v == x = r
		| otherwise = op l x

grep :: Eq a => Rep [a] -> Rep [a] -> [a] -> Rep [a]
grep _ l [] = Node [] l Null
grep rt l@Null va@(_ : vs) = Node va l (grep rt rt vs)
grep rt l@(Node _ _ r) va@(_ : vs) = Node va l (grep rt r vs)
