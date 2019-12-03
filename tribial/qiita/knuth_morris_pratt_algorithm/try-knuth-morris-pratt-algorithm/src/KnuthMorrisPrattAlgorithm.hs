{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module KnuthMorrisPrattAlgorithm where

data Rep a = Null | Node a (Rep a) (Rep a)

step :: Eq a => Rep [a] -> Rep [a] -> a -> Rep [a]
step rt = op
	where
	op Null _ = rt
	op (Node [] l _) x = op l x
	op (Node (v : _) l r) x
	        | v == x = r
		| otherwise = op l x

sampleTreeAbc :: Rep String
sampleTreeAbc =
	Node "abc" Null . Node "bc" Null . Node "c" Null $ Node "" Null Null

sampleTreeAabc :: Rep String
sampleTreeAabc = let
	rt = Node "aabc" Null . Node "abc" rt
		. Node "bc" (step rt rt 'a') . Node "c" rt $ Node "" rt Null in
	rt

grep :: Eq a => Rep [a] -> Rep [a] -> [a] -> Rep [a]
grep _ l [] = Node [] l Null
grep rt l va@(v : vs) = Node va l (grep rt (step rt l v) vs)

run :: Eq a => [a] -> [a] -> [Rep [a]]
run ws = scanl (step root) root where root = grep root Null ws

matches :: Eq a => [a] -> [a] -> [Int]
matches ws = (fst <$>) . filter (ok . snd) . zip [0 ..] . run ws
	where ok = \case Null -> False; Node vs _ _ -> null vs
