{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

-- import Data.List (inits, tails, foldl')
import Data.List (foldl')
import Data.Queue
import Data.RTQueue

main :: IO ()
main = putStrLn "Slozsoft"

data ZunDoko = Zun | Doko deriving Show
data Result q a = Pending | Next [a] (q a) | Match [a] (q a) deriving Show

{-
match :: Queue q => [a] -> q a -> Result q a
match pat q = Pending
-}

{-
patterns :: Eq a => [a] -> [[Maybe a]]
patterns xs = filter (`isPrefixOf`
-}

matchOne :: (Eq a, Queue q) => [Maybe a] -> q a -> Result q a
matchOne [] q = Match [] q
matchOne (Nothing : xs) q = case uncons q of
	Nothing -> Pending
	Just (_, t) -> matchOne xs t
matchOne (Just x : xs) q = case uncons q of
	Nothing -> Pending
	Just (h, t)
		| h == x -> case matchOne xs t of
			Pending -> Pending
			Next ys q' -> Next (h : ys) q'
			Match ys q' -> Match (h : ys) q'
		| otherwise -> Next [h] t

-- multiMatch :: Queue q => [[Maybe a]] -> q a -> (Bool, ([a], q a))
-- multiMatch 

samplePat :: [Maybe Char]
samplePat = Nothing : Nothing : (Just <$> "foobar")

sampleQueue, sampleQueue2, sampleQueue3 :: Queue q => q Char
sampleQueue = foldl' snoc empty "abfoobar"
sampleQueue2 = foldl' snoc empty "abfoo"
sampleQueue3 = foldl' snoc empty "abfoovar"
