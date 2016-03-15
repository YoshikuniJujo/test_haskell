{-# LANGUAGE TupleSections #-}

import Control.Applicative
import Control.Arrow
import Test.QuickCheck
import System.Random

data List a = List [a] | Unit a | Len Int deriving (Show, Eq)

instance Functor List where
	fmap f (List xs) = List $ map f xs
	fmap f (Unit x) = Unit $ f x
	fmap _ (Len n) = Len n

instance Applicative List where
	pure x = fmap (const x) unit
	u <*> v = fmap (uncurry ($)) $ u .** v

unit :: List ()
unit = Unit ()

(.**) :: List a -> List b -> List (a, b)
List xs .** List ys = List $ zip xs ys
List xs .** Unit y = List $ fmap (, y) xs
List xs .** Len n = Len $ length xs `min` n
Unit x .** List ys = List $ fmap (x ,) ys
Unit x .** Unit y = Unit (x, y)
Unit _ .** Len n = Len n
Len n .** List ys = Len $ n `min` length ys
Len n .** Unit _ = Len n
Len n .** Len n' = Len $ n `min` n'

prop_monoidal_left, prop_monoidal_right :: Eq a => List a -> Bool
prop_monoidal_left v = fmap snd (unit .** v) == v
prop_monoidal_right u = fmap fst (u .** unit) == u
prop_monoidal_assoc :: Eq a => List a -> List a -> List a -> Bool
prop_monoidal_assoc u v w = fmap asl (u .** (v .** w)) == (u .** v) .** w
	where asl (x, (y, z)) = ((x, y), z)

instance Random a => Random (List a) where
	random g = let (i, g') = randomR (0 :: Int, 2) g in
		case i of
			0 -> let
				(l, g'') = randomR (0 :: Int, 100) g'
				(g3, g4) = split g'' in
				(List . take l $ randoms g3, g4)
			1 -> Unit `first` random g'
			2 -> Len `first` random g'
	randomR (List xs, List ys) g = let
		(l, g') = randomR (length xs, length ys) g
		(g'', g3) = split g' in
		(List . take l $ randoms g'', g3)
	randomR (Unit x, Unit y) g = Unit `first` randomR (x, y) g
	randomR (Len n, Len n') g = Len `first` randomR (n, n') g
	randomR _ g = random g

instance Bounded a => Bounded (List a) where
	minBound = Len 0
	maxBound = List $ replicate 100 maxBound

instance (Bounded a, Random a) => Arbitrary (List a) where
	arbitrary = arbitraryBoundedRandom
