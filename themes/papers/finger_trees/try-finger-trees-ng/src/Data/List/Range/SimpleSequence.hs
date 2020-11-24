{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.Range.SimpleSequence where

import Prelude hiding (head)

import Data.List.Range

data Tree a = Zero a | Succ (Tree (Node a)) deriving Show
type Node a = RangeL 2 3 a

-- infixr 5 <||, <|
infixr 5 <||

(<||) :: a -> Node a -> RangeL 1 2 (Node a)
a <|| b :. c :. NilL = (a :. b :. c :.. NilL) :. NilL
a <|| b :. c :. d :.. NilL = (a :. b :. NilL) :. (c :. d :. NilL) :.. NilL
_ <|| _ = error "never occur"

head :: Tree a -> a
head (Zero a) = a
head (Succ t) = let (h :. _) = head t in h

uncons :: Tree a -> Either a (a, Tree a)
uncons (Zero a) = Left a
uncons (Succ t) = case uncons t of
	Left (a :. b :. NilL) -> Right (a, Zero b)
	Left (a :. b :. c :.. NilL) -> Right (a, Succ . Zero $ b :. c :. NilL)
	Right (a :. b :. NilL, t') -> Right (a, b `ins` Succ t')
	Right (a :. b :. c :.. NilL,  t') -> Right (a, Succ $ (b :. c :. NilL) `ins` t')
	_ -> error "never occur"

ins :: a -> Tree a -> Tree a
a `ins` t = case uncons t of
	Left b -> Succ . Zero $ a :. b :. NilL
	Right (b, Zero c) -> Succ . Zero $ a :. b :. c :.. NilL
	Right (b, Succ t') -> Succ $ (a :. b :. NilL) `ins` t'

sampleTree :: Tree Char
sampleTree = Succ . Succ . Zero $ ('t' :. 'h' :. NilL) :. ('i' :. 's' :. NilL) :. ('i' :. 's' :. NilL) :.. NilL
