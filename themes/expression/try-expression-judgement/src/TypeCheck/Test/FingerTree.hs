{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE GADTs, DataKinds, TypeOperators, KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=TypeCheck.Nat #-}

module TypeCheck.Test.FingerTree where

import GHC.TypeLits

import TypeCheck.Test.Range

data FingerTree a
	= Empty
	| Single a
	| Deep (DigitL a) (FingerTree (Node a)) (DigitR a)
	deriving Show

type DigitL = RangeL 1 4
type DigitR = RangeR 1 4
type Node = RangeL 2 3

reducer :: Foldable t => (a -> b -> b) -> t a -> b -> b
reducer = flip . foldr

reducel :: Foldable t => (b -> a -> b) -> b -> t a -> b
reducel = foldl

instance Foldable FingerTree where
	foldr :: forall a b . (a -> b -> b) -> b -> FingerTree a -> b
	foldr _ z Empty = z
	foldr (-<) z (Single x) = x -< z
	foldr (-<) z (Deep pr m sf) = pr -<. (m -<.. (sf -<. z))
		where
		(-<.) :: forall t . Foldable t => t a -> b -> b
		(-<.) = reducer (-<)
		(-<..) :: forall t t' . (Foldable t, Foldable t') => t (t' a) -> b -> b
		(-<..) = reducer (-<.)
	foldl :: forall a b . (b -> a -> b) -> b -> FingerTree a -> b
	foldl _ z Empty = z
	foldl (>-) z (Single x) = z >- x
	foldl (>-) z (Deep pr m sf) = ((z >-. pr) >-.. m) >-. sf
		where
		(>-.) :: forall t . Foldable t => b -> t a -> b
		(>-.) = reducel (>-)
		(>-..) :: forall t t' . (Foldable t, Foldable t') => b -> t (t' a) -> b
		(>-..) = reducel (>-.)

infixr 5 <||, <|, <|.

(<||) :: a -> DigitL a -> Either (DigitL a) (DigitL a, Node a)
a <|| b :. NilL = Left $ a :. b :.. NilL
a <|| b :. c :.. NilL = Left $ a :. b :.. c :.. NilL
a <|| b :. c :.. d :.. NilL = Left $ a :. b :.. c :.. d :.. NilL
a <|| b :. c :.. d :.. e :.. NilL = Right (a :. b :.. NilL, c :. d :. e :.. NilL)
_ <|| _ = error "never occur"

(<|) :: a -> FingerTree a -> FingerTree a
a <| Empty = Single a
a <| Single b = Deep (a :. NilL) Empty (NilR :+ b)
a <| Deep pr m sf = case a <|| pr of
	Left pr' -> Deep pr' m sf
	Right (pr', n3) -> Deep pr' (n3 <| m) sf

(<|.) :: Foldable t => t a -> FingerTree a -> FingerTree a
(<|.) = reducer (<|)

infixl 5 ||>, |>, |>.

(||>) :: DigitR a -> a -> Either (DigitR a) (Node a, DigitR a)
NilR :+ a ||> b = Left $ NilR :++ a :+ b
NilR :++ a :+ b ||> c = Left $ NilR :++ a :++ b :+ c
NilR :++ a :++ b :+ c ||> d = Left $ NilR :++ a :++ b :++ c :+ d
NilR :++ a :++ b :++ c :+ d ||> e = Right (a :. b :. c :.. NilL, NilR :++ d :+ e)
_ ||> _ = error "never occur"

(|>) :: FingerTree a -> a -> FingerTree a
Empty |> a = Single a
Single a |> b = Deep (a :. NilL) Empty (NilR :+ b)
Deep pr m sf |> a = case sf ||> a of
	Left sf' -> Deep pr m sf'
	Right (n3, sf') -> Deep pr (m |> n3) sf'

(|>.) :: Foldable t => FingerTree a -> t a -> FingerTree a
(|>.) = reducel (|>)

toTree :: Foldable t => t a -> FingerTree a
toTree = (<|. Empty)

data ViewL s a = NL | ConsL a (s a) deriving Show

viewL :: FingerTree a -> ViewL FingerTree a
viewL Empty = NL
viewL (Single x) = ConsL x Empty
viewL (Deep (a :. pr') m sf) = ConsL a $ deepL pr' m sf

nodeToDigitL :: Node a -> DigitL a
nodeToDigitL = loosenL

deepL :: RangeL 0 3 a -> FingerTree (Node a) -> DigitR a -> FingerTree a
deepL NilL m sf = case viewL m of
	NL -> toTree sf
	ConsL a m' -> Deep (nodeToDigitL a) m' sf
deepL (a :.. pr) m sf = Deep (loosenL $ a :. pr) m sf
deepL _ _ _ = error "never occur"

data ViewR s a = NR | ConsR (s a) a deriving Show

viewR :: FingerTree a -> ViewR FingerTree a
viewR Empty = NR
viewR (Single x) = ConsR Empty x
viewR (Deep pr m (sf' :+ a)) = ConsR (deepR pr m sf') a

nodeToDigitR :: Node a -> DigitR a
nodeToDigitR = loosenR . leftToRight

deepR :: DigitL a -> FingerTree (Node a) -> RangeR 0 3 a -> FingerTree a
deepR pr m NilR = case viewR m of
	NR -> toTree pr
	ConsR m' a -> Deep pr m' (nodeToDigitR a)
deepR pr m (sf :++ a) = Deep pr m (loosenR $ sf :+ a)
deepR _ _ _ = error "never occur"

app3 :: forall a . FingerTree a -> RangeL 0 4 a -> FingerTree a -> FingerTree a
app3 Empty ts xs = ts <|. xs
app3 xs ts Empty = xs |>. ts
app3 (Single x) ts xs = x <| (ts <|. xs)
app3 xs ts (Single x) = (xs |>. ts) |> x
app3 (Deep pr1 m1 sf1) ts (Deep pr2 m2 sf2) =
	Deep pr1 (app3 m1 (loosenL (nodes (rightToLeft sf1 ++. ts ++. pr2) :: RangeL 1 4 (Node a))) m2) sf2

sampleHello :: FingerTree Char
sampleHello = toTree "Hello, world!"

class Nodes m m' where nodes :: RangeL 2 m a -> RangeL 1 m' (Node a)

instance Nodes 3 1 where nodes = (:. NilL)

instance {-# OVERLAPPABLE #-}
	(1 <= ((m' - 1) - 1) + 1, 1 <= (m' - 1) + 1, 1 <= (m' - 1), Nodes (m - 3) (m' - 1)) => Nodes m m' where
	nodes :: forall a . RangeL 2 m a -> RangeL 1 m' (Node a)
	nodes (a :. b :. NilL) = (a :. b :. NilL) :. NilL
	nodes (a :. b :. c :.. NilL) = (a :. b :. c :.. NilL) :. NilL
	nodes (a :. b :. c :.. d :.. NilL) =
		(a :. b :. NilL) :. (c :. d :. NilL) :.. NilL
	nodes (a :. b :. c :.. d :.. e :.. xs) =
		(a :. b :. c :.. NilL) .:..
			(nodes (d :. e :. xs :: RangeL 2 (m - 3) a)
				:: RangeL 1 (m' - 1) (Node a))
	nodes _ = error "never occur"

sample0, sample1, sample2, sample3, sample4 :: RangeL 2 12 Int
sample0	 = 1 :. 2 :. 3 :.. 4 :.. 5 :.. 6 :.. 7 :.. 8 :.. 9 :.. 10 :.. 11 :.. NilL
sample1	 = 1 :. 2 :. 3 :.. 4 :.. 5 :.. 6 :.. 7 :.. 8 :.. 9 :.. 10 :.. NilL
sample2 = 1 :. 2 :. 3 :.. 4 :.. 5 :.. 6 :.. 7 :.. 8 :.. 9 :.. NilL
sample3 = 1 :. 2 :. 3 :.. 4 :.. 5 :.. 6 :.. 7 :.. 8 :.. NilL
sample4 = 1 :. 2 :. 3 :.. 4 :.. 5 :.. 6 :.. 7 :.. NilL

(><) :: FingerTree a -> FingerTree a -> FingerTree a
xs >< ys = app3 xs NilL ys
