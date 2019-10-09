{-# LANGUAGE ScopedTypeVariables, GADTs, DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=GHC.TypeLits.Normalise #-}

module FingerTreeRange where

import GHC.TypeLits

import Range

data FingerTree a
	= Empty
	| Single a
	| Deep (Digit a) (FingerTree (Node a)) (Digit a)
	deriving Show

type Digit = Range 1 4
type Node = Range 2 3

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

(<||) :: a -> Digit a -> Either (Digit a) (Digit a, Node a)
a <|| b :. Nil = Left $ a :. b :.. Nil
a <|| b :. c :.. Nil = Left $ a :. b :.. c :.. Nil
a <|| b :. c :.. d :.. Nil = Left $ a :. b :.. c :.. d :.. Nil
a <|| b :. c :.. d :.. e :.. Nil = Right (a :. b :.. Nil, c :. d :. e :.. Nil)
_ <|| _ = error "never occur"

(<|) :: a -> FingerTree a -> FingerTree a
a <| Empty = Single a
a <| Single b = Deep (a :. Nil) Empty (b :. Nil)
a <| Deep pr m sf = case a <|| pr of
	Left pr' -> Deep pr' m sf
	Right (pr', n3) -> Deep pr' (n3 <| m) sf

(<|.) :: Foldable t => t a -> FingerTree a -> FingerTree a
(<|.) = reducer (<|)

infixl 5 ||>, |>, |>.

(||>) :: Digit a -> a -> Either (Digit a) (Node a, Digit a)
a :. Nil ||> b = Left $ a :. b :.. Nil
a :. b :.. Nil ||> c = Left $ a :. b :.. c :.. Nil
a :. b :.. c :.. Nil ||> d = Left $ a :. b :.. c :.. d :.. Nil
a :. b :.. c :.. d :.. Nil ||> e = Right (a :. b :. c :.. Nil, d :. e :.. Nil)
_ ||> _ = error "never occur"

(|>) :: FingerTree a -> a -> FingerTree a
Empty |> a = Single a
Single a |> b = Deep (a :. Nil) Empty (b :. Nil)
Deep pr m sf |> a = case sf ||> a of
	Left sf' -> Deep pr m sf'
	Right (n3, sf') -> Deep pr (m |> n3) sf'

(|>.) :: Foldable t => FingerTree a -> t a -> FingerTree a
(|>.) = reducel (|>)

toTree :: Foldable t => t a -> FingerTree a
toTree = (<|. Empty)

data ViewL s a = NilL | ConsL a (s a) deriving Show

viewL :: FingerTree a -> ViewL FingerTree a
viewL Empty = NilL
viewL (Single x) = ConsL x Empty
viewL (Deep (a :. pr') m sf) = ConsL a $ deepL pr' m sf

nodeToDigit :: Node a -> Digit a
nodeToDigit = loosen

deepL :: Range 0 3 a -> FingerTree (Node a) -> Digit a -> FingerTree a
deepL Nil m sf = case viewL m of
	NilL -> toTree sf
	ConsL a m' -> Deep (nodeToDigit a) m' sf
deepL (a :.. pr) m sf = Deep (loosen $ a :. pr) m sf
deepL _ _ _ = error "never occur"

sampleList :: [Int]
sampleList = [1 .. 7]

sampleFTree :: FingerTree Int
sampleFTree = toTree sampleList

isEmpty :: FingerTree a -> Bool
isEmpty ft = case viewL ft of NilL -> True; ConsL _ _ -> False

data ViewR s a = NilR | ConsR (s a) a deriving Show

viewR :: FingerTree a -> ViewR FingerTree a
viewR Empty = NilR
viewR (Single x) = ConsR Empty x
viewR (Deep pr m sf) = case unsnoc sf of
	(sf', a) -> ConsR (deepR pr m sf') a

deepR :: Digit a -> FingerTree (Node a) -> Range 0 3 a -> FingerTree a
deepR pr m Nil = case viewR m of
	NilR -> toTree pr
	ConsR m' a -> Deep pr m' (nodeToDigit a)
deepR pr m (a :.. sf) = Deep pr m (loosen $ a :. sf)
deepR _ _ _ = error "never occur"

app3 :: forall a . FingerTree a -> Range 0 4 a -> FingerTree a -> FingerTree a
app3 Empty ts xs = ts <|. xs
app3 xs ts Empty = xs |>. ts
app3 (Single x) ts xs = x <| (ts <|. xs)
app3 xs ts (Single x) = (xs |>. ts) |> x
app3 (Deep pr1 m1 sf1) ts (Deep pr2 m2 sf2) =
	Deep pr1 (app3 m1 (loosen (nodes' (sf1 ++. ts ++.. pr2) :: Range 1 4 (Node a))) m2) sf2

nodesTest :: Range 2 6 a -> Range 1 2 (Node a)
nodesTest (a :. b :. Nil) = (a :. b :. Nil) :. Nil
nodesTest (a :. b :. c :.. Nil) = (a :. b :. c :.. Nil) :. Nil
nodesTest (a :. b :. c :.. d :.. Nil) = (a :. b :. Nil) :. (c :. d :. Nil) :.. Nil
nodesTest (a :. b :. c :.. d :.. e :.. Nil) = (a :. b :. c :.. Nil) :. (d :. e :. Nil) :.. Nil
nodesTest (a :. b :. c :.. d :.. e :.. f :.. Nil) = (a :. b :. c :.. Nil) :. (d :. e :. f :.. Nil) :.. Nil
nodesTest _ = error "never occur"

class Nodes m m' where
	nodes' :: Range 2 m a -> Range 1 m' (Node a)

instance Nodes 6 2 where
	nodes' = loosen . nodesTest

instance {-# OVERLAPPABLE #-} Nodes (m - 3) (m' - 1) => Nodes m m' where
	nodes' :: forall a . Range 2 m a -> Range 1 m' (Node a)
	nodes' (a :. b :. Nil) = (a :. b :. Nil) :. Nil
	nodes' (a :. b :. c :.. Nil) = (a :. b :. c :.. Nil) :. Nil
	nodes' (a :. b :. c :.. d :.. Nil) = (a :. b :. Nil) :. (c :. d :. Nil) :.. Nil
	nodes' (a :. b :. c :.. d :.. e :.. xs :: Range 2 m a) = (a :. b :. c :.. Nil) .:.. (nodes' (d :. e :. xs :: Range 2 (m - 3) a) :: Range 1 (m' - 1) (Node a))
	nodes' _ = error "never occur"

(><) :: FingerTree a -> FingerTree a -> FingerTree a
xs >< ys = app3 xs Nil ys
