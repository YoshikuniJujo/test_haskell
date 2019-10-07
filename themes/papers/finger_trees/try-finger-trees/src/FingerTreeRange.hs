{-# LANGUAGE ScopedTypeVariables, GADTs, DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FingerTreeRange where

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

notNil :: Range 0 m a -> Maybe (Range 1 m a)
notNil Nil = Nothing
notNil (x :.. xs) = Just $ x :. xs
notNil _ = error "never occur"

deepL :: Range 0 3 a -> FingerTree (Node a) -> Digit a -> FingerTree a
deepL pr m sf = case notNil pr of
	Nothing -> case viewL m of
		NilL -> toTree sf
		ConsL a m' -> Deep (nodeToDigit a) m' sf
	Just pr' -> Deep (loosen pr') m sf
