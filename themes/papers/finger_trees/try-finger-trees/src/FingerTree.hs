{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, InstanceSigs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FingerTree where

import Data.Foldable

import FingerTree.TH

data FingerTree a
	= Empty
	| Single a
	| Deep (Digit a) (FingerTree (Node a)) (Digit a)
	deriving Show

data Digit a = One a | Two a a | Three a a a | Four a a a a deriving Show
data Node a = Node2 a a | Node3 a a a deriving Show

instanceFoldable ''Node [('Node2, 2), ('Node3, 3)]
instanceFoldable ''Digit [('One, 1), ('Two, 2), ('Three, 3), ('Four, 4)]

{-
instance Foldable Node where
	foldr (-<) z (Node2 a b) = a -< (b -< z)
	foldr (-<) z (Node3 a b c) = a -< (b -< (c -< z))
	foldl (>-) z (Node2 a b) = (z >- a) >- b
	foldl (>-) z (Node3 a b c) = ((z >- a) >- b) >- c
	-}

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
		(-<..) = reducer $ reducer (-<)
	foldl :: forall a b . (b -> a -> b) -> b -> FingerTree a -> b
	foldl _ z Empty = z
	foldl (>-) z (Single x) = z >- x
	foldl (>-) z (Deep pr m sf) = ((z >-. pr) >-.. m) >-. sf
		where
		(>-.) :: forall t . Foldable t => b -> t a -> b
		(>-.) = foldl (>-)
		(>-..) :: forall t t'. (Foldable t, Foldable t') => b -> t (t' a) -> b
		(>-..) = foldl $ foldl (>-)

infixr 5 <|

(<||) :: a -> Digit a -> Either (Digit a) (Digit a, Node a)
a <|| One b = Left $ Two a b
a <|| Two b c = Left $ Three a b c
a <|| Three b c d = Left $ Four a b c d
a <|| Four b c d e = Right (Two a b, Node3 c d e)

(<|) :: a -> FingerTree a -> FingerTree a
a <| Empty = Single a
a <| Single b = Deep (One a) Empty (One b)
a <| Deep pr m sf = case a <|| pr of
	Left pr' -> Deep pr' m sf
	Right (pr', n3) -> Deep pr' (n3 <| m) sf

(||>) :: Digit a -> a -> Either (Digit a) (Node a, Digit a)
One a ||> b = Left $ Two a b
Two a b ||> c = Left $ Three a b c
Three a b c ||> d = Left $ Four a b c d
Four a b c d ||> e = Right (Node3 a b c, Two d e)

(|>) :: FingerTree a -> a -> FingerTree a
Empty |> a = Single a
Single a |> b = Deep (One a) Empty (One b)
Deep pr m sf |> a = case sf ||> a of
	Left sf' -> Deep pr m sf'
	Right (n3, sf') -> Deep pr (m |> n3) sf'

(<|.) :: Foldable t => t a -> FingerTree a -> FingerTree a
(<|.) = reducer (<|)

(|>.) :: Foldable t => FingerTree a -> t a -> FingerTree a
(|>.) = reducel (|>)

toTree :: Foldable t => t a -> FingerTree a
toTree = (<|. Empty)

data ViewL s a = NilL | ConsL a (s a) deriving Show

viewL :: FingerTree a -> ViewL FingerTree a
viewL Empty = NilL
viewL (Single x) = ConsL x Empty
viewL (Deep pr m sf) = ConsL (head $ toList pr) (deepL (tail $ toList pr) m sf)

nodeToDigit :: Node a -> Digit a
nodeToDigit (Node2 a b) = Two a b
nodeToDigit (Node3 a b c) = Three a b c

fromList :: [a] -> Digit a
fromList [a] = One a
fromList [a, b] = Two a b
fromList [a, b, c] = Three a b c
fromList [a, b, c, d] = Four a b c d
fromList _ = error "bad"

deepL :: [a] -> FingerTree (Node a) -> Digit a -> FingerTree a
deepL [] m sf = case viewL m of
	NilL -> toTree sf
	ConsL a m' -> Deep (nodeToDigit a) m' sf
deepL pr m sf = Deep (fromList pr) m sf
