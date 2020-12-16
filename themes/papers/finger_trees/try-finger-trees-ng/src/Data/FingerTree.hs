{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
	UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}

module Data.FingerTree (
	FingerTree, mkFingerTree, isEmpty, uncons, unsnoc,
	(<|), (<|.), (|>), (|>.), (><) ) where

import GHC.TypeNats (type (-), type (<=))

import Data.List.Range (
	RangeL(..), (.:..), (++.), loosenL, RangeR(..), loosenR,
	leftToRight, (++..) )
import Data.View (ViewL(..), ViewR(..))
import Internal.Tools (reducer, reducel)

---------------------------------------------------------------------------

-- * FINGER TREE
-- * FOLDABLE
-- * PUSH LEFT
-- * PUSH RIGHT
-- * VIEW LEFT
-- * VIEW RIGHT
-- * CONCATENATE

---------------------------------------------------------------------------
-- FINGER TREE
---------------------------------------------------------------------------

data FingerTree a
	= Empty | Single a | Deep (DigitL a) (FingerTree (Node a)) (DigitR a)
	deriving Show

type Node = RangeL 2 3; type DigitL = RangeL 1 4; type DigitR = RangeR 1 4

---------------------------------------------------------------------------
-- FOLDABLE
---------------------------------------------------------------------------

instance Foldable FingerTree where
	foldr :: forall a b . (a -> b -> b) -> b -> FingerTree a -> b
	foldr _ z Empty = z
	foldr (-<) z (Single x) = x -< z
	foldr (-<) z (Deep pr m sf) = pr -<. (m -<.. (sf -<. z))
		where
		(-<.) :: forall t . Foldable t => t a -> b -> b
		(-<.) = reducer (-<)
		(-<..) :: forall t t' .
			(Foldable t, Foldable t') => t (t' a) -> b -> b
		(-<..) = reducer (-<.)
	foldl :: forall a b . (b -> a -> b) -> b -> FingerTree a -> b
	foldl _ z Empty = z
	foldl (>-) z (Single x) = z >- x
	foldl (>-) z (Deep pr m sf) = z >-. pr >-.. m >-. sf
		where
		(>-.) :: forall t . Foldable t => b -> t a -> b
		(>-.) = reducel (>-)
		(>-..) :: forall t t' .
			(Foldable t, Foldable t') => b -> t (t' a) -> b
		(>-..) = reducel (>-.)

---------------------------------------------------------------------------
-- PUSH LEFT
---------------------------------------------------------------------------

infixr 5 <||, <|, <|.

(<||) :: a -> DigitL a -> Either (DigitL a) (DigitL a, Node a)
a <|| b :. NilL = Left $ a :. b :.. NilL
a <|| b :. c :.. NilL = Left $ a :. b :.. c :.. NilL
a <|| b :. c :.. d :.. NilL = Left $ a :. b :.. c :.. d :.. NilL
a <|| b :. c :.. d :.. e :.. NilL =
	Right (a :. b :.. NilL, c :. d :. e :.. NilL)
_ <|| _ = error "never occur"

(<|) :: a -> FingerTree a -> FingerTree a
a <| Empty = Single a
a <| Single b = Deep (a :. NilL) Empty (NilR :+ b)
a <| Deep pr m sf = case a <|| pr of
	Left pr' -> Deep pr' m sf
	Right (pr', n3) -> Deep pr' (n3 <| m) sf

(<|.) :: Foldable t => t a -> FingerTree a -> FingerTree a
(<|.) = reducer (<|)

mkFingerTree :: Foldable t => t a -> FingerTree a
mkFingerTree = (<|. Empty)

---------------------------------------------------------------------------
-- PUSH RIGHT
---------------------------------------------------------------------------

infixl 5 ||>, |>, |>.

(||>) :: DigitR a -> a -> Either (DigitR a) (Node a, DigitR a)
NilR :+ a ||> b = Left $ NilR :++ a :+ b
NilR :++ a :+ b ||> c = Left $ NilR :++ a :++ b :+ c
NilR :++ a :++ b :+ c ||> d = Left $ NilR :++ a :++ b :++ c :+ d
NilR :++ a :++ b :++ c :+ d ||> e =
	Right (a :. b :. c :.. NilL, NilR :++ d :+ e)
_ ||> _ = error "never occur"

(|>) :: FingerTree a -> a -> FingerTree a
Empty |> a = Single a
Single a |> b = Deep (a :. NilL) Empty (NilR :+ b)
Deep pr m sf |> a = case sf ||> a of
	Left sf' -> Deep pr m sf'
	Right (n3, sf') -> Deep pr (m |> n3) sf'

(|>.) :: Foldable t => FingerTree a -> t a -> FingerTree a
(|>.) = reducel (|>)

---------------------------------------------------------------------------
-- VIEW LEFT
---------------------------------------------------------------------------

viewL :: FingerTree a -> ViewL FingerTree a
viewL Empty = NL
viewL (Single x) = ConsL x Empty
viewL (Deep (a :. pr') m sf) = ConsL a $ deepL pr' m sf

deepL :: RangeL 0 3 a -> FingerTree (Node a) -> DigitR a -> FingerTree a
deepL NilL m sf = case viewL m of
	NL -> mkFingerTree sf
	ConsL n m' -> Deep (nodeToDigitL n) m' sf
deepL (a :.. pr) m sf = Deep (loosenL $ a :. pr) m sf
deepL _ _ _ = error "never occur"

nodeToDigitL :: Node a -> DigitL a
nodeToDigitL = loosenL

isEmpty :: FingerTree a -> Bool
isEmpty t = case viewL t of NL -> True; ConsL _ _ -> False

uncons :: FingerTree a -> Maybe (a, FingerTree a)
uncons t = case viewL t of NL -> Nothing; ConsL a t' -> Just (a, t')

---------------------------------------------------------------------------
-- VIEW RIGHT
---------------------------------------------------------------------------

viewR :: FingerTree a -> ViewR FingerTree a
viewR Empty = NR
viewR (Single x) = ConsR Empty x
viewR (Deep pr m (sf' :+ a)) = ConsR (deepR pr m sf') a

deepR :: DigitL a -> FingerTree (Node a) -> RangeR 0 3 a -> FingerTree a
deepR pr m NilR = case viewR m of
	NR -> mkFingerTree pr
	ConsR m' a -> Deep pr m' (nodeToDigitR a)
deepR pr m (sf :++ a) = Deep pr m (loosenR $ sf :+ a)
deepR _ _ _ = error "never occur"

nodeToDigitR :: Node a -> DigitR a
nodeToDigitR = loosenR . leftToRight

unsnoc :: FingerTree a -> Maybe (FingerTree a, a)
unsnoc t = case viewR t of NR -> Nothing; ConsR t' a -> Just (t', a)

---------------------------------------------------------------------------
-- CONCATENATE
---------------------------------------------------------------------------

class Nodes m m' where nodes :: RangeL 2 m a -> RangeL 1 m' (Node a)

instance Nodes 3 1 where nodes = (:. NilL)

instance {-# OVERLAPPABLE #-} (2 <= m', Nodes (m - 3) (m' - 1)) =>
	Nodes m m' where
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

app3 :: forall a . FingerTree a -> RangeL 0 4 a -> FingerTree a -> FingerTree a
app3 Empty m xs = m <|. xs
app3 xs m Empty = xs |>. m
app3 (Single x) m xs = x <| m <|. xs
app3 xs m (Single x) = xs |>. m |> x
app3 (Deep pr1 m1 sf1) m (Deep pr2 m2 sf2) = Deep pr1 (app3
	m1
	(loosenL (nodes (sf1 ++.. m ++. pr2) :: RangeL 1 4 (Node a)))
	m2) sf2

(><) :: FingerTree a -> FingerTree a -> FingerTree a
xs >< ys = app3 xs NilL ys
