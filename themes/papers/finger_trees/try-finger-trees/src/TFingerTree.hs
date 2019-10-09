{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE GADTs, DataKinds, TypeOperators, KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
-- {-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=GHC.TypeLits.Normalise #-}

module TFingerTree where

import GHC.TypeLits
import Data.Char

import TFoldable
import TRange

data TFingerTree :: (* -> * -> *) -> * -> * -> * where
	TEmpty :: TFingerTree c x x
	TSingle :: c x y -> TFingerTree c x y
	TDeep :: TDigit c x y ->
		TFingerTree (TNode c) y z -> TDigit c z w -> TFingerTree c x w

type TDigit = TRange 1 4
type TNode = TRange 2 3

instance TFoldable TFingerTree where
	tfoldr :: forall c d x' y' z' .
		(forall x y z . c x y -> d y z -> d x z) ->
			d y' z' -> TFingerTree c x' y' -> d x' z'
	tfoldr _ z TEmpty = z
	tfoldr (-<) z (TSingle x) = x -< z
	tfoldr (-<) z (TDeep pr m sf) = pr -<. (m -<.. (sf -<. z))
		where
		(-<.) :: forall t x y z .
			TFoldable t => t c x y -> d y z -> d x z
		(-<.) = treducer (-<)
		(-<..) :: forall t t' x y z . (TFoldable t, TFoldable t') =>
			t (t' c) x y -> d y z -> d x z
		(-<..) = treducer (-<.)
	tfoldl :: forall c d x' y' z' .
		(forall x y z . d x y -> c y z -> d x z) ->
			d x' y' -> TFingerTree c y' z' -> d x' z'
	tfoldl _ z TEmpty = z
	tfoldl (>-) z (TSingle x) = z >- x
	tfoldl (>-) z (TDeep pr m sf) = ((z >-. pr) >-.. m) >-. sf
		where
		(>-.) :: forall t x y z .
			TFoldable t => d x y -> t c y z -> d x z
		(>-.) = treducel (>-)
		(>-..) :: forall t t' x y z . (TFoldable t, TFoldable t') =>
			d x y -> t (t' c) y z -> d x z
		(>-..) = treducel (>-.)

infixr 5 <||, <|, <|.

(<||) :: c x y -> TDigit c y z -> Either (TDigit c x z) (TPair (TDigit c) (TNode c) x z)
a <|| b :. Nil = Left $ a :. b :.. Nil
a <|| b :. c :.. Nil = Left $ a :. b :.. c :.. Nil
a <|| b :. c :.. d :.. Nil = Left $ a :. b :.. c :.. d :.. Nil
a <|| b :. c :.. d :.. e :.. Nil = Right $ a :. b :.. Nil :| c :. d :. e :.. Nil
_ <|| _ = error "never occur"

(<|) :: c x y -> TFingerTree c y z -> TFingerTree c x z
a <| TEmpty = TSingle a
a <| TSingle b = TDeep (a :. Nil) TEmpty (b :. Nil)
a <| TDeep pr m sf = case a <|| pr of
	Left pr' -> TDeep pr' m sf
	Right (pr' :| n3) -> TDeep pr' (n3 <| m) sf

(<|.) :: TFoldable t => t c x y -> TFingerTree c y z -> TFingerTree c x z
(<|.) = treducer (<|)

toTree :: TFoldable t => t c x y -> TFingerTree c x y
toTree = (<|. TEmpty)

sampleList :: TList (->) Char Int
sampleList = ord ::: (* 2) ::: (+ 3) ::: (+ 3) ::: (+ 3) ::: (+ 3) ::: (+ 3) ::: EmptyList

sampleFTree :: TFingerTree (->) Char Int
sampleFTree = toTree sampleList

-- infixr 5 ||>, |>, |>.
infixr 5 ||>

(||>) :: TDigit c x y -> c y z -> Either (TDigit c x z) (TPair (TNode c) (TDigit c) x z)
a :. Nil ||> b = Left $ a :. b :.. Nil
a :. b :.. Nil ||> c = Left $ a :. b :.. c :.. Nil
a :. b :.. c :.. Nil ||> d = Left $ a :. b :.. c :.. d :.. Nil
a :. b :.. c :.. d :.. Nil ||> e = Right $ a :. b :. c :.. Nil :| d :. e :.. Nil
_ ||> _ = error "never occur"

(|>) :: TFingerTree c x y -> c y z -> TFingerTree c x z
TEmpty |> a = TSingle a
TSingle a |> b = TDeep (a :. Nil) TEmpty (b :. Nil)
TDeep pr m sf |> a = case sf ||> a of
	Left sf' -> TDeep pr m sf'
	Right (n3 :| sf') -> TDeep pr (m |> n3) sf'

(|>.) :: TFoldable t => TFingerTree c x y -> t c y z -> TFingerTree c x z
(|>.) = treducel (|>)

data TViewL ::
	((* -> * -> *) -> * -> * -> *) -> (* -> * -> *) -> * -> * -> * where
	NilL :: TViewL s c x x
	ConsL :: c x y -> s c y z -> TViewL s c x z

viewL :: TFingerTree c x y -> TViewL TFingerTree c x y
viewL TEmpty = NilL
viewL (TSingle x) = ConsL x TEmpty
viewL (TDeep (a :. pr') m sf) = ConsL a $ deepL pr' m sf

nodeToDigit :: TNode c x y -> TDigit c x y
nodeToDigit = tloosen

deepL :: TRange 0 3 c x y ->
	TFingerTree (TNode c) y z -> TDigit c z w -> TFingerTree c x w
deepL Nil m sf = case viewL m of
	NilL -> toTree sf
	ConsL a m' -> TDeep (nodeToDigit a) m' sf
deepL (x :.. xs) m sf = TDeep (tloosen $ x :. xs) m sf
deepL _ _ _ = error "never occur"

some :: TRange 0 1 c x y -> TList c y z -> TList c x z
some Nil l = l
some (x :.. Nil) l = x ::: l
some _ _ = error "never occur"

testViewL :: Int
testViewL = case viewL sampleFTree of
	ConsL f fs -> tfoldr (flip (.)) id fs . f $ 'c'
--	NilL -> error "never occur"

data TViewR ::
	((* -> * -> *) -> * -> * -> *) -> (* -> * -> *) -> * -> * -> * where
	NilR :: TViewR s c x x
	ConsR :: s c x y -> c y z -> TViewR s c x z

viewR :: TFingerTree c x y -> TViewR TFingerTree c x y
viewR TEmpty = NilR
viewR (TSingle x) = ConsR TEmpty x
viewR (TDeep pr m sf) = case unsnoc sf of
	sf' :| a -> ConsR (deepR pr m sf') a

deepR :: TDigit c x y ->
	TFingerTree (TNode c) y z -> TRange 0 3 c z w -> TFingerTree c x w
deepR pr m Nil = case viewR m of
	NilR -> toTree pr
	ConsR m' a -> TDeep pr m' (nodeToDigit a)
deepR pr m (a :.. sf) = TDeep pr m (tloosen $ a :. sf)
deepR _ _ _ = error "never occur"

testViewR :: Int
testViewR = case viewR sampleFTree of
	ConsR fs f -> f . tfoldr (flip (.)) id fs $ 'c'

app3 :: forall c x y z w . TFingerTree c x y ->
	TRange 0 4 c y z -> TFingerTree c z w -> TFingerTree c x w
app3 TEmpty ts ys = ts <|. ys
app3 xs ts TEmpty = xs |>. ts
app3 (TSingle x) ts ys = x <| (ts <|. ys)
app3 xs ts (TSingle y) = (xs |>. ts) |> y
app3 (TDeep pr1 m1 sf1) ts (TDeep pr2 m2 sf2) =
	TDeep pr1 (app3 m1 (tloosen' (nodes (sf1 ++. ts ++.. pr2))) m2) sf2
	where
	tloosen' :: forall x' y' . TRange 1 4 (TNode c) x' y' -> TRange 0 4 (TNode c) x' y'
	tloosen' = tloosen

nodes0 :: TRange 2 6 c x y -> TRange 1 2 (TNode c) x y
nodes0 (a :. b :. Nil) = (a :. b :. Nil) :. Nil
nodes0 (a :. b :. c :.. Nil) = (a :. b :. c :.. Nil) :. Nil
nodes0 (a :. b :. c :.. d :.. Nil) = (a :. b :. Nil) :. (c :. d :. Nil) :.. Nil
nodes0 (a :. b :. c :.. d :.. e :.. Nil) = (a :. b :. c :.. Nil) :. (d :. e :. Nil) :.. Nil
nodes0 (a :. b :. c :.. d :.. e :.. f :.. Nil) = (a :. b :. c :.. Nil) :. (d :. e :. f :.. Nil) :.. Nil
nodes0 _ = error "never occur"

class Nodes m m' where
	nodes :: TRange 2 m c x y -> TRange 1 m' (TNode c) x y

instance Nodes 6 2 where
	nodes = tloosen . nodes0

instance {-# OVERLAPPABLE #-} Nodes (m - 3) (m' - 1) => Nodes m m' where
	nodes :: forall c x y . TRange 2 m c x y -> TRange 1 m' (TNode c) x y
	nodes (a :. b :. Nil) = (a :. b :. Nil) :. Nil
	nodes (a :. b :. c :.. Nil) = (a :. b :. c :.. Nil) :. Nil
	nodes (a :. b :. c :.. d :.. Nil) = (a :. b :. Nil) :. (c :. d :. Nil) :.. Nil
	nodes (a :. b :. c :.. d :.. e :.. xs) = (a :. b :. c :.. Nil) .:.. nodes' (d :. e :. xs)
		where
		nodes' :: forall x' . TRange 2 (m - 3) c x' y -> TRange 1 (m' - 1) (TNode c) x' y
		nodes' = nodes
	nodes _ = error "never occur"

(><) :: TFingerTree c x y -> TFingerTree c y z -> TFingerTree c x z
xs >< ys = app3 xs Nil ys
