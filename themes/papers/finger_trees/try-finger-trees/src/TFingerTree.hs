{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE GADTs, DataKinds, KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TFingerTree where

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

infix 5 :|

data TPair :: (* -> * -> *) -> (* -> * -> *) -> * -> * -> * where
	(:|) :: c x y -> d y z -> TPair c d x z

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
