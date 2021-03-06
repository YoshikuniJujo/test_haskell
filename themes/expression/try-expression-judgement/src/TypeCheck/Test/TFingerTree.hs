{-# LANGUAGE ScopedTypeVariables, RankNTypes, InstanceSigs #-}
{-# LANGUAGE GADTs, DataKinds, KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=TypeCheck.Nat #-}

module TypeCheck.Test.TFingerTree where

import GHC.TypeLits
import Data.Char

import TypeCheck.Test.TFoldable
import TypeCheck.Test.TRange

data TFingerTree :: (* -> * -> *) -> * -> * -> * where
	TEmpty :: TFingerTree c x x
	TSingle :: c x y -> TFingerTree c x y
	TDeep :: TDigitL c x y ->
		TFingerTree (TNode c) y z -> TDigitR c z w -> TFingerTree c x w

type TDigitL = TRangeL 1 4
type TDigitR = TRangeR 1 4
type TNode = TRangeL 2 3

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
		(-<..) :: forall t t' x y z .  (TFoldable t, TFoldable t') =>
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

(<||) :: c x y -> TDigitL c y z -> Either (TDigitL c x z) (TPair (TDigitL c) (TNode c) x z)
a <|| b :. NilL = Left $ a :. b :.. NilL
a <|| b :. c :.. NilL = Left $ a :. b :.. c :.. NilL
a <|| b :. c :.. d :.. NilL = Left $ a :. b :.. c :.. d :.. NilL
a <|| b :. c :.. d :.. e :.. NilL = Right $ a :. b :.. NilL :| c :. d :. e :.. NilL
_ <|| _ = error "never occur"

(<|) :: c x y -> TFingerTree c y z -> TFingerTree c x z
a <| TEmpty = TSingle a
a <| TSingle b = TDeep (a :. NilL) TEmpty (NilR :+ b)
a <| TDeep pr m sf = case a <|| pr of
	Left pr' -> TDeep pr' m sf
	Right (pr' :| n3) -> TDeep pr' (n3 <| m) sf

(<|.) :: TFoldable t => t c x y -> TFingerTree c y z -> TFingerTree c x z
(<|.) = treducer (<|)

toTree :: TFoldable t => t c x y -> TFingerTree c x y
toTree = (<|. TEmpty)

sampleTFingerTree :: TFingerTree (->) Char Int
sampleTFingerTree = toTree $ ord ::: (* 2) ::: (+ 3) ::: (+ 3) ::: (+ 3) ::: (+ 3) ::: (+ 3) ::: EmptyList

sampleTFingerTree2 :: TFingerTree (->) Int Int
sampleTFingerTree2 = toTree $ (* 2) ::: (* 5) ::: (* 2) ::: (* 5) ::: (* 2) ::: (* 5) ::: EmptyList

appTFingerTree :: TFingerTree (->) a b -> a -> b
appTFingerTree ft = tfoldr (flip (.)) id ft

infixl 5 ||>, |>, |>.

(||>) :: TDigitR c x y -> c y z -> Either (TDigitR c x z) (TPair (TNode	c) (TDigitR c) x z)
NilR :+ a ||> b = Left $ NilR :++ a :+ b
NilR :++ a :+ b ||> c = Left $ NilR :++ a :++ b :+ c
NilR :++ a :++ b :+ c ||> d = Left $ NilR :++ a :++ b :++ c :+ d
NilR :++ a :++ b :++ c :+ d ||> e = Right $ a :. b :. c :.. NilL :| NilR :++ d :+ e
_ ||> _ = error "never occur"

(|>) :: TFingerTree c x y -> c y z -> TFingerTree c x z
TEmpty |> a = TSingle a
TSingle a |> b = TDeep (a :. NilL) TEmpty (NilR :+ b)
TDeep pr m sf |> a = case sf ||> a of
	Left sf' -> TDeep pr m sf'
	Right (n3 :| sf') -> TDeep pr (m |> n3) sf'

(|>.) :: TFoldable t => TFingerTree c x y -> t c y z -> TFingerTree c x z
(|>.) = treducel (|>)

data TViewL ::
	((* -> * -> *) -> * -> * -> *) -> (* -> * -> *) -> * -> * -> * where
	NL :: TViewL s c x x
	ConsL :: c x y -> s c y z -> TViewL s c x z

viewL :: TFingerTree c x y -> TViewL TFingerTree c x y
viewL TEmpty = NL
viewL (TSingle x) = ConsL x TEmpty
viewL (TDeep (a :. pr') m sf) = ConsL a $ deepL pr' m sf

nodeToDigitL :: TNode c x y -> TDigitL c x y
nodeToDigitL = loosenL

deepL :: TRangeL 0 3 c x y ->
	TFingerTree (TNode c) y z -> TDigitR c z w -> TFingerTree c x w
deepL NilL m sf = case viewL m of
	NL -> toTree sf
	ConsL a m' -> TDeep (nodeToDigitL a) m' sf
deepL (x :.. xs) m sf = TDeep (loosenL $ x :. xs) m sf
deepL _ _ _ = error "never occur"

testViewL :: Int
testViewL = case viewL sampleTFingerTree of
	ConsL f fs -> tfoldr (flip (.)) id fs . f $ 'c'
--	NL -> error "never occur"

data TViewR ::
	((* -> * -> *) -> * -> * -> *) -> (* -> * -> *) -> * -> * -> * where
	NR :: TViewR s c x x
	ConsR :: s c x y -> c y z -> TViewR s c x z

viewR :: TFingerTree c x y -> TViewR TFingerTree c x y
viewR TEmpty = NR
viewR (TSingle x) = ConsR TEmpty x
viewR (TDeep pr m (sf' :+ a)) = ConsR (deepR pr m sf') a

nodeToDigitR :: TNode c x y -> TDigitR c x y
nodeToDigitR = tloosenR . leftToRight

deepR :: TDigitL c x y ->
	TFingerTree (TNode c) y z -> TRangeR 0 3 c z w -> TFingerTree c x w
deepR pr m NilR = case viewR m of
	NR -> toTree pr
	ConsR m' a -> TDeep pr m' (nodeToDigitR a)
deepR pr m (sf :++ a) = TDeep pr m (tloosenR $ sf :+ a)
deepR _ _ _ = error "never occur"

testViewR :: Int
testViewR = case viewR sampleTFingerTree of
	ConsR fs f -> f . tfoldr (flip (.)) id fs $ 'c'

app3 :: forall c x y z w . TFingerTree c x y -> TRangeL 0 4 c y z -> TFingerTree c z w -> TFingerTree c x w
app3 TEmpty ts xs = ts <|. xs
app3 xs ts TEmpty = xs |>. ts
app3 (TSingle x) ts xs = x <| (ts <|. xs)
app3 xs ts (TSingle x) = (xs |>. ts) |> x
app3 (TDeep pr1 m1 sf1) ts (TDeep pr2 m2 sf2) =
	tDeep pr1 m1 (nodes (rightToLeft sf1 ++. ts ++. pr2)) m2 sf2
--	TDeep pr1 (app3 m1 (loosenL (nodes (rightToLeft sf1 ++. ts ++. pr2))) m2) sf2
	where
	tDeep :: forall d e f g . TDigitL c x d -> TFingerTree (TNode c) d e ->
		TRangeL 1 4 (TNode c) e f -> TFingerTree (TNode c) f g -> TDigitR c g w ->
		TFingerTree c x w
	tDeep p1 m1' nd m2' s2 = TDeep p1 (app3 m1' (loosenL nd) m2') s2

class Nodes m m' where nodes :: TRangeL 2 m c x y -> TRangeL 1 m' (TNode c) x y

instance Nodes 3 1 where nodes = (:. NilL)

instance {-# OVERLAPPABLE #-}
	(1 <= m' - 1, 1 <= m', Nodes (m - 3) (m' - 1)) => Nodes m m' where
	nodes :: forall c x y . TRangeL 2 m c x y -> TRangeL 1 m' (TNode c) x y
	nodes (a :. b :. NilL) = (a :. b :. NilL) :. NilL
	nodes (a :. b :. c :.. NilL) = (a :. b :. c :.. NilL) :. NilL
	nodes (a :. b :. c :.. d :.. NilL) =
		(a :. b :. NilL) :. (c :. d :. NilL) :.. NilL
	nodes (a :. b :. c :.. d :.. e :.. xs) = f (a :. b :. c :.. NilL) (d :. e :. xs)
--		(a :. b :. c :.. NilL) ..:.. (nodes (d :. e :. xs))
		where
		f :: forall w . TNode c x w -> TRangeL 2 (m - 3) c w y -> TRangeL 1 m' (TNode c) x y
		f x y = x ..:.. nodes y
			where
			(..:..) :: TNode c x w -> TRangeL 1 (m' - 1) (TNode c) w y -> TRangeL 1 m' (TNode c) x y
			(..:..) = (.:..)
	nodes _ = error "never occur"

(><) :: TFingerTree c x y -> TFingerTree c y z -> TFingerTree c x z
xs >< ys = app3 xs NilL ys
