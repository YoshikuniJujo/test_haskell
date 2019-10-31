{-# LANGUAGE ScopedTypeVariables, RankNTypes, InstanceSigs #-}
{-# LANGUAGE GADTs, DataKinds, KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeCheck.Test.TFingerTree where

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
