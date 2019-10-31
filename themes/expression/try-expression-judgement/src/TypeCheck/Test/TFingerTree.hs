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

newtype Fun a b = Fun (a -> b)

o :: Fun a b -> (b -> c) -> (a -> c)
o (Fun f) = (. f)

sampleTFingerTree :: TFingerTree Fun Char Int
sampleTFingerTree = toTree $ Fun ord ::: Fun (+ 3) ::: Fun (* 2) ::: EmptyList

appTFingerTree :: TFingerTree Fun a b -> a -> b
appTFingerTree ft = tfoldr o id ft
