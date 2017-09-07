{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures, DataKinds, TypeOperators #-}
{-# LANGUAGE
	MultiParamTypeClasses,
	FlexibleInstances, UndecidableInstances #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module OpenUnion (Union, Member, inj, decomp, extract) where

import Unsafe.Coerce (unsafeCoerce)

data Union (ts :: [* -> *]) a = forall t . Union !Word (t a)

unsafeInj :: Word -> t a -> Union ts a
unsafeInj = Union

unsafePrj :: Word -> Union ts a -> Maybe (t a)
unsafePrj i (Union j x)
	| i == j = Just $ unsafeCoerce x
	| otherwise = Nothing

newtype P (t :: * -> *) (ts :: [* -> *]) = P { unP :: Word }

class FindElem (t :: * -> *) (ts :: [* -> *]) where elemNo :: P t ts
instance FindElem t (t ': ts) where
	elemNo = P 0
instance {-# OVERLAPPABLE #-} FindElem t ts => FindElem t (_t' ': ts) where
	elemNo = P $ 1 + unP (elemNo :: P t ts)

class FindElem t ts => Member t ts where
	inj :: t a -> Union ts a
	_prj :: Union ts a -> Maybe (t a)

instance FindElem t ts => Member t ts where
	inj = unsafeInj $ unP (elemNo :: P t ts)
	_prj = unsafePrj $ unP (elemNo :: P t ts)

decomp :: Union (t ': ts) a -> Either (Union ts a) (t a)
decomp (Union 0 tx) = Right $ unsafeCoerce tx
decomp (Union i tx) = Left $ Union (i - 1) tx

extract :: Union '[t] a -> t a
extract (Union _ tx) = unsafeCoerce tx
