{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures, DataKinds, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Private.OpenUnion (Union, Member, inj, prj, decomp, extract) where

import Unsafe.Coerce (unsafeCoerce)

data Union (ts :: [* -> *]) a = forall t . Union !Word (t a)

inj :: forall t ts a . Member t ts => t a -> Union ts a
inj = unsafeInj $ unP (elemNo :: P t ts)

prj :: forall t ts a . Member t ts => Union ts a -> Maybe (t a)
prj = unsafePrj $ unP (elemNo :: P t ts)

newtype P (t :: * -> *) (ts :: [* -> *]) = P { unP :: Word }

class Member (t :: * -> *) (ts :: [* -> *]) where elemNo :: P t ts
instance Member t (t ': ts) where
	elemNo = P 0
instance {-# OVERLAPPABLE #-} Member t ts => Member t (_t' ': ts) where
	elemNo = P $ 1 + unP (elemNo :: P t ts)

unsafeInj :: Word -> t a -> Union ts a
unsafeInj = Union

unsafePrj :: Word -> Union ts a -> Maybe (t a)
unsafePrj i (Union j x)
	| i == j = Just $ unsafeCoerce x
	| otherwise = Nothing

decomp :: Union (t ': ts) a -> Either (Union ts a) (t a)
decomp (Union 0 tx) = Right $ unsafeCoerce tx
decomp (Union i tx) = Left $ Union (i - 1) tx

extract :: Union '[t] a -> t a
extract (Union _ tx) = unsafeCoerce tx
