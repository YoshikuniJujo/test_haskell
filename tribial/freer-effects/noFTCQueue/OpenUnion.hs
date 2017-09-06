{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures, DataKinds, TypeOperators #-}
{-# LANGUAGE
	MultiParamTypeClasses, FlexibleInstances,
	UndecidableInstances #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module OpenUnion where

import Unsafe.Coerce

data Union (r :: [* -> *]) a = forall t . Union !Word (t a)

{-
data Union (r :: [* -> *]) a where
	Union :: !Word -> t a -> Union r a
-}

unsafeInj :: Word -> t a -> Union r a
unsafeInj = Union

unsafePrj :: Word -> Union r a -> Maybe (t a)
unsafePrj n (Union n' x)
	| n == n' = Just (unsafeCoerce x)
	| otherwise = Nothing

newtype P (t :: * -> *) (r :: [* -> *]) = P { unP :: Word }

class FindElem (t :: * -> *) (r :: [* -> *]) where
	elemNo :: P t r

instance FindElem t (t ': r) where
	elemNo = P 0

instance {-# OVERLAPPABLE #-} FindElem t r => FindElem t (t' ': r) where
	elemNo = P $ 1 + unP (elemNo :: P t r)

class FindElem t r => Member (t :: * -> *) r where
	inj :: t a -> Union r a
	prj :: Union r a -> Maybe (t a)

instance FindElem t r => Member t r where
	inj = unsafeInj $ unP (elemNo :: P t r)
	prj = unsafePrj $ unP (elemNo :: P t r)

decomp :: Union (t ': r) a -> Either (Union r a) (t a)
decomp (Union 0 a) = Right $ unsafeCoerce a
decomp (Union n a) = Left $ Union (n - 1) a

extract :: Union '[t] a -> t a
extract (Union _ a) = unsafeCoerce a
