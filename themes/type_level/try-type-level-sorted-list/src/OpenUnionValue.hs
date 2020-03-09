{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification #-}
{-# LANGUAGE TypeOperators, KindSignatures, DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module OpenUnionValue (UnionValue, inj, prj, extract) where

import Data.Kind
import Unsafe.Coerce

import Sorted.Internal

newtype P (a :: Type) (as :: Sorted Type) = P { unP :: Word } deriving Show

class Member (a :: Type) (as :: Sorted Type) where elemNo :: P a as
instance Member a (a ':~ as) where
	elemNo = P 0
instance {-# OVERLAPPABLE #-} Member a as => Member a (_a' ':~ as) where
	elemNo = P $ 1 + unP (elemNo :: P a as)

data UnionValue (as :: Sorted Type) = forall a . UnionValue Word a

inj :: forall a as . Member a as => a -> UnionValue as
inj = UnionValue $ unP (elemNo :: P a as)

prj :: forall a as . Member a as => UnionValue as -> Maybe a
prj (UnionValue i x)
	| i == unP (elemNo :: P a as) = Just $ unsafeCoerce x
	| otherwise = Nothing

extract :: UnionValue (a ':~ 'Nil) -> a
extract (UnionValue _ x) = unsafeCoerce x
