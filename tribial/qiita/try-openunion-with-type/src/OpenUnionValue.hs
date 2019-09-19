{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures, DataKinds, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module OpenUnionValue (UnionValue, Member, inj, prj, decomp, extract) where

import Unsafe.Coerce (unsafeCoerce)

data UnionValue (as :: [*]) = forall a . UnionValue Word a

newtype P (a :: *) (as :: [*]) = P { unP :: Word } deriving Show

class Member (a :: *) (as :: [*]) where elemNo :: P a as
instance Member a (a ': as) where
	elemNo = P 0
instance {-# OVERLAPPABLE #-} Member a as => Member a (_a' ': as) where
	elemNo = P $ 1 + unP (elemNo :: P a as)

inj :: forall a as . Member a as => a -> UnionValue as
inj = UnionValue $ unP (elemNo :: P a as)

prj :: forall a as . Member a as => UnionValue as -> Maybe a
prj (UnionValue i x)
	| i == unP (elemNo :: P a as) = Just $ unsafeCoerce x
	| otherwise = Nothing

decomp :: UnionValue (a : as) -> Either (UnionValue as) a
decomp (UnionValue 0 x) = Right $ unsafeCoerce x
decomp (UnionValue i x) = Left $ UnionValue (i - 1) x

extract :: UnionValue '[a] -> a
extract (UnionValue _ x) = unsafeCoerce x
