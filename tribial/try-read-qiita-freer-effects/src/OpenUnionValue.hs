{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module OpenUnionValue where

import Data.Kind
import Unsafe.Coerce

newtype P (a :: Type)  (as :: [Type]) = P { unP :: Word } deriving Show

class Member (a :: Type) (as :: [Type]) where elemNo :: P a as

instance Member a (a ': as) where elemNo = P 0

instance {-# OVERLAPPABLE #-} Member a as => Member a (_a' ': as) where
	elemNo = P $ 1 + unP (elemNo :: P a as)

data UnionValue (as :: [Type]) = forall a . UnionValue Word a

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
