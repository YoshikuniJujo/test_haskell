{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Some where

import Prelude hiding (head)

import GHC.Generics

newtype Identity a = Identity a deriving (Show, Generic)

class Head a where
	type HeadType a

	head :: a -> HeadType a

--	default type (Generic a, GHead (Rep a)) => HeadType a

	default head :: (Generic a, GHead (Rep a), HeadType a ~ GHeadType (Rep a)) => a -> HeadType a
	head = ghead . from

class GHead f where
	type GHeadType f

	ghead :: f a -> GHeadType f

instance GHead a => GHead (M1 i c a) where
	type GHeadType (M1 i c a) = GHeadType a
	
	ghead (M1 x) = ghead x

instance GHead (K1 i a) where
	type GHeadType (K1 i a) = a

	ghead (K1 x) = x

instance Head (Identity a) where
	type HeadType (Identity a) = a
