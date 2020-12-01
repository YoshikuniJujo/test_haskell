{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.Range.OrderedSequence where

import Data.Foldable

import Data.List.Range.AnnotatedFingerTree
import Data.View

data Key a = NoKey | Key a deriving (Show, Eq, Ord)

instance Semigroup (Key a) where k <> NoKey = k; _ <> k = k
instance Monoid (Key a) where mempty = NoKey

instance Measured (Elem a) (Key a) where measure (Elem x) = Key x

newtype OrdSeq a = OrdSeq (FingerTree (Key a) (Elem a))

instance Foldable OrdSeq where foldMap f (OrdSeq ft) = foldMap (f . getElem) ft

instance Show a => Show (OrdSeq a) where
	show sq = "fromList " ++ show (toList sq)

partition :: (Ord a) => a -> OrdSeq a -> (OrdSeq a, OrdSeq a)
partition k (OrdSeq xs) = (OrdSeq l, OrdSeq r)
	where (l, r) = split (>= Key k) xs

insert :: (Ord a) => a -> OrdSeq a -> OrdSeq a
insert x (OrdSeq xs) = OrdSeq $ l >< (Elem x <| r)
	where (l, r) = split (>= Key x) xs

deleteAll :: (Ord a) => a -> OrdSeq a -> OrdSeq a
deleteAll x (OrdSeq xs) = OrdSeq $ l>< r'
	where (l, r) = split (>= Key x) xs; (_, r') = split (> Key x) r

merge :: (Ord a) => OrdSeq a -> OrdSeq a -> OrdSeq a
merge (OrdSeq xs) (OrdSeq ys) = OrdSeq $ merge' xs ys
	where merge' as bs = case viewL bs of
		NL -> as
		ConsL a bs' -> l >< (a <| merge' bs' r)
			where (l, r) = split (> measure a) as

fromList :: (Functor t, Foldable t, Ord a) => t a -> OrdSeq a
fromList = foldr insert $ OrdSeq Empty
