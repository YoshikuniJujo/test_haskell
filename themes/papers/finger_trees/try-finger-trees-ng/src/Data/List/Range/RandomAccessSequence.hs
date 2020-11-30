{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFoldable #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.Range.RandomAccessSequence where

import Prelude hiding (splitAt)

import Data.Foldable

import Data.List.Range.AnnotatedFingerTree
import Data.View

newtype Size = Size { getSize :: Int } deriving (Show, Eq, Ord)

instance Semigroup Size where Size m <> Size n = Size $ m + n
instance Monoid Size where mempty = Size 0

instance Measured (Elem a) Size where measure _ = Size 1

sampleAnn1, sampleAnn2 :: FingerTree Size (Elem Int)
sampleAnn1 = toTree $ Elem <$> [1 .. 100]
sampleAnn2 = toTree $ Elem <$> [123 .. 200]

newtype Seq a = Seq (FingerTree Size (Elem a))

instance Foldable Seq where
	foldMap f (Seq ft) = foldMap (f . getElem) ft
	length (Seq ft) = getSize $ measure ft

instance Show a => Show (Seq a) where
	show sq = "fromList " ++ show (toList sq)

fromList :: (Functor t, Foldable t) => t a -> Seq a
fromList = Seq . toTree . (Elem <$>)

splitAt :: Int -> Seq a -> (Seq a, Seq a)
splitAt i (Seq xs) = (Seq l, Seq r)
	where (l, r) = split (Size i <) xs

(!) :: Seq a -> Int -> a
Seq xs ! i = getElem x where Split _ x _ = splitTree (Size i <) (Size 0) xs
