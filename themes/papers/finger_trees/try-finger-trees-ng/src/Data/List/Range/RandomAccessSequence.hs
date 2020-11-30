{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFoldable #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.Range.RandomAccessSequence where

import Data.List.Range.AnnotatedFingerTree

newtype Size = Size { getSize :: Int } deriving (Show, Eq, Ord)

instance Semigroup Size where Size m <> Size n = Size $ m + n
instance Monoid Size where mempty = Size 0

newtype Elem a = Elem { getElem :: a } deriving Show

instance Measured (Elem a) Size where measure _ = Size 1

sampleAnn1, sampleAnn2 :: FingerTree Size (Elem Int)
sampleAnn1 = toTree $ Elem <$> [1 .. 100]
sampleAnn2 = toTree $ Elem <$> [123 .. 200]

newtype Seq a = Seq (FingerTree Size (Elem a))

fromList :: (Functor t, Foldable t) => t a -> Seq a
fromList = Seq . toTree . (Elem <$>)
