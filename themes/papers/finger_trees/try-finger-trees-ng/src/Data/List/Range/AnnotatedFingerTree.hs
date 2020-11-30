{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.Range.AnnotatedFingerTree where

-- import GHC.TypeNats

import Data.List.Range
import Internal.Tools

class Monoid v => Measured a v where measure :: a -> v

data Node v a = Node v (RangeL 2 3 a) deriving Show

node2 :: Measured a v => a -> a -> Node v a
node2 a b = Node (measure a <> measure b) $ a :. b :. NilL

node3 :: Measured a v => a -> a -> a -> Node v a
node3 a b c = Node (measure a <> measure b <> measure c) $ a :. b :. c :.. NilL

instance Monoid v => Measured (Node v a) v where measure (Node v _) = v

type DigitL = RangeL 1 4
type DigitR = RangeR 1 4

instance (Foldable (RangeL n m), Measured a v) => Measured (RangeL n m a) v where
	measure xs = reducel (\i a -> i <> measure a) mempty xs

instance (Foldable (RangeR n m), Measured a v) => Measured (RangeR n m a) v where
	measure xs = reducer (\a i -> measure a <> i) xs mempty

data FingerTree v a
	= Empty | Single a
	| Deep v (DigitL a) (FingerTree v (Node v a)) (DigitR a)
	deriving Show

deep :: Measured a v =>
	DigitL a -> FingerTree v (Node v a) -> DigitR a -> FingerTree v a
deep pr m sf = Deep (measure pr <> measure m <> measure sf) pr m sf

instance Measured a v => Measured (FingerTree v a) v where
	measure Empty = mempty
	measure (Single x) = measure x
	measure (Deep v _ _ _) = v

infixr 5 <||, <|, <|.

(<||) :: Measured a v => a -> DigitL a -> Either (DigitL a) (DigitL a, Node v a)
a <|| b :. NilL = Left $ a :. b :.. NilL
a <|| b :. c :.. NilL = Left $ a :. b :.. c :.. NilL
a <|| b :. c :.. d :.. NilL = Left $ a :. b :.. c :.. d :.. NilL
a <|| b :. c :.. d :.. e :.. NilL = Right (a :. b :.. NilL, node3 c d e)
_ <|| _ = error "never occur"

(<|) :: Measured a v => a -> FingerTree v a -> FingerTree v a
a <| Empty = Single a
a <| Single b = deep (a :. NilL) Empty (NilR :+ b)
a <| Deep _ pr m sf = case a <|| pr of
	Left pr' -> deep pr' m sf
	Right (pr', n3) -> deep pr' (n3 <| m) sf

(<|.) :: (Measured a v, Foldable t) => t a -> FingerTree v a -> FingerTree v a
(<|.) = reducer (<|)

toTree :: (Measured a v, Foldable t) => t a -> FingerTree v a
toTree = (<|. Empty)

newtype Size = Size { getSize :: Int } deriving (Show, Eq, Ord)

instance Semigroup Size where Size m <> Size n = Size $ m + n
instance Monoid Size where mempty = Size 0

newtype Elem a = Elem { getElem :: a } deriving Show

instance Measured (Elem a) Size where measure _ = Size 1
