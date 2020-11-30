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
