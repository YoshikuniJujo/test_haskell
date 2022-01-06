{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryGenerics where

import GHC.Generics
import Data.Bits

data UserTree a = Node a (UserTree a) (UserTree a) | Leaf
	deriving Generic

class Serialize a where
	put :: a -> [Bit]

	default put :: (Generic a, GSerialize (Rep a)) => a -> [Bit]
	put a = gput (from a)

instance Serialize a => Serialize (UserTree a)

data Bit = O | I deriving Show

class GSerialize f where
	gput :: f a -> [Bit]

instance GSerialize U1 where
	gput U1 = []

instance (GSerialize a, GSerialize b) => GSerialize (a :*: b) where
	gput (a :*: b) = gput a ++ gput b

instance (GSerialize a, GSerialize b) => GSerialize (a :+: b) where
	gput = \case L1 x -> O : gput x; R1 x -> I : gput x

instance GSerialize a => GSerialize (M1 i c a) where
	gput (M1 x) = gput x

instance (Serialize a) => GSerialize (K1 i a) where
	gput (K1 x) = put x

instance Serialize Int where
	put i = serializeInt i

serializeInt :: Int -> [Bit]
serializeInt = serializeBits

serializeBits :: FiniteBits n => n -> [Bit]
serializeBits n = boolToBit . testBit n <$> [0 .. s - 1]
	where
	s = finiteBitSize n
	boolToBit = \case False -> O; True -> I
