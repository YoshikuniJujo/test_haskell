{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import GHC.Generics
import Data.Bits

data Bit = O | I deriving Show

class Serialize a where
	put :: a -> [Bit]

instance Serialize a => Serialize [a] where
	put = \case [] -> []; h : t -> put h ++ put t

instance Serialize Int where
	put i = serializeInt i

serializeInt :: Int -> [Bit]
serializeInt = serializeBits

serializeBits :: FiniteBits n => n -> [Bit]
serializeBits n = boolToBit . testBit n <$> [0 .. s - 1]
	where
	s = finiteBitSize n
	boolToBit = \case False -> O; True -> I

data UserTree a = Node a (UserTree a) (UserTree a) | Leaf

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

-- type RepUserTree a = U1 :+: a :*: UserTree a :*: UserTree a

{-
instance Generic (UserTree a) where
	type Rep (UserTree a) = RepUserTree a

	from Leaf = L1 U1
	from (Node a l r) = R1 (a :*: l :*: r)

	to (L1 U1) = Leaf
	to (R1 (a :*: l :*: r)) = Node a l r
	-}
