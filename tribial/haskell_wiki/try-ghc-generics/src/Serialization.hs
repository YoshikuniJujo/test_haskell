{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Serialization where

import GHC.Generics
import Data.Bits

data Bit = O | I deriving Show

boolToBit :: Bool -> Bit
boolToBit = \case False -> O; True -> I

class Serialize a where
	put :: a -> [Bit]

	default put :: (Generic a, GSerialize (Rep a)) => a -> [Bit]
	put a = gput (from a)

instance {-# OVERLAPPABLE #-} FiniteBits n => Serialize n where
	put i = serializeFiniteBits i

serializeFiniteBits :: FiniteBits n => n -> [Bit]
serializeFiniteBits n = boolToBit . testBit n <$> [0 .. finiteBitSize n - 1]

instance Serialize a => Serialize [a] where
	put = \case [] -> []; h : t -> put h ++ put t

class GSerialize f where gput :: f a -> [Bit]

instance GSerialize U1 where gput U1 = []

instance (GSerialize a, GSerialize b) => GSerialize (a :*: b) where
	gput (a :*: b) = gput a ++ gput b

instance (GSerialize a, GSerialize b) => GSerialize (a :+: b) where
	gput = \case L1 x -> O : gput x; R1 x -> I : gput x

instance GSerialize a => GSerialize (M1 i c a) where gput (M1 x) = gput x
instance Serialize a => GSerialize (K1 i a) where gput (K1 x) = put x

data UserTree a =
	Node a (UserTree a) (UserTree a) | Tip deriving (Show, Generic)

instance Serialize a => Serialize (UserTree a)
