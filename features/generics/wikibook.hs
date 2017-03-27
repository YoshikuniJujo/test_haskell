{-# LANGUAGE TypeOperators, DeriveGeneric, DefaultSignatures, FlexibleContexts #-}

import Data.Bits
import Data.List
import Data.Bool

import GHC.Generics

data Bit = O | I deriving (Show, Enum)

{-
class Serialize a where
	put :: a -> [Bit]
	-}

instance Serialize Int where
	put i = serializeInt i

instance Serialize a => Serialize [a] where
	put [] = []
	put (h : t) = put h ++ put t

serializeInt :: Int -> [Bit]
serializeInt =
	(\b -> ((++) <$> id <*> (`replicate` b) . (64 -) . length)
			. serializeBits)
		<$> bool O I . (< 0)
		<*> id

serializeBits :: (Num n, Bits n) => n -> [Bit]
serializeBits = unfoldr maybePop

maybePop :: (Num n, Bits n) => n -> Maybe (Bit, n)
maybePop = bool Nothing
	<$> Just . pop
	<*> ((&&) <$> (/= zeroBits) <*> (/= zeroBits) . complement)

pop :: (Num n, Bits n) => n -> (Bit, n)
pop = (,) <$> toEnum . fromEnum . (`testBit` 0) <*> (`shiftR` 1)

{-
type RepUserTree a =
	U1 :+:
	a :*: UserTree a :*: UserTree a

type RealRepUserTree a =
	M1 D Data_UserTree (
		M1 C Con_Leaf U1 :+:
		M1 C Con_Node (
			M1 S NoSelector (K1 P a) :*:
			M1 S NoSelector (K1 R (UserTree a)) :*:
			M1 S NoSelector (K1 R (UserTree a)) ) )

-}

class GSerialize f where
	gput :: f a -> [Bit]

instance GSerialize U1 where
	gput U1 = []

instance (GSerialize a, GSerialize b) => GSerialize (a :*: b) where
	gput (a :*: b) = gput a ++ gput b

instance (GSerialize a, GSerialize b) => GSerialize (a :+: b) where
	gput (L1 x) = O : gput x
	gput (R1 y) = I : gput y

instance (GSerialize a) => GSerialize (M1 i c a) where
	gput (M1 x) = gput x

instance (Serialize a) => GSerialize (K1 i a) where
	gput (K1 x) = put x

-- instance Generic (UserTree a) where

class Serialize a where
	put :: a -> [Bit]

	default put :: (Generic a, GSerialize (Rep a)) => a -> [Bit]
	put = gput . from

data UserTree a = Node a (UserTree a) (UserTree a) | Leaf
	deriving (Show, Generic)

instance Serialize a => Serialize (UserTree a)
