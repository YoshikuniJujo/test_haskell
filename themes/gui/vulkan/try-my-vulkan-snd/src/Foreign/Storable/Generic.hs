{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foreign.Storable.Generic where

import GHC.Generics
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.SizeAlignment
import Data.Word

class G a where
	gSizeOf :: a -> Int
	gAlignment :: a -> Int
	gPeek :: Ptr a -> IO a
	gPoke :: Ptr a -> a -> IO ()

	default gSizeOf :: SizeAlignmentList a => a -> Int
	gSizeOf _ = fst (wholeSizeAlignment @a)

	default gAlignment :: SizeAlignmentList a => a -> Int
	gAlignment _ = snd (wholeSizeAlignment @a)

	default gPeek :: (Generic a, Gg (Rep a)) => Ptr a -> IO a
	gPeek = (to <$>) . ggPeek . castPtr

	default gPoke :: (Generic a, Gg (Rep a)) => Ptr a -> a -> IO ()
	gPoke p = ggPoke (castPtr p) . from

class Gg f where
	ggSizeOf :: f a -> Int
	ggAlignment :: f a -> Int
	ggPeek :: Ptr (f a) -> IO (f a)
	ggPoke :: Ptr (f a) -> f a -> IO ()

instance Gg U1 where
	ggSizeOf _ = 0
	ggAlignment _ = 1
	ggPeek _ = pure U1
	ggPoke _ _ = pure ()

instance (Gg a, Gg b) => Gg (a :*: b) where
	ggSizeOf _ = ((ggSizeOf @a undefined - 1) `div` a + 1) * a + ggSizeOf @b undefined
		where a = ggAlignment @b undefined
	ggAlignment _ = ggAlignment @a undefined `lcm` ggAlignment @b undefined
	ggPeek p = (:*:) <$> ggPeek (castPtr p) <*> ggPeek (castPtr p')
		where
		p' = p `plusPtr` (((ggSizeOf @a undefined - 1) `div` a + 1) * a)
		a = ggAlignment @b undefined
	ggPoke p (x :*: y) = ggPoke (castPtr p) x >> ggPoke (castPtr p') y
		where
		p' = p `plusPtr` (((ggSizeOf @a undefined - 1) `div` a + 1) * a)
		a = ggAlignment @b undefined

{-
instance (Gg a, Gg b) => Gg (a :+: b) where
	ggSizeOf _ = ggSizeOf @a undefined `max` ggSizeOf @b undefined
	ggAlignment _ = ggAlignment @a undefined `lcm` ggAlignment @b undefined
	-}

instance Gg a => Gg (M1 i c a) where
	ggSizeOf (M1 x) = ggSizeOf x
	ggAlignment (M1 x) = ggAlignment x
	ggPeek = (M1 <$>) . ggPeek . castPtr
	ggPoke p (M1 x) = ggPoke (castPtr p) x

instance Storable a => Gg (K1 i a) where
	ggSizeOf (K1 x) = sizeOf x
	ggAlignment (K1 x) = alignment x
	ggPeek = (K1 <$>) . peek . castPtr
	ggPoke p (K1 x) = poke (castPtr p) x

newtype Wrap a = W { unW :: a } deriving (Show, Eq, Ord, Enum, Generic, SizeAlignmentList)

instance G a => Storable (Wrap a) where
	sizeOf = gSizeOf . unW
	alignment = gAlignment . unW
	peek = (W <$>) . gPeek . castPtr
	poke p = gPoke (castPtr p) . unW

instance SizeAlignmentListUntil a b => SizeAlignmentListUntil a (Wrap b) where
	sizeAlignmentListUntil = sizeAlignmentListUntil @a @b

{-
instance {-# OVERLAPPABLE #-} G a => Storable a where
	sizeOf = gSizeOf
	alignment = gAlignment
	peek = gPeek
	poke = gPoke
	-}

instance G Word32 where
	gSizeOf = sizeOf
	gAlignment = alignment
	gPeek = peek
	gPoke = poke

instance (Storable a, Storable b) => G (a, b)
