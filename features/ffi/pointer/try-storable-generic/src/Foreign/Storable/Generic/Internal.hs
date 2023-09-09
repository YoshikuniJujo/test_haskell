{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foreign.Storable.Generic.Internal (G(..), W(..)) where

import GHC.Generics
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Data.Kind

class G a where
	gSizeOf :: a -> Int
	gAlignment :: a -> Int
	gPeek :: Ptr a -> IO a
	gPoke :: Ptr a -> a -> IO ()

	default gSizeOf :: MapTypeVal2 Sizable (Flatten (Rep a)) => a -> Int
	gSizeOf _ = fst (wholeSizeAlignmentNew @a)

	default gAlignment :: MapTypeVal2 Sizable (Flatten (Rep a)) => a -> Int
	gAlignment _ = snd (wholeSizeAlignmentNew @a)

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

newtype W a = W { unW :: a }
	deriving (Show, Eq, Ord, Enum)
	deriving newtype Generic

instance G a => Storable (W a) where
	sizeOf = gSizeOf . unW
	alignment = gAlignment . unW
	peek = (W <$>) . gPeek . castPtr
	poke p = gPoke (castPtr p) . unW

{-
instance {-# OVERLAPPABLE #-} G a => Storable a where
	sizeOf = gSizeOf
	alignment = gAlignment
	peek = gPeek
	poke = gPoke
	-}

wholeSizeAlignmentNew ::
	forall a . MapTypeVal2 Sizable (Flatten (Rep a)) => SizeAlignment
wholeSizeAlignmentNew = let sas = sizeAlignmentListNew @a in
	(calcWholeSize sas, calcWholeAlignment sas)

calcWholeAlignment :: [SizeAlignment] -> Alignment
calcWholeAlignment = foldl lcm 1 . (snd <$>)

calcWholeSize :: [SizeAlignment] -> Size
calcWholeSize = foldl next 0 . rotateAlignmentL

next :: Offset -> SizeAlignment -> Offset
next os (sz, algn) = ((os + sz - 1) `div` algn + 1) * algn

type Offset = Int

rotateAlignmentL :: [SizeAlignment] -> [SizeAlignment]
rotateAlignmentL [] = error "empty size and alignment list"
rotateAlignmentL sas = zip ss (as ++ [a]) where (ss, a : as) = unzip sas

sizeAlignmentListNew ::
	forall a . MapTypeVal2 Sizable (Flatten (Rep a)) => [SizeAlignment]
sizeAlignmentListNew = sizeAlignmentTypeList @(Flatten (Rep a))

sizeAlignmentTypeList ::
	forall (as :: [Type]) . MapTypeVal2 Sizable as => [SizeAlignment]
sizeAlignmentTypeList = mapTypeVal2 @Sizable @as (\(_ :: a) -> (sizeOf' @a, alignment' @a))

type Size = Int
type Alignment = Int
type SizeAlignment = (Size, Alignment)

class MapTypeVal2 c (as :: [Type]) where
	mapTypeVal2 :: (forall a . c a => a -> b) -> [b]

instance MapTypeVal2 c '[] where mapTypeVal2 _ = []

instance (c a, MapTypeVal2 c as) => MapTypeVal2 c (a ': as) where
	mapTypeVal2 x = x (undefined :: a) : mapTypeVal2 @c @as x

type family GetType (x :: Type -> Type) :: Type where
	GetType (K1 i a) = a
	GetType (M1 m i a) = GetType a

type family Flatten (x :: Type -> Type) :: [Type] where
	Flatten U1 = '[]
	Flatten (K1 i a) = '[a]
	Flatten (M1 m i a) = Flatten a
	Flatten (M1 m i a :*: t2) = GetType a ': Flatten t2
	Flatten ((t1 :*: t2) :*: t3) = Flatten (t1 :*: t2 :*: t3)
