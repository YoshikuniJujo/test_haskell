{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foreign.Storable.PeekPoke.Internal (

	-- * Sizable, Peek and Poke

	Sizable(..), Peek(..), peekMaybe, Poke(..),

	-- * Peekable, WithPoked and Pokable

	Peekable, peekArray',
	WithPoked(..), PtrS(..), ptrS, withPtrS, withPokedMaybe',
	Pokable, withPoked, withPokedMaybe,

	-- * Storable' and NullPtr

	Storable', pattern NullPtr ) where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe

class Sizable a where sizeOf' :: Int; alignment' :: Int
class Peek a where peek' :: Ptr a -> IO a
class Poke a where poke' :: Ptr a -> a -> IO ()

class (Sizable a, Peek a) => Peekable a
instance (Sizable a, Peek a) => Peekable a

class (Sizable a, Poke a, WithPoked a) => Pokable a
instance (Sizable a, Poke a) => Pokable a

class (Peekable a, Pokable a, Storable a) => Storable' a
instance (Peekable a, Pokable a, Storable a) => Storable' a

instance {-# OVERLAPPABLE #-} Storable a => Sizable a where
	sizeOf' = sizeOf @a undefined
	alignment' = alignment @a undefined

instance {-# OVERLAPPABLE #-} Storable a => Peek a where peek' = peek
instance {-# OVERLAPPABLE #-} Storable a => Poke a where poke' = poke

withPoked :: Pokable a => a -> (Ptr a -> IO b) -> IO b
withPoked x f = alloca' \p -> poke' p x >> f p

withPokedMaybe :: Pokable a => Maybe a -> (Ptr a -> IO b) -> IO b
withPokedMaybe = \case Nothing -> ($ NullPtr); Just x -> withPoked x

alloca' :: forall a b . Sizable a => (Ptr a -> IO b) -> IO b
alloca' = allocaBytesAligned (sizeOf' @a) (alignment' @a)

pattern NullPtr :: Ptr a
pattern NullPtr <- ((== nullPtr) -> True) where NullPtr = nullPtr

peekMaybe :: Peek a => Ptr a -> IO (Maybe a)
peekMaybe = \case NullPtr -> pure Nothing; p -> Just <$> peek' p

peekArray' :: forall a . Peekable a => Int -> Ptr a -> IO [a]
peekArray' n ((`alignPtr` (alignment' @a)) -> p)
	| n <= 0 = pure []
	| True = (:) <$> peek' p <*> peekArray' (n - 1) (p `plusPtr` sizeOf' @a)

newtype PtrS s a = PtrS_ (Ptr a) deriving Show

ptrS :: Ptr a -> PtrS s a
ptrS = PtrS_

castPtrS :: PtrS s a -> PtrS s b
castPtrS (PtrS_ p) = PtrS_ $ castPtr p

pattern NullPtrS :: PtrS s a
pattern NullPtrS <- PtrS_ NullPtr where NullPtrS = PtrS_ NullPtr

withPtrS :: PtrS s a -> (Ptr a -> IO b) -> IO ()
withPtrS (PtrS_ p) = (() <$) . ($ p)

class WithPoked a where
	withPoked' :: a -> (forall s . PtrS s a -> IO b) -> IO b

instance {-# OVERLAPPABLE #-} Pokable a => WithPoked a where
	withPoked' x f = withPoked x $ f . ptrS

withPokedMaybe' :: WithPoked a =>
	Maybe a -> (forall s . PtrS s a -> IO b) -> IO b
withPokedMaybe' = \case Nothing -> ($ NullPtrS); Just x -> withPoked' x

instance WithPoked (TPMaybe.M t 'Nothing) where
	withPoked' TPMaybe.N f = f $ ptrS nullPtr

instance WithPoked (t a) => WithPoked (TPMaybe.M t ('Just a)) where
	withPoked' (TPMaybe.J x) f = withPoked' x (f . castPtrS)

instance {-# OVERLAPPABLE #-} WithPoked a => WithPoked (TMaybe.Id a) where
	withPoked' (TMaybe.Id x) f = withPoked' x (f . castPtrS)
