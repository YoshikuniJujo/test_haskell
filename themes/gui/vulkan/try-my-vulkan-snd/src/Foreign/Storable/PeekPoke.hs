{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foreign.Storable.PeekPoke where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable

class Sizable a where sizeOf' :: Int; alignment' :: Int
class Peek a where peek' :: Ptr a -> IO a
class Poke a where poke' :: Ptr a -> a -> IO ()

class (Sizable a, Peek a) => Peekable a
instance (Sizable a, Peek a) => Peekable a

class (Sizable a, Poke a) => Pokable a
instance (Sizable a, Poke a) => Pokable a

class (Peekable a, Pokable a) => Storable' a
instance (Peekable a, Pokable a) => Storable' a

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

newtype PtrS s a = PtrS_ (Ptr a) deriving Show

ptrS :: Ptr a -> PtrS s a
ptrS = PtrS_

withPtrS :: PtrS s a -> (Ptr a -> IO b) -> IO ()
withPtrS (PtrS_ p) = (() <$) . ($ p)

class WithPoked a where
	withPoked' :: a -> (forall s . PtrS s a -> IO b) -> IO b

instance {-# OVERLAPPABLE #-} Pokable a => WithPoked a where
	withPoked' x f = withPoked x $ f . ptrS
