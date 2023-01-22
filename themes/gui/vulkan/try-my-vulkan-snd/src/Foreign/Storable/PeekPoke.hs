{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
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

instance Storable a => Sizable a where
	sizeOf' = sizeOf @a undefined
	alignment' = alignment @a undefined

instance Storable a => Peek a where peek' = peek
instance Storable a => Poke a where poke' = poke

withPoked :: Pokable a => a -> (Ptr a -> IO b) -> IO b
withPoked x f = alloca' \p -> poke' p x >> f p

alloca' :: forall a b . Sizable a => (Ptr a -> IO b) -> IO b
alloca' = allocaBytesAligned (sizeOf' @a) (alignment' @a)
