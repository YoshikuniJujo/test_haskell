{-# LANGUAGE ScopedTypeVariables #-}

module Storable where

import Foreign.Storable
import Data.Word

instance Storable a => Storable (Maybe a) where
	sizeOf _ = sizeOf (undefined :: Bool) + sizeOf (undefined :: a)
	alignment _ = max
		(alignment (undefined :: Bool))
		(alignment (undefined :: a))
	peek ptr = do
		isJ :: Bool <- peekByteOff ptr 0
		if not isJ then return Nothing else do
			v :: a <- peekByteOff ptr (sizeOf (undefined :: Bool))
			return (Just v)
	poke ptr (Just v) = do
		pokeByteOff ptr 0 True
		pokeByteOff ptr (sizeOf (undefined :: Bool)) v
	poke ptr _ = pokeByteOff ptr 0 False

instance (Storable a, Storable b) => Storable (Either a b) where
