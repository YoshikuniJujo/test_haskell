{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Arr where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal

newtype Arr a = Arr (Ptr a) deriving (Show, Storable)

withArr :: forall a b . Storable a => [a] -> (Arr a -> IO b) -> IO b
withArr xs f = allocaBytes (sizeOf @Int undefined + sizeOf @a undefined * length xs) \p -> do
	poke (castPtr p) $ length xs
	pokeArray (p `plusPtr` sizeOf @Int undefined) xs
	f $ Arr p

newArr :: forall a . Storable a => [a] -> IO (Arr a)
newArr xs = do
	p <- mallocBytes (sizeOf @Int undefined + sizeOf @a undefined * length xs)
	poke (castPtr p) $ length xs
	Arr p <$ pokeArray (p `plusPtr` sizeOf @Int undefined) xs

freeArr :: Arr a -> IO ()
freeArr (Arr p) = free p

peekArr :: Storable a => Arr a -> IO [a]
peekArr (Arr p) = do
	n <- peek $ castPtr p
	peekArray n (p `plusPtr` sizeOf @Int undefined)
