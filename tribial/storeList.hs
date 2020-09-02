{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal

data Arr a = Arr (Ptr a) deriving Show

withArr :: forall a b . Storable a => [a] -> (Arr a -> IO b) -> IO b
withArr xs f = allocaBytes (sizeOf @Int undefined + sizeOf @a undefined * length xs) \p -> do
	poke (castPtr p) $ length xs
	pokeArray (p `plusPtr` sizeOf @Int undefined) xs
	f $ Arr p

peekArr :: Storable a => Arr a -> IO [a]
peekArr (Arr p) = do
	n <- peek $ castPtr p
	peekArray n (p `plusPtr` sizeOf @Int undefined)
