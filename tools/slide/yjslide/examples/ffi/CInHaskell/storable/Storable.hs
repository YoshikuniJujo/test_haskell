{-# LANGUAGE ScopedTypeVariables #-}

module Storable where

import Foreign.Storable
import Foreign.C.Types

align :: Int -> Int -> Int
align x a = (d + if m > 0 then 1 else 0) * a
	where
	(d, m) = x `divMod` a

instance Storable a => Storable (Maybe a) where
	sizeOf _ = sizeOf (undefined :: Bool) `align` alignment (undefined :: a) +
		sizeOf (undefined :: a)
	alignment _ = max
		(alignment (undefined :: Bool))
		(alignment (undefined :: a))
	peek ptr = do
		isJ :: Bool <- peekByteOff ptr 0
		if not isJ then return Nothing else do
			v :: a <- peekByteOff ptr (sizeOf (undefined :: Bool))
			return $ Just v
	poke ptr (Just v) = do
		pokeByteOff ptr 0 True
		pokeByteOff ptr (sizeOf (undefined :: Bool)) v
	poke ptr _ = pokeByteOff ptr 0 False

instance (Storable a, Storable b) => Storable (Either a b) where
	sizeOf _ = max ls rs
		where
		ls = sizeOf (undefined :: Bool) `align` alignment (undefined :: a) +
			sizeOf (undefined :: a)
		rs = sizeOf (undefined :: Bool) `align` alignment (undefined :: b) +
			sizeOf (undefined :: b)
	alignment _ = maximum [
		alignment (undefined :: Bool),
		alignment (undefined :: a),
		alignment (undefined :: b) ]
	peek ptr = do
		isR :: Bool <- peekByteOff ptr 0
		if not isR
		then do	v :: a <- peekByteOff ptr (sizeOf (undefined :: Bool))
			return $ Left v
		else do	v :: b <- peekByteOff ptr (sizeOf (undefined :: Bool))
			return $ Right v
	poke ptr (Left v) = do
		pokeByteOff ptr 0 False
		pokeByteOff ptr (sizeOf (undefined :: Bool)) v
	poke ptr (Right v) = do
		pokeByteOff ptr 0 True
		pokeByteOff ptr (sizeOf (undefined :: Bool)) v

data Shape = Circle CDouble | Rectangle CDouble CDouble deriving Show

instance Storable Shape where
	sizeOf _ = sizeOf (undefined :: CInt) `align`
		alignment (undefined :: CDouble) +
		2 * sizeOf (undefined :: CDouble)
	alignment _ = max
		(alignment (undefined :: CInt))
		(alignment (undefined :: CDouble))
	peek ptr = do
		typ :: CInt <- peekByteOff ptr tOffset
		case typ of
			0 -> do	r <- peekByteOff ptr rOffset
				return $ Circle r
			1 -> do	w <- peekByteOff ptr wOffset
				h <- peekByteOff ptr hOffset
				return $ Rectangle w h
		where
		tOffset = 0
		rOffset = sizeOf (undefined :: CInt)
		wOffset = sizeOf (undefined :: CInt)
		hOffset = wOffset + sizeOf (undefined :: CDouble)
	poke ptr (Circle r) = do
		pokeByteOff ptr tOffset (0 :: CInt)
		pokeByteOff ptr rOffset r
		where
		tOffset = 0
		rOffset = sizeOf (undefined :: CInt)
	poke ptr (Rectangle w h) = do
		pokeByteOff ptr tOffset (1 :: CInt)
		pokeByteOff ptr wOffset w
		pokeByteOff ptr hOffset h
		where
		tOffset = 0
		wOffset = sizeOf (undefined :: CInt)
		hOffset = wOffset + sizeOf (undefined :: CDouble)
