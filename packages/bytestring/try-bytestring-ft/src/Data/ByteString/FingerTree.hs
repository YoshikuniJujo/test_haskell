{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.ByteString.FingerTree where

import Data.Foldable
import Data.ByteString qualified as BS

newtype ByteString = ByteString (FingerTree BS.ByteString)
	deriving Show

pattern Empty :: ByteString
pattern Empty = ByteString EmptyT

data FingerTree a
	= EmptyT
	| Single a
	| Deep {-# UNPACK #-} !Int !(Digit a) (FingerTree (Node a)) !(Digit a)
	deriving Show

data Digit a
	= One a
	| Two a a
	| Three a a a
	| Four a a a a
	deriving Show

foldDigit :: (b -> b -> b) -> (a -> b) -> Digit a -> b
foldDigit (<+>) f = \case
	One a -> f a
	Two a b -> f a <+> f b
	Three a b c -> f a <+> f b <+> f c
	Four a b c d -> f a <+> f b <+> f c <+> f d

instance Foldable Digit where
	foldMap = foldDigit mappend

	foldr f z = \case
		One a -> a `f` z
		Two a b -> a `f` (b `f` z)
		Three a b c -> a `f` (b `f` (c `f` z))
		Four a b c d -> a `f` (b `f` (c `f` (d `f` z)))
	{-# INLINE foldr #-}

	foldl f z = \case
		One a -> z `f` a
		Two a b -> (z `f` a) `f` b
		Three a b c -> ((z `f` a) `f` b) `f` c
		Four a b c d -> (((z `f` a) `f` b) `f` c) `f` d
	{-# INLINE foldl #-}

	foldr' f !z = \case
		One a -> f a z
		Two a b -> f a $! f b z
		Three a b c -> f a $! f b $! f c z
		Four a b c d -> f a $! f b $! f c $! f d z
	{-# INLINE foldr' #-}

	foldl' f !z = \case
		One a -> f z a
		Two a b -> (f $! f z a) b
		Three a b c -> (f $! (f $! f z a) b) c
		Four a b c d -> (f $! (f $! (f $! f z a) b) c) d
	{-# INLINE foldl' #-}

	foldr1 f = \case
		One a -> a
		Two a b -> a `f` b
		Three a b c -> a `f` (b `f` c)
		Four a b c d -> a `f` (b `f` (c `f` d))

	foldl1 f = \case
		One a -> a
		Two a b -> a `f` b
		Three a b c -> (a `f` b) `f` c
		Four a b c d -> ((a `f` b) `f` c) `f` d

instance Functor Digit where
	{-# INLINE fmap #-}
	fmap f = \case 
		One a -> One $ f a
		Two a b -> Two (f a) (f b)
		Three a b c -> Three (f a) (f b) (f c)
		Four a b c d -> Four (f a) (f b) (f c) (f d)

instance Sized a => Sized (Digit a) where
	{-# INLINE size #-}
	size = foldl1 (+) . fmap size

data Node a
	= Node2 {-# UNPACK #-} !Int a a
	| Node3 {-# UNPACK #-} !Int a a a
	deriving Show

{-
{-# INLINE deep #-}

deep :: Sized a => Digit a -> FingerTree (Node a) -> Digit a -> FingerTree a
deep pr m sf = Deep (size pr + size m + size sf) pr m sf
-}

class Sized a where size :: a -> Int
