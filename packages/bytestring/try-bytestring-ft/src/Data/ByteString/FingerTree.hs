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

instance Sized a => Sized (FingerTree a) where
	{-# SPECIALIZE instance Sized (FingerTree (Node a)) #-}
	size EmptyT = 0
	size (Single x) = size x
	size (Deep v _ _ _) = v

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

foldNode :: (b -> b -> b) -> (a -> b) -> Node a -> b
foldNode (<+>) f (Node2 _ a b) = f a <+> f b
foldNode (<+>) f (Node3 _ a b c) = f a <+> f b <+> f c

instance Foldable Node where
	foldMap = foldNode mappend

	foldr f z = \case
		Node2 _ a b -> a `f` (b `f` z)
		Node3 _ a b c -> a `f` (b `f` (c `f` z))
	{-# INLINE foldr #-}

	foldl f z = \case
		Node2 _ a b -> (z `f` a) `f` b
		Node3 _ a b c -> ((z `f` a) `f` b) `f` c
	{-# INLINE foldl #-}

	foldr' f !z = \case
		Node2 _ a b -> f a $! f b z
		Node3 _ a b c -> f a $! f b $! f c z
	{-# INLINE foldr' #-}

	foldl' f !z = \case
		Node2 _ a b -> (f $! f z a) b
		Node3 _ a b c -> (f $! (f $! f z a) b) c

instance Functor Node where
	{-# INLINE fmap #-}
	fmap f = \case
		Node2 v a b -> Node2 v (f a) (f b)
		Node3 v a b c -> Node3 v (f a) (f b) (f c)

instance Sized (Node a) where
	size = \case Node2 v _ _ -> v; Node3 v _ _ _ -> v


{-# INLINE deep #-}

deep :: Sized a => Digit a -> FingerTree (Node a) -> Digit a -> FingerTree a
deep pr m sf = Deep (size pr + size m + size sf) pr m sf

class Sized a where size :: a -> Int
