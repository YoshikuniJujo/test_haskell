{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeCheck.Test.AnnotatedFingerTree where

import GHC.TypeLits

import TypeCheck.Test.Range

reducer :: Foldable t => (a -> b -> b) -> t a -> b -> b
reducer = flip . foldr

reducel :: Foldable t => (b -> a -> b) -> b -> t a -> b
reducel = foldl

class Monoid v => Measured a v where measure :: a -> v

data Node v a = Node v (RangeL 2 3 a) deriving Show

node2 :: Measured a v => a -> a -> Node v a
node2 a b = Node (measure a <> measure b) $ a :. b :. NilL

node3 :: Measured a v => a -> a -> a -> Node v a
node3 a b c = Node (measure a <> measure b <> measure c) $ a :. b :. c :.. NilL

instance Foldable (Node v) where
	foldr (-<) z (Node _ xs) = foldr (-<) z xs
	foldl (>-) z (Node _ xs) = foldl (>-) z xs

instance Monoid v => Measured (Node v a) v where measure (Node v _) = v

type DigitL = RangeL 1 4
type DigitR = RangeR 1 4

instance Measured a v => Measured (DigitL a) v where
	measure = reducel (\i a -> i <> measure a) mempty

instance Measured a v => Measured (DigitR a) v where
	measure = reducel (\i a -> i <> measure a) mempty

data FingerTree v a
	= Empty
	| Single a
	| Deep v (DigitL a) (FingerTree v (Node v a)) (DigitR a)
	deriving Show

deep :: Measured a v =>
	DigitL a -> FingerTree v (Node v a) -> DigitR a -> FingerTree v a
deep pr m sf = Deep (measure pr <> measure m <> measure sf) pr m sf

instance Measured a v => Measured (FingerTree v a) v where
	measure Empty = mempty
	measure (Single x) = measure x
	measure (Deep v _ _ _) = v

instance Foldable (FingerTree v) where
	foldr :: forall a b . (a -> b -> b) -> b -> FingerTree v a -> b
	foldr _ z Empty = z
	foldr (-<) z (Single x) = x -< z
	foldr (-<) z (Deep _ pr m sf) = pr -<. (m -<.. (sf -<. z))
		where	
		(-<.) :: forall t . Foldable t => t a -> b -> b
		(-<.) = reducer (-<)
		(-<..) :: forall t t' . (Foldable t, Foldable t') => t (t' a) -> b -> b
		(-<..) = reducer (-<.)
	foldl :: forall a b . (b -> a -> b) -> b -> FingerTree v a -> b
	foldl _ z Empty = z
	foldl (>-) z (Single x) = z >- x
	foldl (>-) z (Deep _ pr m sf) = ((z >-. pr) >-.. m) >-. sf
		where
		(>-.) :: forall t . Foldable t => b -> t a -> b
		(>-.) = reducel (>-)
		(>-..) :: forall t t' . (Foldable t, Foldable t') => b -> t (t' a) -> b
		(>-..) = reducel (>-.)

infixr 5 <||, <|, <|.

(<||) :: Measured a v => a -> DigitL a -> Either (DigitL a) (DigitL a, Node v a)
a <|| b :. NilL = Left $ a :. b :.. NilL
a <|| b :. c :.. NilL = Left $ a :. b :.. c :.. NilL
a <|| b :. c :.. d :.. NilL = Left $ a :. b :.. c :.. d :.. NilL
a <|| b :. c :.. d :.. e :.. NilL = Right (a :. b :.. NilL, node3 c d e)
_ <|| _ = error "never occur"

(<|) :: Measured a v => a -> FingerTree v a -> FingerTree v a
a <| Empty = Single a
a <| Single b = deep (a :. NilL) Empty (NilR :+ b)
a <| Deep _ pr m sf = case a <|| pr of
	Left pr' -> deep pr' m sf
	Right (pr', n3) -> deep pr' (n3 <| m) sf

(<|.) :: (Measured a v, Foldable t) => t a -> FingerTree v a -> FingerTree v a
(<|.) = reducer (<|)

toTree :: (Measured a v, Foldable t) => t a -> FingerTree v a
toTree = (<|. Empty)

instance Measured Char () where measure _ = ()
