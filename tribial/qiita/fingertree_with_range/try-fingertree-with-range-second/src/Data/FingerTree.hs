{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
	UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}

module Data.FingerTree where

import GHC.TypeNats

import Data.List.Range

data FingerTree a
	= Empty
	| Single a
	| Deep (DigitL a) (FingerTree (Node a)) (DigitR a)
	deriving Show

type Node = RangeL 2 3
type DigitL = RangeL 1 4
type DigitR = RangeR 1 4

reducer :: Foldable t => (a -> b -> b) -> t a -> b -> b
reducer = flip . foldr

reducel :: Foldable t => (b -> a -> b) -> b -> t a -> b
reducel = foldl

instance Foldable FingerTree where
	foldr :: forall a b . (a -> b -> b) -> b -> FingerTree a -> b
	foldr _ z Empty = z
	foldr (-<) z (Single x) = x -< z
	foldr (-<) z (Deep pr m sf) = pr -<. (m -<.. (sf -<. z))
		where
		(-<.) :: forall t . Foldable t => t a -> b -> b
		(-<.) = reducer (-<)
		(-<..) :: forall t t' .
			(Foldable t, Foldable t') => t (t' a) -> b -> b
		(-<..) = reducer (-<.)

infixr 5 <||

(<||) :: a -> DigitL a -> Either (DigitL a) (DigitL a, Node a)
a <|| b :. NilL = Left $ a :. b :.. NilL
a <|| b :. c :.. NilL = Left $ a :. b :.. c :.. NilL
a <|| b :. c :.. d :.. NilL = Left $ a :. b :.. c :.. d :.. NilL
a <|| b :. c :.. d :.. e :.. NilL =
	Right (a :. b :.. NilL, c :. d :. e :.. NilL)
_ <|| _ = error "never occur"

infixr 5 <|

(<|) :: a -> FingerTree a -> FingerTree a
a <| Empty = Single a
a <| Single b = Deep (a :. NilL) Empty (NilR :+ b)
a <| Deep pr m sf = case a <|| pr of
	Left pr' -> Deep pr' m sf
	Right (pr', n3) -> Deep pr' (n3 <| m) sf

infixr 5 <|.

(<|.) :: Foldable t => t a -> FingerTree a -> FingerTree a
(<|.) = reducer (<|)

mkFingerTree :: Foldable t => t a -> FingerTree a
mkFingerTree = (<|. Empty)

uncons :: FingerTree a -> Maybe (a, FingerTree a)
uncons Empty = Nothing
uncons (Single x) = Just (x, Empty)
uncons (Deep (a :. pr') m sf) = Just (a, deepL pr' m sf)

deepL :: RangeL 0 3 a -> FingerTree (Node a) -> DigitR a -> FingerTree a
deepL NilL m sf = case uncons m of
	Nothing -> mkFingerTree sf
	Just (n, m') -> Deep (nodeToDigitL n) m' sf
deepL (a :.. pr) m sf = Deep (loosenL $ a :. pr) m sf
deepL _ _ _ = error "never occur"

nodeToDigitL :: Node a -> DigitL a
nodeToDigitL = loosenL
