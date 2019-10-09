{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs, DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module AnnotatedFingerTree where

import Range

reducel :: Foldable t => (b -> a -> b) -> b -> t a -> b
reducel = foldl

class Monoid v => Measured a v where
	measure :: a -> v

data Node v a = Node v (Range 2 3 a) deriving Show

node2 :: Measured a v => a -> a -> Node v a
node2 a b = Node (measure a <> measure b) $ a :. b :. Nil

node3 :: Measured a v => a -> a -> a -> Node v a
node3 a b c = Node (measure a <> measure b) $ a :. b :. c :.. Nil

instance Monoid v => Measured (Node v a) v where
	measure (Node v _) = v

type Digit = Range 1 4

instance Measured a v => Measured (Digit a) v where
	measure = reducel (\i a -> i <> measure a) mempty

data FingerTree v a
	= Empty
	| Single a
	| Deep v (Digit a) (FingerTree v (Node v a)) (Digit a)
	deriving Show

deep :: Measured a v =>
	Digit a -> FingerTree v (Node v a) -> Digit a -> FingerTree v a
deep pr m sf = Deep (measure pr <> measure m <> measure sf) pr m sf

instance Measured a v => Measured (FingerTree v a) v where
	measure Empty = mempty
	measure (Single x) = measure x
	measure (Deep v _ _ _) = v

infixr 5 <||, <|

(<||) :: Measured a v => a -> Digit a -> Either (Digit a) (Digit a, Node v a)
a <|| b :. Nil = Left $ a :. b :.. Nil
a <|| b :. c :.. Nil = Left $ a :. b :.. c :.. Nil
a <|| b :. c :.. d :.. Nil = Left $ a :. b :.. c :.. d :.. Nil
a <|| b :. c :.. d :.. e :.. Nil = Right (a :. b :.. Nil, node3 c d e)
_ <|| _ = error "never occur"

(<|) :: Measured a v => a -> FingerTree v a -> FingerTree v a
a <| Empty = Single a
a <| Single b = deep (a :. Nil) Empty (b :. Nil)
a <| Deep _ pr m sf = case a <|| pr of
	Left pr' -> deep pr' m sf
	Right (pr', n3) -> deep pr' (n3 <| m) sf
