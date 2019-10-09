{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs, DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module AnnotatedFingerTree where

import Range

reducer :: Foldable t => (a -> b -> b) -> t a -> b -> b
reducer = flip . foldr

reducel :: Foldable t => (b -> a -> b) -> b -> t a -> b
reducel = foldl

class Monoid v => Measured a v where
	measure :: a -> v

data Node v a = Node v (Range 2 3 a) deriving Show

node2 :: Measured a v => a -> a -> Node v a
node2 a b = Node (measure a <> measure b) $ a :. b :. Nil

node3 :: Measured a v => a -> a -> a -> Node v a
node3 a b c = Node (measure a <> measure b <> measure c) $ a :. b :. c :.. Nil

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

infixr 5 <||, <|, <|.

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

(<|.) :: (Measured a v, Foldable t) => t a -> FingerTree v a -> FingerTree v a
(<|.) = reducer (<|)

toTree :: (Measured a v, Foldable t) => t a -> FingerTree v a
toTree = (<|. Empty)

infixl 5 ||>, |>, |>.

(||>) :: Measured a v => Digit a -> a -> Either (Digit a) (Node v a, Digit a)
a :. Nil ||> b = Left $ a :. b :.. Nil
a :. b :.. Nil ||> c = Left $ a :. b :.. c :.. Nil
a :. b :.. c :.. Nil ||> d = Left $ a :. b :.. c :.. d :.. Nil
a :. b :.. c :.. d :.. Nil ||> e = Right (node3 a b c, d :. e :.. Nil)
_ ||> _ = error "never occur"

(|>) :: Measured a v => FingerTree v a -> a -> FingerTree v a
Empty |> a = Single a
Single a |> b = deep (a :. Nil) Empty (b :. Nil)
Deep _ pr m sf |> a = case sf ||> a of
	Left sf' -> deep pr m sf'
	Right (n3, sf') -> deep pr (m |> n3) sf'

(|>.) :: (Measured a v, Foldable t) => FingerTree v a -> t a -> FingerTree v a
(|>.) = reducel (|>)

data ViewL s a = NilL | ConsL a (s a) deriving Show

viewL :: Measured a v => FingerTree v a -> ViewL (FingerTree v) a
viewL Empty = NilL
viewL (Single x) = ConsL x Empty
viewL (Deep _ (a :. pr') m sf) = ConsL a $ deepL pr' m sf

nodeToDigit :: Node v a -> Digit a
nodeToDigit (Node _ xs) = loosen xs

deepL :: Measured a v =>
	Range 0 3 a -> FingerTree v (Node v a) -> Digit a -> FingerTree v a
deepL Nil m sf = case viewL m of
	NilL -> toTree sf
	ConsL a m' -> deep (nodeToDigit a) m' sf
deepL (a :.. pr) m sf = deep (loosen $ a :. pr) m sf
deepL _ _ _ = error "never occur"

newtype Size = Size { getSize :: Int } deriving (Show, Eq, Ord)
instance Semigroup Size where Size m <> Size n = Size $ m + n
instance Monoid Size where mempty = Size 0

newtype Elem a = Elem { getElem :: a } deriving Show
instance Measured (Elem a) Size where measure _ = Size 1

sampleList :: [Int]
sampleList = [1 .. 7]

sampleFTree :: FingerTree Size (Elem Int)
sampleFTree = toTree $ Elem <$> sampleList
