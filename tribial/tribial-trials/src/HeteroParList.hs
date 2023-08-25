{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module HeteroParList where

import Data.Kind

infixr 5 :.

data HeteroParList t ss where
	Nil :: HeteroParList t '[]
	(:.) :: t s -> HeteroParList t ss -> HeteroParList t (s ': ss)

instance Show (HeteroParList t '[]) where show Nil = "Nil"

instance (Show (t s), Show (HeteroParList t ss)) =>
	Show (HeteroParList t (s ': ss)) where
	show (x :. xs) = show x ++ " :. " ++ show xs

fromList :: (forall s . a -> t s) -> [a] ->
	(forall ss . HeteroParList t ss -> a) -> a
fromList _ [] f = f Nil
fromList c (x : xs) f = fromList c xs \ys -> f $ c x :. ys

newtype Foo s = Foo s deriving Show

class ShowHetero (ss :: [Type]) where
	showHetero :: HeteroParList Foo ss -> String

instance ShowHetero '[] where showHetero Nil = "Nil"

instance (Show s, ShowHetero ss) => ShowHetero (s ': ss) where
	showHetero (x :. xs) = show x ++ " :. " ++ showHetero xs

fooFromList :: Show t => (forall s . s -> Foo s) -> [t] ->
	(forall ss . ShowHetero ss => HeteroParList Foo ss -> a) -> a
fooFromList _ [] f = f Nil
fooFromList c (x : xs) f = fooFromList c xs \ys -> f $ c x :. ys

newtype Bar s = Bar Int deriving Show

class ShowBars ss where
	showBars :: HeteroParList Bar ss -> String

instance ShowBars '[] where showBars Nil = "Nil"

instance ShowBars ss => ShowBars (s ': ss) where
	showBars (x :. xs) = show x ++ " :. " ++ showBars xs

barsFromList :: (forall s . x -> Bar s) -> [x] ->
	(forall ss . ShowBars ss => HeteroParList Bar ss -> a) -> a
barsFromList _ [] f = f Nil
barsFromList c (x : xs) f = barsFromList c xs \ys -> f $ c x :. ys
