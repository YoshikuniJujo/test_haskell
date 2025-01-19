{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Language.Haskell.TH

import Data.Swizzle.Class qualified as Swz
import Data.SwizzleSet.Class qualified as SwzS

someFunc :: IO ()
someFunc = putStrLn "someFunc"

x :: (Swz.Swizzle1 a, SwzS.SwizzleSet1 a, Swz.X a ~ SwzS.X a, Functor f) =>
	(Swz.X a -> f (Swz.X a)) -> a -> f a
x f a = flip SwzS.x a <$> f (Swz.x a)

xy :: (Swz.Swizzle2 a, SwzS.SwizzleSet2 a,
	Swz.X a ~ SwzS.X a, Swz.Y a ~ SwzS.Y a, Functor f) =>
	((Swz.X a, Swz.Y a) -> f (Swz.X a, Swz.Y a)) -> a -> f a
xy f a = s <$> f (Swz.x a, Swz.y a)
	where
	s (x', y') = SwzS.x x' . SwzS.y y' $ a

newtype Const c a = Const { runConst :: c } deriving Show

instance Functor (Const c) where _ `fmap` Const c = Const c

newtype Identity a = Identity { runIdentity :: a } deriving Show

instance Functor Identity where f `fmap` Identity x = Identity $ f x

tdec :: DecsQ
tdec = [d|
	xy :: Int
	xy = undefined
	|]
