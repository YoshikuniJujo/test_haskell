{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.SwizzleModifyBase where

import Data.Swizzle.Class qualified as Swz
import Data.SwizzleSet.Class qualified as SwzS

x :: (Swz.Swizzle1 a, SwzS.SwizzleSet1 a, Swz.X a ~ SwzS.X a) =>
	(Swz.X a -> Swz.X a) -> a -> a
x f a = SwzS.x (f $ Swz.x a) a

y :: (Swz.Swizzle2 a, SwzS.SwizzleSet2 a, Swz.Y a ~ SwzS.Y a) =>
	(Swz.Y a -> Swz.Y a) -> a -> a
-- y f a = SwzS.y (f $ Swz.y a) a
y f = SwzS.y <$> (f . Swz.y) <*> id

s :: (a -> b -> c) -> (a -> b) -> a -> c
s x y = x <$> id <*> y

z :: (Swz.Swizzle3 a, SwzS.SwizzleSet3 a, Swz.Z a ~ SwzS.Z a) =>
	(Swz.Z a -> Swz.Z a) -> a -> a
z f a = SwzS.z (f $ Swz.z a) a

foo = 'SwzS.x
