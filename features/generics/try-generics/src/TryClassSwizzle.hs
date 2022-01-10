{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module TryClassSwizzle where

import qualified GHC.Generics as G
import Data.List

import SwizzleGen

concat <$> classSwizzle `mapM` [1 .. 19]

xx :: Swizzle1 a => a -> (X a, X a)
xx a_ = (x a_, x a_)

xxx :: Swizzle1 a => a -> (X a, X a, X a)
xxx a_ = (x a_, x a_, x a_)

xy :: Swizzle2 a => a -> (X a, Y a)
xy a_ = (x a_, y a_)

yx :: Swizzle2 a => a -> (Y a, X a)
yx a_ = (y a_, x a_)

ypq :: Swizzle11 a => a -> (Y a, P a, Q a)
ypq a_ = (y a_, p a_, q a_)

swizzle "yps"

concat <$> swizzle `mapM` permutations "xyzw"

swizzle "zzxw"

data Point3d = Point3d Double Double Double deriving (Show, G.Generic)

concat <$> swizzle `mapM` permutations "xyz"

instance Swizzle1 Point3d where type X Point3d = Double
instance Swizzle2 Point3d where type Y Point3d = Double
instance Swizzle3 Point3d where type Z Point3d = Double
