{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TrySwizzleClass where

import GHC.Generics
import Data.List

import SwizzleClass
import SwizzleFun

data Point3d = Point3d Double Double Double deriving (Show, Generic)

instance Swizzle1 Point3d where type X Point3d = Double
instance Swizzle2 Point3d where type Y Point3d = Double
instance Swizzle3 Point3d where type Z Point3d = Double

-- concat <$> swizzle `mapM` permutations "xyz"
concat <$> swizzle `mapM` [ [x_, y_, z_] | x_ <- "xyz", y_ <- "xyz", z_ <- "xyz" ]

swizzle "yyzxx"
