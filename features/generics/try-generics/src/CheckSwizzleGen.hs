{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CheckSwizzleGen where

import GHC.Generics

import SwizzleGen

classSwizzle 1

instance Swizzle1 (x, y) where type X (x, y) = x
instance Swizzle1 (x, y, z) where type X (x, y, z) = x

classSwizzle 2

instance Swizzle2 (x, y) where type Y (x, y) = y
instance Swizzle2 (x, y, z) where type Y (x, y, z) = y

classSwizzle 3

instance Swizzle3 (x, y, z) where type Z (x, y, z) = z
