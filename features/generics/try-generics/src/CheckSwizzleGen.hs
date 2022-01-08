{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CheckSwizzleGen where

import SwizzleGen

classSwizzle 1

instance Swizzle1 (x, y) where type X (x, y) = x
instance Swizzle1 (x, y, z) where type X (x, y, z) = x
instance Swizzle1 (x, y, z, w) where type X (x, y, z, w) = x
instance Swizzle1 (x, y, z, w, v) where type X (x, y, z, w, v) = x

classSwizzle 2

instance Swizzle2 (x, y) where type Y (x, y) = y
instance Swizzle2 (x, y, z) where type Y (x, y, z) = y
instance Swizzle2 (x, y, z, w) where type Y (x, y, z, w) = y
instance Swizzle2 (x, y, z, w, v) where type Y (x, y, z, w, v) = y

classSwizzle 3

instance Swizzle3 (x, y, z) where type Z (x, y, z) = z
instance Swizzle3 (x, y, z, w) where type Z (x, y, z, w) = z
instance Swizzle3 (x, y, z, w, v) where type Z (x, y, z, w, v) = z

classSwizzle 4

instance Swizzle4 (x, y, z, w) where type W (x, y, z, w) = w
instance Swizzle4 (x, y, z, w, v) where type W (x, y, z, w, v) = w

classSwizzle 5

instance Swizzle5 (x, y, z, w, v) where type V (x, y, z, w, v) = v
