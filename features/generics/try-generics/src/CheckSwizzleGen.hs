{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CheckSwizzleGen where

import GHC.Generics

import SwizzleGen

classGswizzle 1
instanceGswizzle1K1
instanceGswizzleM1 1

instanceGswizzle1Prod

classSwizzle1

instance Swizzle1 (x, y) where type X (x, y) = x
instance Swizzle1 (x, y, z) where type X (x, y, z) = x

classGswizzle 2
instanceGswizzleM1 2
