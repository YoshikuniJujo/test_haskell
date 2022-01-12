{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TrySwizzleFun where

import Data.List

import SwizzleFun

swizzle "zyx"

concat <$> swizzle `mapM` permutations "xyzwv"
