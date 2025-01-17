{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TrySwizzleSet where

import Data.Swizzle.Set.Class.TH.Internal

concat <$> classSwizzle `mapM` [1 .. 26]

instance SwizzleSet1 (x, b, c) where type X (x, b, c) = x
instance SwizzleSet2 (x, b, c) where type Y (x, b, c) = b
