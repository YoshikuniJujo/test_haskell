{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module TryClassSwizzle where

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
