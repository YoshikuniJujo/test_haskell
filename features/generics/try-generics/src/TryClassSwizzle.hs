{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module TryClassSwizzle where

import SwizzleGen

concat <$> classSwizzle `mapM` [1 .. 26]

ypq :: Swizzle11 a => a -> (Y a, P a, Q a)
ypq a_ = (y a_, p a_, q a_)
