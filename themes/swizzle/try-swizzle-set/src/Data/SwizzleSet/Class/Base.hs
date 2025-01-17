{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module Data.SwizzleSet.Class.Base where

import Data.SwizzleSet.Class.TH.Internal

concat <$> classSwizzle `mapM` [1 .. 26]
concat <$> instanceSwizzleTuple `mapM` [1 .. 26]
