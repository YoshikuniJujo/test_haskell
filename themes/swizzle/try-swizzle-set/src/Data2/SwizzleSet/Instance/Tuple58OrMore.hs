{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module Data2.SwizzleSet.Instance.Tuple58OrMore () where

import GHC.Exts
import Data2.SwizzleSet.Class.Base
import Data2.SwizzleSet.Class.TH.Internal

concat <$> instanceSwizzleTuple `mapM` [58 .. maxTupleSize]
