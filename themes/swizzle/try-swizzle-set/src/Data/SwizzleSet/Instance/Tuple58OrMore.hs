{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module Data.SwizzleSet.Instance.Tuple58OrMore where

import GHC.Exts
import Data.SwizzleSet.Class.Base
import Data.SwizzleSet.Class.TH

concat <$> instanceSwizzleTuple `mapM` [58 .. maxTupleSize]
