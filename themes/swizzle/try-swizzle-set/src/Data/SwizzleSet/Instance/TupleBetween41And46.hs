{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module Data.SwizzleSet.Instance.TupleBetween41And46 () where

import Data.SwizzleSet.Class.Base
import Data.SwizzleSet.Class.TH.Internal

concat <$> instanceSwizzleTuple `mapM` [41 .. 46]
