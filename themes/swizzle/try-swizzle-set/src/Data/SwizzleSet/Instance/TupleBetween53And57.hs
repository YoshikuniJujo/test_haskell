{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module Data.SwizzleSet.Instance.TupleBetween53And57 () where

import Data.SwizzleSet.Class.Base
import Data.SwizzleSet.Class.TH.Internal

concat <$> instanceSwizzleTuple `mapM` [53 .. 57]
