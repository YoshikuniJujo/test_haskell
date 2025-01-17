{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module Data.SwizzleSet.Instance.TupleBetween31And40 () where

import Data.SwizzleSet.Class.Base
import Data.SwizzleSet.Class.TH.Internal

concat <$> instanceSwizzleTuple `mapM` [31 .. 40]
