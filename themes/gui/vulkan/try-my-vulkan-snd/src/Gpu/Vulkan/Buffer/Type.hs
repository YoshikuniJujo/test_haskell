{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Buffer.Type where

import GHC.TypeLits
import Data.Kind
import Data.HeteroParList qualified as HeteroParList

import Gpu.Vulkan.Object qualified as VObj
import Gpu.Vulkan.Buffer.Middle qualified as M

data B s (nm :: Symbol) (objs :: [VObj.O]) = B (HeteroParList.PL VObj.ObjectLength objs) M.B

deriving instance Show (HeteroParList.PL VObj.ObjectLength objs) => Show (B s nm objs)

data Binded (sm :: Type) (sb :: Type) (nm :: Symbol) (objs :: [VObj.O]) = Binded (HeteroParList.PL VObj.ObjectLength objs) M.B

deriving instance Show (HeteroParList.PL VObj.ObjectLength objs) => Show (Binded sm sb nm objs)

deriving instance Eq (HeteroParList.PL VObj.ObjectLength objs) => Eq (Binded sm sb nm objs)
