{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.BufferView where

import Data.TypeLevel.Uncurry
import Gpu.Vulkan.Object qualified as VObj

import Gpu.Vulkan.Device.Middle qualified as Device.M
import Gpu.Vulkan.TypeEnum qualified as TEnum
import Gpu.Vulkan.Buffer qualified as Buffer
import Gpu.Vulkan.BufferView.Middle qualified as M

data CreateInfo n t snmobjs = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: M.CreateFlags,
	createInfoBuffer :: U3 Buffer.B snmobjs }

class OffsetRange t (objs :: [VObj.Object]) where
	offsetRange :: (Device.M.Size, Device.M.Size)

type family FormatOf t :: TEnum.Format
