{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.Memory.Buffer.TypeLevel where

import Foreign.Storable
import Data.Kind.Object
import Data.HeteroList

import Gpu.Vulkan.Device.Middle
import Gpu.Vulkan.Device.Memory.Buffer.Types

class OffsetSize (obj :: Object) (objss :: [[Object]]) where
	offsetSize :: HeteroVarList Form objss -> (Size, Size)

instance Storable (ObjectType obj) =>
	OffsetSize obj ((obj ': objs) ': objss) where
	offsetSize (Form ost _ (ln :...: _) :...: _) =
		(ost, fromIntegral $ objectSize ln)
