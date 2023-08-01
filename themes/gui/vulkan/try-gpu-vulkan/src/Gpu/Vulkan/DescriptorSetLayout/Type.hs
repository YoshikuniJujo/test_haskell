{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSetLayout.Type where

import GHC.TypeLits
import Data.Kind
import Gpu.Vulkan.Object qualified as VObj

import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.DescriptorSetLayout.Middle as M

newtype D s (bts :: [BindingType]) = D { unL :: M.D } deriving Show

data BindingType
	= Image [(Symbol, T.Format)] | ImageSampler [(Symbol, T.Format, Type)]
	| Buffer [VObj.O] | BufferView [(Symbol, Type)]

type family BindingTypeListBufferOnlyDynamics bts where
	BindingTypeListBufferOnlyDynamics '[] = '[]
	BindingTypeListBufferOnlyDynamics (bt ': bts) =
		BindingTypeBufferOnlyDynamics bt ':
		BindingTypeListBufferOnlyDynamics bts

type family BindingTypeBufferOnlyDynamics bt where
	BindingTypeBufferOnlyDynamics bt =
		VObj.OnlyDynamics (BindingTypeBufferObjects bt)

type family BindingTypeBufferObjects bt where
	BindingTypeBufferObjects (Buffer os) = os
	BindingTypeBufferObjects _ = '[]
