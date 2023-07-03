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

newtype L s (bts :: [BindingType]) = L { unL :: M.L } deriving Show

data BindingType
	= Image [(Symbol, T.Format)] | ImageSampler [(T.Format, Type)]
	| Buffer [VObj.Object] | BufferView [(Symbol, Type)]

type family BindingTypeBufferObjects bt where
	BindingTypeBufferObjects (Buffer os) = os
	BindingTypeBufferObjects _ = '[]

type family BindingTypeBufferOnlyDynamics bt where
	BindingTypeBufferOnlyDynamics bt =
		VObj.OnlyDynamics (BindingTypeBufferObjects bt)

type family BindingTypeListBufferOnlyDynamics bts where
	BindingTypeListBufferOnlyDynamics '[] = '[]
	BindingTypeListBufferOnlyDynamics (bt ': bts) =
		BindingTypeBufferOnlyDynamics bt ':
		BindingTypeListBufferOnlyDynamics bts

type family MapSnd (tpls :: [(t, u)]) where
	MapSnd '[] = '[]
	MapSnd ('(_, s) ': tpls) = s ': MapSnd tpls
