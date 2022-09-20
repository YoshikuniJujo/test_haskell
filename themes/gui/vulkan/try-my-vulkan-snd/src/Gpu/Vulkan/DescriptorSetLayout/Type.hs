{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSetLayout.Type where

import GHC.TypeLits
import Data.Kind
import Data.Kind.Object

import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.DescriptorSetLayout.Middle as M

newtype L'' s = L'' { unL'' :: M.L } deriving Show

newtype L s (bts :: [BindingType]) = L { unL :: M.L } deriving Show

data BindingType
	= Image [(Symbol, T.Format)] | ImageSampler [(T.Format, Type)]
	| Buffer [Object] | Other

type family MapSnd (tpls :: [(t, u)]) where
	MapSnd '[] = '[]
	MapSnd ('(_, s) ': tpls) = s ': MapSnd tpls
