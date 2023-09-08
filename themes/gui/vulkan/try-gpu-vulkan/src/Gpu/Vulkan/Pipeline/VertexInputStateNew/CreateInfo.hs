{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.VertexInputStateNew.CreateInfo where

import GHC.TypeNats
import Data.Kind
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.Default

import qualified Gpu.Vulkan.Pipeline.VertexInputState.Middle as M
import qualified Gpu.Vulkan.VertexInput.Internal as VtxInp

-- CREATE INFO

data CreateInfo mn (vibs :: [(Type, VtxInp.Rate)]) (vias :: [(Nat, Type)]) =
	CreateInfo {
		createInfoNext :: TMaybe.M mn,
		createInfoFlags :: M.CreateFlags }

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn vibs vias)

instance Default (CreateInfo 'Nothing vibs vias) where
	def = CreateInfo {
		createInfoNext = TMaybe.N, createInfoFlags = zeroBits }

-- CREATE INFO TO MIDDLE

createInfoToMiddle :: forall n vibs vias . (
	BindingStrideList vibs VtxInp.Rate,
	AttributeDescriptions vibs vias ) =>
	CreateInfo n vibs vias -> M.CreateInfo n
createInfoToMiddle CreateInfo {
	createInfoNext = mnxt, createInfoFlags = flgs } = M.CreateInfo {
	M.createInfoNext = mnxt,
	M.createInfoFlags = flgs,
	M.createInfoVertexBindingDescriptions = bindingDescriptions @vibs,
	M.createInfoVertexAttributeDescriptions =
		attributeDescriptions @vibs @vias }
