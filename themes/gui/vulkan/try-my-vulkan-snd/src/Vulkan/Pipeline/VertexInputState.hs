{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.VertexInputState where

import Data.Kind

import Vulkan.Pipeline.VertexInputState.BindingStrideList

import qualified Vulkan.Pipeline.VertexInputState.Middle as M
import qualified Vulkan.VertexInput as VertexInput

data CreateInfo n vs (ts :: [Type]) = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: M.CreateFlags }
	deriving Show

createInfoToBindingDescriptions :: forall n vs ts .
	BindingStrideList vs
		VertexInput.Rate VertexInput.Rate =>
	CreateInfo n vs ts -> [VertexInput.BindingDescription]
createInfoToBindingDescriptions _ = VertexInput.bindingDescriptionFromRaw
	$ bindingStrideList @vs @VertexInput.Rate @VertexInput.Rate
