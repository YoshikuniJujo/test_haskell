{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.VertexInputState where

import Data.Kind

import qualified Vulkan.Pipeline.VertexInputState.Middle as M

data CreateInfo n vs (ts :: [Type]) = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: M.CreateFlags }
	deriving Show

-- createInfoToBindingDescriptionRaw :: forall n vs ts .
--	BindingStrideList vs 
