{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImageView where

import Foreign.Pointable
import Control.Exception

import Gpu.Vulkan.Enum
import Gpu.Vulkan.ImageView.Enum

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Component as Component
import qualified Gpu.Vulkan.Image.Type as Image
import qualified Gpu.Vulkan.Image.Middle as Image.M
import qualified Gpu.Vulkan.ImageView.Middle as M

newtype I s = I M.I deriving Show

data CreateInfo si sm n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoImage :: Image.Binded si sm,
	createInfoViewType :: Type,
	createInfoFormat :: Format,
	createInfoComponents :: Component.Mapping,
	createInfoSubresourceRange :: Image.M.SubresourceRange }
	deriving Show

createInfoToMiddle :: CreateInfo si sm n -> M.CreateInfo n
createInfoToMiddle CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoImage = Image.Binded img,
	createInfoViewType = tp,
	createInfoFormat = fmt,
	createInfoComponents = cps,
	createInfoSubresourceRange = srr
	} = M.CreateInfo {
		M.createInfoNext = mnxt,
		M.createInfoFlags = flgs,
		M.createInfoImage = img,
		M.createInfoViewType = tp,
		M.createInfoFormat = fmt,
		M.createInfoComponents = cps,
		M.createInfoSubresourceRange = srr }

create :: (Pointable n, Pointable c, Pointable d) =>
	Device.D sd -> CreateInfo si sm n ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall s . I s -> IO a) -> IO a
create (Device.D dvc) ci macc macd f = bracket
	(M.create dvc (createInfoToMiddle ci) macc)
	(\i -> M.destroy dvc i macd) (f . I)
