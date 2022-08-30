{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Swapchain (
	createNew, recreateNew, M.CreateInfoNew(..),
	create, recreate, S, M.CreateInfo(..), getImages) where

import Foreign.Pointable
import Control.Exception

import Gpu.Vulkan.Khr.Swapchain.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Image.Type as Image
import qualified Gpu.Vulkan.Khr.Swapchain.Middle as M

createNew (Device.D dvc) ci macc macd f =
	bracket (M.createNew dvc ci macc) (\sc -> M.destroy dvc sc macd) (f . SNew)

create :: (Pointable n, Pointable c, Pointable d) =>
	Device.D sd -> M.CreateInfo n ssfc ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall ssc . S ssc -> IO a) -> IO a
create (Device.D dvc) ci macc macd f =
	bracket (M.create dvc ci macc) (\sc -> M.destroy dvc sc macd) (f . S)

recreateNew (Device.D dvc) ci macc macd (SNew sc) = M.recreateNew dvc ci macc macd sc

recreate :: (Pointable n, Pointable c, Pointable d) =>
	Device.D sd -> M.CreateInfo n ssfc ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	S ssc -> IO ()
recreate (Device.D dvc) ci macc macd (S sc) = M.recreate dvc ci macc macd sc

getImages :: Device.D sd -> S ss -> IO [Image.Binded ss ss]
getImages (Device.D dvc) (S sc) = (Image.Binded <$>) <$> M.getImages dvc sc
