{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Shader where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Cont

import qualified Data.ByteString as BS

import Data.ByteString.AsUint32T
import Vulkan.Base
import Vulkan.Exception
import Vulkan.AllocationCallbacks
import Vulkan.Device

import qualified Vulkan.AllocationCallbacks.Internal as I
import qualified Vulkan.Shader.Internal as I

#include <vulkan/vulkan.h>

newtype ShaderModule = ShaderModule (Ptr ShaderModule) deriving (Show, Storable)

data ShaderModuleCreateInfo n = ShaderModuleCreateInfo {
	shaderModuleCreateInfoNext :: Maybe n,
	shaderModuleCreateInfoFlags :: I.ShaderModuleCreateFlags,
	shaderModuleCreateInfoCode :: BS.ByteString }
	deriving Show

shaderModuleCreateInfoToC :: Pointable n =>
	ShaderModuleCreateInfo n -> (I.ShaderModuleCreateInfo -> IO a) -> IO a
shaderModuleCreateInfoToC ShaderModuleCreateInfo {
	shaderModuleCreateInfoNext = mnxt,
	shaderModuleCreateInfoFlags = flgs,
	shaderModuleCreateInfoCode = cd } = runContT do
	(castPtr -> pnxt) <- ContT $ withPointerMaybe mnxt
	(pcd, sz) <- ContT $ useAsUint32TLen' cd
	pure $ I.ShaderModuleCreateInfo {
		I.shaderModuleCreateInfoSType = (),
		I.shaderModuleCreateInfoPNext = pnxt,
		I.shaderModuleCreateInfoFlags = flgs,
		I.shaderModuleCreateInfoCodeSize = sz,
		I.shaderModuleCreateInfoPCode = pcd }

createShaderModule :: (Pointable n, Pointable n') => Device ->
	ShaderModuleCreateInfo n -> Maybe (AllocationCallbacks n') ->
	IO ShaderModule
createShaderModule dvc smci mac = ($ pure) $ runContT do
	I.ShaderModuleCreateInfo_ fsmci <- ContT $ shaderModuleCreateInfoToC smci
	psmci <- ContT $ withForeignPtr fsmci
	pac <- case mac of
		Nothing -> pure NullPtr
		Just ac -> ContT $ withAllocationCallbacksPtr ac
	psm <- ContT alloca
	lift do	r <- c_vkCreateShaderModule dvc psmci pac psm
		throwUnlessSuccess r
		peek psm

foreign import ccall "vkCreateShaderModule" c_vkCreateShaderModule ::
	Device -> Ptr I.ShaderModuleCreateInfo ->
	Ptr I.AllocationCallbacks -> Ptr ShaderModule -> IO Result

destroyShaderModule :: Pointable n =>
	Device -> ShaderModule -> Maybe (AllocationCallbacks n) -> IO ()
destroyShaderModule dvc sm mac = ($ pure) $ runContT do
	pac <- case mac of
		Nothing -> pure NullPtr
		Just ac -> ContT $ withAllocationCallbacksPtr ac
	lift $ c_vkDestroyShaderModule dvc sm pac

foreign import ccall "vkDestroyShaderModule" c_vkDestroyShaderModule ::
	Device -> ShaderModule -> Ptr I.AllocationCallbacks -> IO ()
