{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.CommandBuffer where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Storable
import Control.Monad.Cont
import Data.Word

import Vulkan.Base
import Vulkan.Exception
import Vulkan.Device
import Vulkan.CommandPool
import Vulkan.CommandBufferLevel

import Vulkan.Framebuffer (Framebuffer)
import Vulkan.QueryControlFlagBits
import Vulkan.QueryPipelineStatisticFlagBits

import Vulkan.CommandBufferUsageFlagBits

import qualified Vulkan.CommandBuffer.Internal as I

data AllocateInfo n = AllocateInfo {
	allocateInfoNext :: Maybe n,
	allocateInfoCommandPool :: CommandPool,
	allocateInfoLevel :: CommandBufferLevel,
	allocateInfoCommandBufferCount :: Word32 }
	deriving Show

allocateInfoToC :: Pointable n => AllocateInfo n -> ContT r IO I.AllocateInfo
allocateInfoToC AllocateInfo {
	allocateInfoNext = mnxt,
	allocateInfoCommandPool = cp,
	allocateInfoLevel = lv,
	allocateInfoCommandBufferCount = word32ToUint32T -> cbc } = do
	(castPtr -> pnxt) <- ContT $ withMaybePointer mnxt
	pure I.AllocateInfo {
		I.allocateInfoSType = (),
		I.allocateInfoPNext = pnxt,
		I.allocateInfoCommandPool = cp,
		I.allocateInfoLevel = lv,
		I.allocateInfoCommandBufferCount = cbc }

data CommandBufferTag
newtype CommandBuffer = CommandBuffer (Ptr CommandBufferTag)
	deriving (Show, Storable)

allocate :: Storable n => Device -> AllocateInfo n -> IO [CommandBuffer]
allocate dvc ai = ($ pure) $ runContT do
	I.AllocateInfo_ fai <- allocateInfoToC ai
	pai <- ContT $ withForeignPtr fai
	pcbs <- ContT $ allocaArray n
	lift do	r <- c_vkAllocateCommandBuffers dvc pai pcbs
		throwUnlessSuccess r
		peekArray n pcbs
	where n = fromIntegral $ allocateInfoCommandBufferCount ai

foreign import ccall "vkAllocateCommandBuffers" c_vkAllocateCommandBuffers ::
	Device -> Ptr I.AllocateInfo -> Ptr CommandBuffer -> IO Result

data InheritanceInfo n = InheritanceInfo {
	inheritanceInfoNext :: Maybe n,
	inheritanceInfoRenderPass :: RenderPass,
	inheritanceInfoSubpass :: Word32,
	inheritanceInfoFramebuffer :: Framebuffer,
	inheritanceInfoOcclusionQueryEnable :: Bool,
	inheritanceInfoQueryFlags :: QueryControlFlags,
	inheritanceInfoPipelineStatistics :: QueryPipelineStatisticFlags }
	deriving Show

inheritanceInfoToC :: Pointable n =>
	InheritanceInfo n -> ContT r IO I.InheritanceInfo
inheritanceInfoToC InheritanceInfo {
	inheritanceInfoNext = mnxt,
	inheritanceInfoRenderPass = rp,
	inheritanceInfoSubpass = word32ToUint32T -> sp,
	inheritanceInfoFramebuffer = fb,
	inheritanceInfoOcclusionQueryEnable = boolToBool32 -> oqe,
	inheritanceInfoQueryFlags = qf,
	inheritanceInfoPipelineStatistics = ps } = do
	(castPtr -> pnxt) <- ContT $ withMaybePointer mnxt
	pure I.InheritanceInfo {
		I.inheritanceInfoSType = (),
		I.inheritanceInfoPNext = pnxt,
		I.inheritanceInfoRenderPass = rp,
		I.inheritanceInfoSubpass = sp,
		I.inheritanceInfoFramebuffer = fb,
		I.inheritanceInfoOcclusionQueryEnable = oqe,
		I.inheritanceInfoQueryFlags = qf,
		I.inheritanceInfoPipelineStatistics = ps }

data BeginInfo n n' = BeginInfo {
	beginInfoNext :: Maybe n,
	beginInfoFlags :: CommandBufferUsageFlags,
	beginInfoInheritanceInfo :: Maybe (InheritanceInfo n') }
	deriving Show

beginInfoToC ::
	(Pointable n, Pointable n') => BeginInfo n n' -> ContT r IO I.BeginInfo
beginInfoToC BeginInfo {
	beginInfoNext = mnxt,
	beginInfoFlags = flgs,
	beginInfoInheritanceInfo = mii } = do
	(castPtr -> pnxt) <- ContT $ withMaybePointer mnxt
	pii <- case mii of
		Nothing -> pure NullPtr
		Just ii -> do
			I.InheritanceInfo_ fii <- inheritanceInfoToC ii
			ContT $ withForeignPtr fii
	pure I.BeginInfo {
		I.beginInfoSType = (),
		I.beginInfoPNext = pnxt,
		I.beginInfoFlags = flgs,
		I.beginInfoPInheritanceInfo = pii }

begin :: (Pointable n, Pointable n') => CommandBuffer -> BeginInfo n n' -> IO ()
begin cb bi = ($ pure) $ runContT do
	I.BeginInfo_ fbi <- beginInfoToC bi
	pbi <- ContT $ withForeignPtr fbi
	lift do	r <- c_vkBeginCommandBuffer cb pbi
		throwUnlessSuccess r

foreign import ccall "vkBeginCommandBuffer" c_vkBeginCommandBuffer ::
	CommandBuffer -> Ptr I.BeginInfo -> IO Result
