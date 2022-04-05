{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.CommandBuffer where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Pointable
import Control.Monad.Cont
import Data.Word

import Vulkan.Enum
import Vulkan.Base
import Vulkan.Exception
import Vulkan.Exception.Enum
import Vulkan.CommandBuffer.Enum

import qualified Vulkan.Device as Device
import qualified Vulkan.RenderPass as RenderPass
import qualified Vulkan.Framebuffer as Framebuffer
import qualified Vulkan.CommandPool as CommandPool
import qualified Vulkan.CommandBuffer.Core as C

data AllocateInfo n = AllocateInfo {
	allocateInfoNext :: Maybe n,
	allocateInfoCommandPool :: CommandPool.C,
	allocateInfoLevel :: Level,
	allocateInfoCommandBufferCount :: Word32 }
	deriving Show

allocateInfoToCore :: Pointable n =>
	AllocateInfo n -> ContT r IO (Ptr C.AllocateInfo)
allocateInfoToCore AllocateInfo {
	allocateInfoNext = mnxt,
	allocateInfoCommandPool = CommandPool.C cp,
	allocateInfoLevel = Level lvl,
	allocateInfoCommandBufferCount = cbc
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	let	C.AllocateInfo_ fAllocateInfo = C.AllocateInfo {
			C.allocateInfoSType = (),
			C.allocateInfoPNext = pnxt,
			C.allocateInfoCommandPool = cp,
			C.allocateInfoLevel = lvl,
			C.allocateInfoCommandBufferCount = cbc }
	ContT $ withForeignPtr fAllocateInfo

newtype C = C C.C deriving Show

allocate :: Pointable n => Device.D -> AllocateInfo n -> IO [C]
allocate (Device.D dvc) ai = ($ pure) . runContT $ (C <$>) <$> do
	pai <- allocateInfoToCore ai
	pc <- ContT $ allocaArray cbc
	lift do	r <- C.allocate dvc pai pc
		throwUnlessSuccess $ Result r
		peekArray cbc pc
	where cbc = fromIntegral $ allocateInfoCommandBufferCount ai

data BeginInfo n n' = BeginInfo {
	beginInfoNext :: Maybe n,
	beginInfoFlags :: UsageFlags,
	beginInfoInheritanceInfo :: Maybe (InheritanceInfo n') }
	deriving Show

data InheritanceInfo n = InheritanceInfo {
	inheritanceInfoNext :: Maybe n,
	inheritanceInfoRenderPass :: RenderPass.R,
	inheritanceInfoSubpass :: Word32,
	inheritanceInfoFramebuffer :: Framebuffer.F,
	inheritanceInfoOcclusionQueryEnable :: Bool,
	inheritanceInfoQueryFlags :: QueryControlFlags,
	inheritanceInfoPipelineStatistics :: QueryPipelineStatisticFlags }
	deriving Show

inheritanceInfoToCore :: Pointable n =>
	InheritanceInfo n -> ContT r IO C.InheritanceInfo
inheritanceInfoToCore InheritanceInfo {
	inheritanceInfoNext = mnxt,
	inheritanceInfoRenderPass = RenderPass.R rp,
	inheritanceInfoSubpass = sp,
	inheritanceInfoFramebuffer = Framebuffer.F fb,
	inheritanceInfoOcclusionQueryEnable = oqe,
	inheritanceInfoQueryFlags = QueryControlFlagBits qf,
	inheritanceInfoPipelineStatistics = QueryPipelineStatisticFlagBits ps
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	pure C.InheritanceInfo {
		C.inheritanceInfoSType = (),
		C.inheritanceInfoPNext = pnxt,
		C.inheritanceInfoRenderPass = rp,
		C.inheritanceInfoSubpass = sp,
		C.inheritanceInfoFramebuffer = fb,
		C.inheritanceInfoOcclusionQueryEnable = boolToBool32 oqe,
		C.inheritanceInfoQueryFlags = qf,
		C.inheritanceInfoPipelineStatistics = ps }
