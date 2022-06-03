{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.CommandBuffer.Middle where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.Default
import Data.Word

import Vulkan.Enum
import Vulkan.Base
import Vulkan.Exception
import Vulkan.Exception.Enum
import Vulkan.CommandBuffer.Enum

import qualified Vulkan.Device.Middle as Device
import qualified Vulkan.RenderPass.Middle as RenderPass
import qualified Vulkan.Framebuffer as Framebuffer
import qualified Vulkan.CommandPool.Middle as CommandPool
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

newtype C vs = C { unC :: C.C } deriving Show

allocate :: Pointable n => Device.D -> AllocateInfo n -> IO [C vs]
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

beginInfoNil :: BeginInfo () ()
beginInfoNil = def

instance Default (BeginInfo n n') where
	def = BeginInfo {
		beginInfoNext = Nothing,
		beginInfoFlags = UsageFlagsZero,
		beginInfoInheritanceInfo = Nothing }

beginInfoToCore :: (Pointable n, Pointable n') =>
	BeginInfo n n' -> ContT r IO (Ptr C.BeginInfo)
beginInfoToCore BeginInfo {
	beginInfoNext = mnxt,
	beginInfoFlags = UsageFlagBits flgs,
	beginInfoInheritanceInfo = mii
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	pii <- maybe (pure NullPtr) inheritanceInfoToCore mii
	let	C.BeginInfo_ fBeginInfo = C.BeginInfo {
			C.beginInfoSType = (),
			C.beginInfoPNext = pnxt,
			C.beginInfoFlags = flgs,
			C.beginInfoPInheritanceInfo = pii }
	ContT $ withForeignPtr fBeginInfo

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
	InheritanceInfo n -> ContT r IO (Ptr C.InheritanceInfo)
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
	let	C.InheritanceInfo_ fInheritanceInfo =  C.InheritanceInfo {
			C.inheritanceInfoSType = (),
			C.inheritanceInfoPNext = pnxt,
			C.inheritanceInfoRenderPass = rp,
			C.inheritanceInfoSubpass = sp,
			C.inheritanceInfoFramebuffer = fb,
			C.inheritanceInfoOcclusionQueryEnable = boolToBool32 oqe,
			C.inheritanceInfoQueryFlags = qf,
			C.inheritanceInfoPipelineStatistics = ps }
	ContT $ withForeignPtr fInheritanceInfo

begin :: (Pointable n, Pointable n') => C vs -> BeginInfo n n' -> IO ()
begin (C c) bi = ($ pure) $ runContT do
	pbi <- beginInfoToCore bi
	lift do	r <- C.begin c pbi
		throwUnlessSuccess $ Result r

end :: C vs -> IO ()
end (C c) = throwUnlessSuccess . Result =<< C.end c

reset :: C vs -> ResetFlags -> IO ()
reset (C c) (ResetFlagBits fs) = throwUnlessSuccess . Result =<< C.reset c fs

freeCs :: Device.D -> CommandPool.C -> [C vs] -> IO ()
freeCs (Device.D dvc) (CommandPool.C cp)
	(length &&& ((\(C cb) -> cb) <$>) -> (cc, cs)) = ($ pure) $ runContT do
	pcs <- ContT $ allocaArray cc
	lift do	pokeArray pcs cs
		C.freeCs dvc cp (fromIntegral cc) pcs
