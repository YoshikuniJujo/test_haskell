{-# LANGUAGE BlockArguments #-}
{-# LANGUaGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CommandBuffer.Middle.Internal (
	C(..),
	AllocateInfo(..), allocate, freeCs,

	BeginInfo(..), InheritanceInfo(..), begin, end, reset
	) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.Default
import Data.IORef
import Data.Word

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.CommandBuffer.Enum
import Gpu.Vulkan.Misc hiding (NullPtr)

import {-# SOURCE #-} qualified Gpu.Vulkan.Device.Middle.Internal as Device
import qualified Gpu.Vulkan.RenderPass.Middle.Internal as RenderPass
import qualified Gpu.Vulkan.Framebuffer.Middle.Internal as Framebuffer
import qualified Gpu.Vulkan.CommandPool.Middle.Internal as CommandPool
import qualified Gpu.Vulkan.CommandBuffer.Core as C
import qualified Gpu.Vulkan.Pipeline.Core as Pipeline.C

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

data C = C {
	cPipeline :: IORef Pipeline.C.P,
	unC :: C.C }

newC :: C.C -> IO C
newC c = C <$> newIORef nullPtr <*> pure c

allocate :: Pointable n => Device.D -> AllocateInfo n -> IO [C]
allocate (Device.D dvc) ai = ($ pure) . runContT $ lift . mapM newC =<< do
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

inheritanceInfoToCore :: Pointable n =>
	InheritanceInfo n -> ContT r IO (Ptr C.InheritanceInfo)
inheritanceInfoToCore InheritanceInfo {
	inheritanceInfoNext = mnxt,
	inheritanceInfoRenderPass = RenderPass.R rp,
	inheritanceInfoSubpass = sp,
	inheritanceInfoFramebuffer = fb,
	inheritanceInfoOcclusionQueryEnable = oqe,
	inheritanceInfoQueryFlags = QueryControlFlagBits qf,
	inheritanceInfoPipelineStatistics = QueryPipelineStatisticFlagBits ps
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	fb' <- lift $ Framebuffer.fToCore fb
	let	C.InheritanceInfo_ fInheritanceInfo =  C.InheritanceInfo {
			C.inheritanceInfoSType = (),
			C.inheritanceInfoPNext = pnxt,
			C.inheritanceInfoRenderPass = rp,
			C.inheritanceInfoSubpass = sp,
			C.inheritanceInfoFramebuffer = fb',
			C.inheritanceInfoOcclusionQueryEnable = boolToBool32 oqe,
			C.inheritanceInfoQueryFlags = qf,
			C.inheritanceInfoPipelineStatistics = ps }
	ContT $ withForeignPtr fInheritanceInfo

begin :: (Pointable n, Pointable n') => C -> BeginInfo n n' -> IO ()
begin (C _ c) bi = ($ pure) $ runContT do
	pbi <- beginInfoToCore bi
	lift do	r <- C.begin c pbi
		throwUnlessSuccess $ Result r

end :: C -> IO ()
end (C rppl c) = throwUnlessSuccess . Result =<< do
	writeIORef rppl nullPtr
	C.end c

reset :: C -> ResetFlags -> IO ()
reset (C _ c) (ResetFlagBits fs) = throwUnlessSuccess . Result =<< C.reset c fs

freeCs :: Device.D -> CommandPool.C -> [C] -> IO ()
freeCs (Device.D dvc) (CommandPool.C cp)
	(length &&& ((\(C _ cb) -> cb) <$>) -> (cc, cs)) = ($ pure) $ runContT do
	pcs <- ContT $ allocaArray cc
	lift do	pokeArray pcs cs
		C.freeCs dvc cp (fromIntegral cc) pcs
