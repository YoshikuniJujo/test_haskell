{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.RenderPass where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont

import Vulkan.RenderPass.Enum

import qualified Vulkan.Attachment as Attachment
import qualified Vulkan.Subpass as Subpass
import qualified Vulkan.RenderPass.Core as C

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoAttachments :: [Attachment.Description],
	createInfoSubpasses :: [Subpass.Description],
	createInfoDependencies :: [Subpass.Dependency] }
	deriving Show

createInfoToCore :: Pointable n => CreateInfo n -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoAttachments =
		(length &&& (Attachment.descriptionToCore <$>)) -> (ac, as),
	createInfoSubpasses = (length &&& id) -> (sc, ss),
	createInfoDependencies =
		(length &&& (Subpass.dependencyToCore <$>)) -> (dc, ds) } = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	pas <- ContT $ allocaArray ac
	lift $ pokeArray pas as
	css <- Subpass.descriptionToCore `mapM` ss
	pss <- ContT $ allocaArray sc
	lift $ pokeArray pss css
	pds <- ContT $ allocaArray dc
	lift $ pokeArray pds ds
	let	C.CreateInfo_ fCreateInfo = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt,
			C.createInfoFlags = flgs,
			C.createInfoAttachmentCount = fromIntegral ac,
			C.createInfoPAttachments = pas,
			C.createInfoSubpassCount = fromIntegral sc,
			C.createInfoPSubpasses = pss,
			C.createInfoDependencyCount = fromIntegral dc,
			C.createInfoPDependencies = pds }
	ContT $ withForeignPtr fCreateInfo
