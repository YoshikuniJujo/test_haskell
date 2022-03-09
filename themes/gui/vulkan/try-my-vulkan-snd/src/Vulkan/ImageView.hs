{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.ImageView where

import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad.Cont

import Vulkan
import Vulkan.Enum
import Vulkan.Base
import Vulkan.Image
import Vulkan.Component
import Vulkan.ImageView.Enum

import qualified Vulkan.ImageView.Core as C

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoImage :: Image,
	createInfoViewType :: Type,
	createInfoFormat :: Format,
	createInfoComponents :: Mapping,
	createInfoSubresourceRange :: SubresourceRange }
	deriving Show

createInfoToC :: Pointable n => CreateInfo n -> ContT r IO (Ptr C.CreateInfo)
createInfoToC CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoImage = Image img,
	createInfoViewType = Type tp,
	createInfoFormat = Format fmt,
	createInfoComponents = cpns,
	createInfoSubresourceRange = srr
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	let C.CreateInfo_ fCreateInfo = C.CreateInfo {
		C.createInfoSType = (),
		C.createInfoPNext = pnxt,
		C.createInfoFlags = flgs,
		C.createInfoImage = img,
		C.createInfoViewType = tp,
		C.createInfoFormat = fmt,
		C.createInfoComponents = mappingToCore cpns,
		C.createInfoSubresourceRange = subresourceRangeToCore srr }
	ContT $ withForeignPtr fCreateInfo
