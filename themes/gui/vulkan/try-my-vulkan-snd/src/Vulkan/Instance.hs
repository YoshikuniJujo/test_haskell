{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Instance where

import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Arrow
import Control.Monad.Cont

import Vulkan
import Vulkan.Base
import Vulkan.Instance.Enum

import qualified Vulkan.Instance.Core as C

data CreateInfo n n' = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoApplicationInfo :: ApplicationInfo n',
	createInfoEnabledLayerNames :: [String],
	createInfoEnabledExtensionNames :: [String] }
	deriving Show

createInfoToCore :: (Pointable n, Pointable n') =>
	CreateInfo n n' -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = (\(CreateFlags f) -> f) -> flgs,
	createInfoApplicationInfo = ai,
	createInfoEnabledLayerNames =
		(fromIntegral . length &&& id) -> (elnc, elns),
	createInfoEnabledExtensionNames =
		(fromIntegral . length &&& id) -> (eenc, eens) } = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	pai <- applicationInfoToCore ai
	pelna <- stringListToCStringArray elns
	peena <- stringListToCStringArray eens
	let	C.CreateInfo_ fCreateInfo = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt,
			C.createInfoFlags = flgs,
			C.createInfoPApplicationInfo = pai,
			C.createInfoEnabledLayerCount = elnc,
			C.createInfoPpEnabledLayerNames = pelna,
			C.createInfoEnabledExtensionCount = eenc,
			C.createInfoPpEnabledExtensionNames = peena }
	ContT $ withForeignPtr fCreateInfo
