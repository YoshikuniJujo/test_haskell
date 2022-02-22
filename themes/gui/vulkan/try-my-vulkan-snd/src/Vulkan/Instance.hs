{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Instance where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Control.Arrow
import Control.Monad.Cont

import qualified Data.Text as T

import Vulkan
import Vulkan.Base
import Vulkan.Exception
import Vulkan.Exception.Enum
import Vulkan.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Instance.Enum

import qualified Vulkan.Instance.Core as C
import qualified Vulkan.AllocationCallbacks as AllocationCallbacks

data CreateInfo n n' = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoApplicationInfo :: ApplicationInfo n',
	createInfoEnabledLayerNames :: [T.Text],
	createInfoEnabledExtensionNames :: [T.Text] }
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
	pelna <- textListToCStringArray elns
	peena <- textListToCStringArray eens
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

create :: (Pointable n, Pointable n2, Pointable n3) =>
	CreateInfo n n2 -> Maybe (AllocationCallbacks n3) -> IO Instance
create ci mac = (Instance <$>) . ($ pure) $ runContT do
	pcci <- createInfoToCore ci
	pac <- AllocationCallbacks.maybeToCore mac
	pist <- ContT alloca
	lift do r <- C.create pcci pac pist
		throwUnlessSuccess $ Result r
		peek pist

destroy :: (Pointable n) => Instance -> Maybe (AllocationCallbacks n) -> IO ()
destroy (Instance cist) mac = ($ pure) $ runContT do
	pac <- AllocationCallbacks.maybeToCore mac
	lift $ C.destroy cist pac
