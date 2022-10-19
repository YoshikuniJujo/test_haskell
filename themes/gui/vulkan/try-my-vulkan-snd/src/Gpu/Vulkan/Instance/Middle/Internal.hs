{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Instance.Middle.Internal (
	I(..), CreateInfo(..), create, destroy,

	enumerateLayerProperties, enumerateExtensionProperties ) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.Default

import qualified Data.Text as T

import Gpu.Vulkan.Middle
import Gpu.Vulkan.Base
import Gpu.Vulkan.Exception
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Instance.Enum

import qualified Gpu.Vulkan.Instance.Core as C
import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks

data CreateInfo n n' = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoApplicationInfo :: Maybe (ApplicationInfo n'),
	createInfoEnabledLayerNames :: [T.Text],
	createInfoEnabledExtensionNames :: [T.Text] }
	deriving Show

instance Default (CreateInfo n n') where
	def = CreateInfo {
		createInfoNext = Nothing,
		createInfoFlags = CreateFlagsZero,
		createInfoApplicationInfo = Nothing,
		createInfoEnabledLayerNames = [],
		createInfoEnabledExtensionNames = [] }

createInfoToCore :: (Pointable n, Pointable n') =>
	CreateInfo n n' -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = (\(CreateFlags f) -> f) -> flgs,
	createInfoApplicationInfo = mai,
	createInfoEnabledLayerNames =
		(fromIntegral . length &&& id) -> (elnc, elns),
	createInfoEnabledExtensionNames =
		(fromIntegral . length &&& id) -> (eenc, eens) } = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	pai <- maybe (pure NullPtr) applicationInfoToCore mai
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

newtype I = I C.I deriving Show

create :: (Pointable n, Pointable n2, Pointable n3) =>
	CreateInfo n n2 -> Maybe (AllocationCallbacks.A n3) -> IO I
create ci mac = (I <$>) . ($ pure) $ runContT do
	pcci <- createInfoToCore ci
	pac <- AllocationCallbacks.maybeToCore mac
	pist <- ContT alloca
	lift do r <- C.create pcci pac pist
		throwUnlessSuccess $ Result r
		peek pist

destroy :: (Pointable n) => I -> Maybe (AllocationCallbacks.A n) -> IO ()
destroy (I cist) mac = ($ pure) $ runContT do
	pac <- AllocationCallbacks.maybeToCore mac
	lift $ C.destroy cist pac

enumerateLayerProperties :: IO [LayerProperties]
enumerateLayerProperties = ($ pure) . runContT
	$ map layerPropertiesFromCore <$> do
		pLayerCount <- ContT alloca
		(fromIntegral -> layerCount) <- lift do
			r <- C.enumerateLayerProperties pLayerCount NullPtr
			throwUnlessSuccess $ Result r
			peek pLayerCount
		pLayerProps <- ContT $ allocaArray layerCount
		lift do	r <- C.enumerateLayerProperties pLayerCount pLayerProps
			throwUnlessSuccess $ Result r
			peekArray layerCount pLayerProps

enumerateExtensionProperties :: Maybe T.Text -> IO [ExtensionProperties]
enumerateExtensionProperties mln = ($ pure) . runContT
	$ map extensionPropertiesFromCore <$> do
		cln <- case mln of
			Nothing -> pure NullPtr
			Just ln -> textToCString ln
		pExtCount <- ContT alloca
		(fromIntegral -> extCount) <- lift do
			r <- C.enumerateExtensionProperties cln pExtCount NullPtr
			throwUnlessSuccess $ Result r
			peek pExtCount
		pExts <- ContT $ allocaArray extCount
		lift do	r <- C.enumerateExtensionProperties cln pExtCount pExts
			throwUnlessSuccess $ Result r
			peekArray extCount pExts
