{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Instance.Middle.Internal (
	I(..), CreateInfo(..), create, destroy,

	enumerateLayerProperties, enumerateExtensionProperties ) where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.Storable.PeekPoke (
	WithPoked, withPoked, withPokedMaybe', withPtrS, pattern NullPtr )
import Control.Arrow
import Control.Monad.Cont
import Data.Default

import qualified Data.Text as T

import Gpu.Vulkan.Middle.Internal
import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Instance.Enum
import Gpu.Vulkan.Misc.Middle.Internal

import qualified Gpu.Vulkan.Instance.Core as C
import Gpu.Vulkan.AllocationCallbacks.Middle.Internal
	qualified as AllocationCallbacks

data CreateInfo n a = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoApplicationInfo :: Maybe (ApplicationInfo a),
	createInfoEnabledLayerNames :: [T.Text],
	createInfoEnabledExtensionNames :: [T.Text] }
	deriving Show

instance Default (CreateInfo n a) where
	def = CreateInfo {
		createInfoNext = Nothing,
		createInfoFlags = CreateFlagsZero,
		createInfoApplicationInfo = Nothing,
		createInfoEnabledLayerNames = [],
		createInfoEnabledExtensionNames = [] }

createInfoToCore :: (WithPoked n, WithPoked n') =>
	CreateInfo n n' -> (Ptr C.CreateInfo -> IO a) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = (\(CreateFlags f) -> f) -> flgs,
	createInfoApplicationInfo = mai,
	createInfoEnabledLayerNames =
		(fromIntegral . length &&& id) -> (elnc, elns),
	createInfoEnabledExtensionNames =
		(fromIntegral . length &&& id) -> (eenc, eens) } f =
	withPokedMaybe' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	textListToCStringArray elns \pelna ->
	textListToCStringArray eens \peena ->
	let	ci pai = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt',
			C.createInfoFlags = flgs,
			C.createInfoPApplicationInfo = pai,
			C.createInfoEnabledLayerCount = elnc,
			C.createInfoPpEnabledLayerNames = pelna,
			C.createInfoEnabledExtensionCount = eenc,
			C.createInfoPpEnabledExtensionNames = peena } in
	case mai of
		Nothing -> () <$ withPoked (ci NullPtr) f
		Just ai -> applicationInfoToCore ai \pai ->
			withPoked (ci pai) f

newtype I = I C.I deriving Show

create :: (WithPoked n, WithPoked a, WithPoked c) =>
	CreateInfo n a -> Maybe (AllocationCallbacks.A c) -> IO I
create ci mac = (I <$>) . ($ pure) $ runContT $ ContT \f ->
	alloca \pist -> do
		createInfoToCore ci \pcci ->
			AllocationCallbacks.maybeToCore mac \pac -> do
				r <- C.create pcci pac pist
				throwUnlessSuccess $ Result r
		f =<< peek pist

destroy :: WithPoked d => I -> Maybe (AllocationCallbacks.A d) -> IO ()
destroy (I cist) mac = AllocationCallbacks.maybeToCore mac $ C.destroy cist

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
			Just ln -> ContT $ textToCString ln
		pExtCount <- ContT alloca
		(fromIntegral -> extCount) <- lift do
			r <- C.enumerateExtensionProperties cln pExtCount NullPtr
			throwUnlessSuccess $ Result r
			peek pExtCount
		pExts <- ContT $ allocaArray extCount
		lift do	r <- C.enumerateExtensionProperties cln pExtCount pExts
			throwUnlessSuccess $ Result r
			peekArray extCount pExts
