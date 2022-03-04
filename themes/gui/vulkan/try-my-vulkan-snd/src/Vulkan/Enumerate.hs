{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Enumerate where

import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Cont

import qualified Data.Text as T

import Vulkan
import Vulkan.Base
import Vulkan.Exception
import Vulkan.Exception.Enum

import qualified Vulkan.Core as C
import qualified Vulkan.Enumerate.Core as C

data LayerProperties = LayerProperties {
	layerPropertiesLayerName :: T.Text,
	layerPropertiesSpecVersion :: ApiVersion,
	layerPropertiesImplementationVersion :: ApiVersion,
	layerPropertiesDescription :: T.Text }
	deriving Show

layerPropertiesFromCore :: C.LayerProperties -> LayerProperties
layerPropertiesFromCore C.LayerProperties {
	C.layerPropertiesLayerName = ln,
	C.layerPropertiesSpecVersion = sv,
	C.layerPropertiesImplementationVersion = iv,
	C.layerPropertiesDescription = dsc } = LayerProperties {
	layerPropertiesLayerName = ln,
	layerPropertiesSpecVersion = ApiVersion sv,
	layerPropertiesImplementationVersion = ApiVersion iv,
	layerPropertiesDescription = dsc }

instanceLayerProperties :: IO [LayerProperties]
instanceLayerProperties = ($ pure) . runContT
	$ map layerPropertiesFromCore <$> do
		pLayerCount <- ContT alloca
		(fromIntegral -> layerCount) <- lift do
			r <- C.instanceLayerProperties pLayerCount NullPtr
			throwUnlessSuccess $ Result r
			peek pLayerCount
		pLayerProps <- ContT $ allocaArray layerCount
		lift do	r <- C.instanceLayerProperties pLayerCount pLayerProps
			throwUnlessSuccess $ Result r
			peekArray layerCount pLayerProps

data ExtensionProperties = ExtensionProperties {
	extensionPropertiesExtensionName :: T.Text,
	extensionPropertiesSpecVersion :: ApiVersion }
	deriving Show

extensionPropertiesFromCore :: C.ExtensionProperties -> ExtensionProperties
extensionPropertiesFromCore C.ExtensionProperties {
	C.extensionPropertiesExtensionName = en,
	C.extensionPropertiesSpecVersion = sv } = ExtensionProperties {
		extensionPropertiesExtensionName = en,
		extensionPropertiesSpecVersion = ApiVersion sv }

instanceExtensionProperties :: Maybe T.Text -> IO [ExtensionProperties]
instanceExtensionProperties mln = ($ pure) . runContT
	$ map extensionPropertiesFromCore <$> do
		cln <- case mln of
			Nothing -> pure NullPtr
			Just ln -> textToCString ln
		pExtCount <- ContT alloca
		(fromIntegral -> extCount) <- lift do
			r <- C.instanceExtensionProperties cln pExtCount NullPtr
			throwUnlessSuccess $ Result r
			peek pExtCount
		pExts <- ContT $ allocaArray extCount
		lift do	r <- C.instanceExtensionProperties cln pExtCount pExts
			throwUnlessSuccess $ Result r
			peekArray extCount pExts
