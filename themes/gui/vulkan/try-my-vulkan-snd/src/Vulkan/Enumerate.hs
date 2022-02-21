{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Enumerate where

import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Cont

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Vulkan
import Vulkan.Base
import Vulkan.Exception
import Vulkan.Exception.Enum

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
	layerPropertiesLayerName = T.decodeUtf8 $ BSC.takeWhile (/= '\NUL') ln,
	layerPropertiesSpecVersion = ApiVersion sv,
	layerPropertiesImplementationVersion = ApiVersion iv,
	layerPropertiesDescription =
		T.decodeUtf8 $ BSC.takeWhile (/= '\NUL') dsc }

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
