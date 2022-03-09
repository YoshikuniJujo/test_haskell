{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Control.Monad.Cont
import Data.Word

import qualified Data.Text as T

import Vulkan.Base

import qualified Vulkan.Core as C
import qualified Vulkan.Instance.Core as Instance.C
import qualified Vulkan.PhysicalDevice.Core as PhysicalDevice.C
import qualified Vulkan.Device.Core as Device.C
import qualified Vulkan.Image.Core as Image.C

#include <vulkan/vulkan.h>

newtype Instance = Instance Instance.C.Instance deriving Show

newtype PhysicalDevice = PhysicalDevice PhysicalDevice.C.PhysicalDevice deriving Show

newtype Device = Device Device.C.Device deriving Show

newtype Queue = Queue C.Queue deriving Show

newtype Image = Image Image.C.Image deriving Show

data ApplicationInfo a = ApplicationInfo {
	applicationInfoNext :: Maybe a,
	applicationInfoApplicationName :: T.Text,
	applicationInfoApplicationVersion :: ApiVersion,
	applicationInfoEngineName :: T.Text,
	applicationInfoEngineVersion :: ApiVersion,
	applicationInfoApiVersion :: ApiVersion }
	deriving Show

newtype ApiVersion = ApiVersion C.ApiVersion deriving (Show, Eq, Ord, Storable)

apiVersion_1_0 :: ApiVersion
apiVersion_1_0 = ApiVersion C.apiVersion_1_0

type Variant = Word8	-- 0 <= variant < 8
type Major = Word8	-- 0 <= major < 127
type Minor = Word16	-- 0 <= minor < 1023
type Patch = Word16	-- 0 <= patch < 4095

makeApiVersion :: Variant -> Major -> Minor -> Patch -> ApiVersion
makeApiVersion v mj mn p = ApiVersion $ C.makeApiVersion v mj mn p

applicationInfoToCore :: Pointable a =>
	ApplicationInfo a -> ContT r IO (Ptr C.ApplicationInfo)
applicationInfoToCore ApplicationInfo {
	applicationInfoNext = mnxt,
	applicationInfoApplicationName = anm,
	applicationInfoApplicationVersion = (\(ApiVersion v) -> v) -> appv,
	applicationInfoEngineName = enm,
	applicationInfoEngineVersion = (\(ApiVersion v) -> v) -> engv,
	applicationInfoApiVersion = (\(ApiVersion v) -> v) -> apiv
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	canm <- textToCString anm
	cenm <- textToCString enm
	let	C.ApplicationInfo_ fApplicationInfo = C.ApplicationInfo {
			C.applicationInfoSType = (),
			C.applicationInfoPNext = pnxt,
			C.applicationInfoPApplicationName = canm,
			C.applicationInfoApplicationVersion = appv,
			C.applicationInfoPEngineName = cenm,
			C.applicationInfoEngineVersion = engv,
			C.applicationInfoApiVersion = apiv }
	ContT $ withForeignPtr fApplicationInfo

newtype ObjectHandle = ObjectHandle #{type uint64_t} deriving Show

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
