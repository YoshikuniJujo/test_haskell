{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.PhysicalDevice where

import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Cont
import Data.Maybe
import Data.Word
import Data.UUID

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

import Vulkan
import Vulkan.Base
import Vulkan.Exception
import Vulkan.Exception.Enum
import Vulkan.PhysicalDevice.Enum
import Vulkan.PhysicalDevice.Struct

import qualified Vulkan.PhysicalDevice.Core as C

enumerate :: Instance -> IO [PhysicalDevice]
enumerate (Instance ist) = ($ pure) . runContT $ map PhysicalDevice <$> do
	pdvcc <- ContT alloca
	(fromIntegral -> dvcc) <- lift do
		r <- C.enumerate ist pdvcc NullPtr
		throwUnlessSuccess $ Result r
		peek pdvcc
	pdvcs <- ContT $ allocaArray dvcc
	lift do	r <- C.enumerate ist pdvcc pdvcs
		throwUnlessSuccess $ Result r
		peekArray dvcc pdvcs

data Properties = Properties {
	propertiesApiVersion :: ApiVersion,
	propertiesDriverVersion :: Word32,
	propertiesVendorId :: Word32,
	propertiesDeviceId :: Word32,
	propertiesDeviceType :: Type,
	propertiesDeviceName :: T.Text,
	propertiesPipelineCacheUuid :: UUID,
	propertiesLimits :: Limits,
	propertiesSparseProperties :: SparseProperties }
	deriving Show

propertiesFromCore :: C.Properties -> Properties
propertiesFromCore C.Properties {
	C.propertiesApiVersion = av,
	C.propertiesDriverVersion = dv,
	C.propertiesVendorId = vi,
	C.propertiesDeviceId = di,
	C.propertiesDeviceType = dt,
	C.propertiesDeviceName = dn,
	C.propertiesPipelineCacheUuid = pcu,
	C.propertiesLimits = l,
	C.propertiesSparseProperties = sp } = Properties {
	propertiesApiVersion = ApiVersion av,
	propertiesDriverVersion = dv,
	propertiesVendorId = vi,
	propertiesDeviceId = di,
	propertiesDeviceType = Type dt,
	propertiesDeviceName = dn,
	propertiesPipelineCacheUuid = word8listToUuid pcu,
	propertiesLimits = limitsFromCore l,
	propertiesSparseProperties = sparsePropertiesFromCore sp }

word8listToUuid :: [Word8] -> UUID
word8listToUuid ws = fromJust . fromByteString $ LBS.pack ws

data SparseProperties = SparseProperties {
	sparsePropertiesResidencyStandard2DBlockShape :: Bool,
	sparsePropertiesResidencyStandard2DMultisampleBlockShape :: Bool,
	sparsePropertiesResidencyStandard3DBlockShape :: Bool,
	sparsePropertiesResidencyAlignedMipSize :: Bool,
	sparsePropertiesResidencyNonResidentStrict :: Bool }
	deriving Show

sparsePropertiesFromCore :: C.SparseProperties -> SparseProperties
sparsePropertiesFromCore C.SparseProperties {
	C.sparsePropertiesResidencyStandard2DBlockShape = crs2bs,
	C.sparsePropertiesResidencyStandard2DMultisampleBlockShape = crs2mbs,
	C.sparsePropertiesResidencyStandard3DBlockShape = crs3bs,
	C.sparsePropertiesResidencyAlignedMipSize = crams,
	C.sparsePropertiesResidencyNonResidentStrict = cnrs } =
	SparseProperties {
		sparsePropertiesResidencyStandard2DBlockShape = rs2bs,
		sparsePropertiesResidencyStandard2DMultisampleBlockShape =
			rs2mbs,
		sparsePropertiesResidencyStandard3DBlockShape = rs3bs,
		sparsePropertiesResidencyAlignedMipSize = rams,
		sparsePropertiesResidencyNonResidentStrict = nrs }
	where [rs2bs, rs2mbs, rs3bs, rams, nrs] = bool32ToBool <$>
		[crs2bs, crs2mbs, crs3bs, crams, cnrs]
