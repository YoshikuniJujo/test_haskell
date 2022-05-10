{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.PhysicalDevice where

import Foreign.Marshal
import Foreign.Storable
import Foreign.Pointable
import Control.Monad.Cont
import Data.Maybe
import Data.Word
import Data.UUID
import System.IO.Unsafe

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

import Vulkan
import Vulkan.Base
import Vulkan.Exception
import Vulkan.Exception.Enum
import Vulkan.PhysicalDevice.Enum
import Vulkan.PhysicalDevice.Struct

import qualified Vulkan.Instance as Instance
import qualified Vulkan.PhysicalDevice.Core as C
import qualified Vulkan.QueueFamily as QueueFamily
import qualified Vulkan.Memory.Middle as Memory.M

newtype P = P C.P deriving Show

enumerate :: Instance.I -> IO [P]
enumerate (Instance.I ist) = ($ pure) . runContT $ map P <$> do
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

getProperties :: P -> IO Properties
getProperties (P pdvc) =
	($ pure) . runContT $ propertiesFromCore <$> do
		pppts <- ContT alloca
		lift do	C.getProperties pdvc pppts
			peek pppts

getFeatures :: P -> IO Features
getFeatures (P pdvc) =
	($ pure) . runContT $ featuresFromCore <$> do
		pfts <- ContT alloca
		lift do	C.getFeatures pdvc pfts
			peek pfts

featuresZero :: Features
featuresZero = unsafePerformIO $ featuresFromCore <$> C.getClearedFeatures

getQueueFamilyProperties :: P -> IO [QueueFamily.Properties]
getQueueFamilyProperties (P pdvc) =
	($ pure) . runContT $ map QueueFamily.propertiesFromCore <$> do
		ppptc <- ContT alloca
		(fromIntegral -> pptc) <- lift do
			C.getQueueFamilyProperties pdvc ppptc NullPtr
			peek ppptc
		pppts <- ContT $ allocaArray pptc
		lift do	C.getQueueFamilyProperties pdvc ppptc pppts
			peekArray pptc pppts

enumerateExtensionProperties ::
	P -> Maybe T.Text -> IO [ExtensionProperties]
enumerateExtensionProperties (P pdvc) mlnm = ($ pure) $ runContT do
	pExtensionCount <- ContT alloca
	cmlnm <- case mlnm of
		Nothing -> pure NullPtr
		Just lnm -> textToCString lnm
	(fromIntegral -> extensionCount) <- lift do
		r <- C.enumerateExtensionProperties
			pdvc cmlnm pExtensionCount NullPtr
		throwUnlessSuccess $ Result r
		peek pExtensionCount
	pAvailableExtensions <- ContT $ allocaArray extensionCount
	map extensionPropertiesFromCore <$> lift do
		r <- C.enumerateExtensionProperties
			pdvc cmlnm pExtensionCount pAvailableExtensions
		throwUnlessSuccess $ Result r
		peekArray extensionCount pAvailableExtensions

data MemoryProperties = MemoryProperties {
	memoryPropertiesMemoryTypes :: [Memory.M.MType],
	memoryPropertiesMemoryHeaps :: [Memory.M.Heap] }
	deriving Show

memoryPropertiesFromCore :: C.MemoryProperties -> MemoryProperties
memoryPropertiesFromCore C.MemoryProperties {
	C.memoryPropertiesMemoryTypeCount = fromIntegral -> mtc,
	C.memoryPropertiesMemoryTypes = (Memory.M.mTypeFromCore <$>) -> mts,
	C.memoryPropertiesMemoryHeapCount = fromIntegral -> mhc,
	C.memoryPropertiesMemoryHeaps = (Memory.M.heapFromCore <$>) -> mhs } =
	MemoryProperties {
		memoryPropertiesMemoryTypes = take mtc mts,
		memoryPropertiesMemoryHeaps = take mhc mhs }

getMemoryProperties :: P -> IO MemoryProperties
getMemoryProperties (P p) =
	(memoryPropertiesFromCore <$>) . ($ pure) $ runContT do
		pmps <- ContT alloca
		lift do	C.getMemoryProperties p pmps
			peek pmps
