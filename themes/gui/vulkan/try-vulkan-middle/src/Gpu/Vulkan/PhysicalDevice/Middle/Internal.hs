{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PhysicalDevice.Middle.Internal (

	-- * ENUMERATE, PROPERTIES AND FEATURES

	enumerate, P(..), getProperties, Properties(..), getFeatures,

	Features2Result(..), getFeatures2,

	-- * OTHER PROPERTIES

	getQueueFamilyProperties,
	enumerateExtensionProperties,
	getFormatProperties,
	getMemoryProperties, MemoryProperties(..),

	-- * OTHER FEATURES

	ShaderDrawParametersFeatures(..)

	) where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.Maybe
import Data.List.Length
import Data.Word
import Data.UUID

import qualified Data.ByteString.Lazy as LBS

import Data.Text qualified as T
import Data.Text.Foreign.Misc

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Middle.Internal
import Gpu.Vulkan.Base.Middle.Internal
import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.PhysicalDevice.Enum
import Gpu.Vulkan.PhysicalDevice.Struct

import qualified Gpu.Vulkan.Instance.Middle.Internal as Instance.M
import qualified Gpu.Vulkan.PhysicalDevice.Core as C
import qualified Gpu.Vulkan.QueueFamily.Middle.Internal as QueueFamily
import qualified Gpu.Vulkan.QueueFamily.EnumManual as QueueFamily
import qualified Gpu.Vulkan.Memory.Middle.Internal as Memory.M

import Data.HeteroParList qualified as HeteroParList
import Gpu.Vulkan.PNext.Middle.Internal

newtype P = P C.P deriving Show

enumerate :: Instance.M.I -> IO [P]
enumerate (Instance.M.I ist) = map P <$> alloca \pdvcc ->
	C.enumerate ist pdvcc NullPtr >>= \r ->
	throwUnlessSuccess (Result r) >>
	peek pdvcc >>= \(fromIntegral -> dvcc) ->
	allocaArray dvcc \pdvcs ->
	C.enumerate ist pdvcc pdvcs >>= \r' ->
	throwUnlessSuccess (Result r') >>
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
	where
	(rs2bs :. rs2mbs :. rs3bs :. rams :. nrs :. NilL :: LengthL 5 Bool) =
		bool32ToBool <$>
			(crs2bs :. crs2mbs :. crs3bs :. crams :. cnrs :. NilL)

getProperties :: P -> IO Properties
getProperties (P pdvc) = propertiesFromCore <$> alloca \pppts -> do
	C.getProperties pdvc pppts
	peek pppts

getFeatures :: P -> IO Features
getFeatures (P pdvc) = featuresFromCore <$> alloca \pfts -> do
	C.getFeatures pdvc pfts
	peek pfts

getFeatures2 :: FindPNextChainAll ns => P -> IO (Features2Result ns)
getFeatures2 (P pdvc) = features2FromCore =<< alloca \pfts -> do
	cfs <- C.getClearedFeatures
	poke pfts $ C.Features2 {
		C.features2SType = (),
		C.features2PNext = nullPtr,
		C.features2Features = cfs }
	C.getFeatures2 pdvc pfts
	peek pfts

data Features2Result ns = Features2Result {
	features2ResultNexts :: HeteroParList.PL Maybe ns,
	features2ResultFeatures :: Features }

deriving instance Show (HeteroParList.PL Maybe ns) => Show (Features2Result ns)

features2FromCore :: FindPNextChainAll ns => C.Features2 -> IO (Features2Result ns)
features2FromCore C.Features2 {
	C.features2PNext = pnxt,
	C.features2Features = ftrs } = do
	nxts <- findPNextChainAll pnxt
	let	ftrs' = featuresFromCore ftrs
	pure Features2Result {
		features2ResultNexts = nxts, features2ResultFeatures = ftrs' }

getQueueFamilyProperties :: P -> IO [(QueueFamily.Index, QueueFamily.Properties)]
getQueueFamilyProperties (P pdvc) =
	(QueueFamily.indices `zip`)
		. map QueueFamily.propertiesFromCore <$> alloca \ppptc ->
	C.getQueueFamilyProperties pdvc ppptc NullPtr >>
	peek ppptc >>= \(fromIntegral -> pptc) ->
	allocaArray pptc \pppts ->
	C.getQueueFamilyProperties pdvc ppptc pppts >>
	peekArray pptc pppts

enumerateExtensionProperties ::
	P -> Maybe T.Text -> IO [ExtensionProperties]
enumerateExtensionProperties (P pdvc) mlnm =
	map extensionPropertiesFromCore <$> case mlnm of
		Nothing -> go NullPtr
		Just lnm -> textToCString lnm go
	where
	go cmlnm = alloca \pExtensionCount ->
		C.enumerateExtensionProperties
			pdvc cmlnm pExtensionCount NullPtr >>= \r ->
		throwUnlessSuccess (Result r) >>
		peek pExtensionCount >>= \(fromIntegral -> extensionCount) ->
		allocaArray extensionCount \pAvailableExtensions -> do
			r' <- C.enumerateExtensionProperties pdvc cmlnm
				pExtensionCount pAvailableExtensions
			throwUnlessSuccess $ Result r'
			peekArray extensionCount pAvailableExtensions

data MemoryProperties = MemoryProperties {
	memoryPropertiesMemoryTypes :: [(Memory.M.TypeIndex, Memory.M.MType)],
	memoryPropertiesMemoryHeaps :: [Memory.M.Heap] }
	deriving Show

memoryPropertiesFromCore :: C.MemoryProperties -> MemoryProperties
memoryPropertiesFromCore C.MemoryProperties {
	C.memoryPropertiesMemoryTypeCount = fromIntegral -> mtc,
	C.memoryPropertiesMemoryTypes = (Memory.M.mTypeFromCore <$>) -> mts,
	C.memoryPropertiesMemoryHeapCount = fromIntegral -> mhc,
	C.memoryPropertiesMemoryHeaps = (Memory.M.heapFromCore <$>) -> mhs } =
	MemoryProperties {
		memoryPropertiesMemoryTypes = [0 ..] `zip` take mtc mts,
		memoryPropertiesMemoryHeaps = take mhc mhs }

getMemoryProperties :: P -> IO MemoryProperties
getMemoryProperties (P p) = memoryPropertiesFromCore <$> alloca \pmps -> do
	C.getMemoryProperties p pmps
	peek pmps

getFormatProperties :: P -> Format -> IO FormatProperties
getFormatProperties (P pdvc) (Format fmt) = formatPropertiesFromCore <$> alloca \pp -> do
	C.getFormatProperties pdvc fmt pp
	peek pp

data ShaderDrawParametersFeatures mn = ShaderDrawParametersFeatures {
	shaderDrawParametersFeaturesNext :: TMaybe.M mn,
	shaderDrawParametersFeaturesShaderDrawParameters :: Bool }

deriving instance Show (TMaybe.M mn) => Show (ShaderDrawParametersFeatures mn)

shaderDrawParametersFeaturesToCore :: WithPoked (TMaybe.M mn) =>
	ShaderDrawParametersFeatures mn ->
	(Ptr C.ShaderDrawParametersFeatures -> IO a) -> IO a
shaderDrawParametersFeaturesToCore ShaderDrawParametersFeatures {
	shaderDrawParametersFeaturesNext = mnxt,
	shaderDrawParametersFeaturesShaderDrawParameters = sdp } f =
	alloca \pfs -> withPoked' mnxt \pnxt -> do
		withPtrS pnxt \(castPtr -> pnxt') -> poke pfs
			C.ShaderDrawParametersFeatures {
				C.shaderDrawParametersFeaturesSType = (),
				C.shaderDrawParametersFeaturesPNext = pnxt',
				C.shaderDrawParametersFeaturesShaderDrawParameters =
					boolToBool32 sdp }
		f pfs

instance WithPoked (TMaybe.M mn) => WithPoked (ShaderDrawParametersFeatures mn) where
	withPoked' sdpfs f =
		shaderDrawParametersFeaturesToCore sdpfs $ f . ptrS . castPtr
