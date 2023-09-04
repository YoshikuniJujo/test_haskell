{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PhysicalDevice.Struct.ThTest where

import Language.Haskell.TH

import Foreign.Ptr
import Foreign.Storable.PeekPoke

import Data.TypeLevel.Maybe qualified as TMaybe
import Gpu.Vulkan.PhysicalDevice.Struct.Core qualified as C

import Gpu.Vulkan.Base.Middle.Internal

data DescriptorIndexingFeatures mn = DescriptorIndexingFeatures {
	descriptorIndexingFeaturesNext :: TMaybe.M mn,
	descriptorIndexingFeaturesShaderInputAttachmentArrayDynamicIndexing :: Bool
	}

foo :: DecsQ
foo = [d|

	data DescriptorIndexingFeatures mn = DescriptorIndexingFeatures {
		descriptorIndexingFeaturesNext :: TMaybe.M mn,
		descriptorIndexingFeaturesShaderInputAttachmentArrayDynamicIndexing :: Bool
		}
	|]

mkFoo :: DecQ
mkFoo = dataD (cxt [])
	(mkName "DescriptorIndexingFeatures")
	[plainTV $ mkName "mn"] Nothing
	[recC (mkName "DescriptorIndexingFeatures") [
		varBangType (mkName "descriptorIndexingFeaturesNext") $ bangType
			(bang noSourceUnpackedness noSourceStrictness)
			(conT ''TMaybe.M `appT` varT (mkName "mn")),
		varBangType (mkName "descriptorIndexingFeaturesShaderInputAttachmentArrayDynamicIndexing") $ bangType
			(bang noSourceUnpackedness noSourceStrictness)
			(conT ''Bool)
		]]
	[]

deriving instance Show (TMaybe.M mn) => Show (DescriptorIndexingFeatures mn)

data DescriptorIndexingFeaturesNoNext = DescriptorIndexingFeaturesNoNext {
	descriptorIndexingFeaturesNoNextShaderInputAttachmentArrayDynamicIndexing :: Bool
	}
	deriving Show

descriptorIndexingFeaturesToCore :: WithPoked (TMaybe.M mn) =>
	DescriptorIndexingFeatures mn ->
	(C.DescriptorIndexingFeatures -> IO a) -> IO ()
descriptorIndexingFeaturesToCore DescriptorIndexingFeatures {
	descriptorIndexingFeaturesNext = mnxt,
	descriptorIndexingFeaturesShaderInputAttachmentArrayDynamicIndexing =
		siaad } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	f C.DescriptorIndexingFeatures {
		C.descriptorIndexingFeaturesPNext = pnxt',
		C.descriptorIndexingFeaturesShaderInputAttachmentArrayDynamicIndexing =
			boolToBool32 siaad }

bar :: DecsQ
bar = [d|
	descriptorIndexingFeaturesToCore :: WithPoked (TMaybe.M mn) =>
		DescriptorIndexingFeatures mn ->
		(C.DescriptorIndexingFeatures -> IO a) -> IO ()
	descriptorIndexingFeaturesToCore DescriptorIndexingFeatures {
		descriptorIndexingFeaturesNext = mnxt,
		descriptorIndexingFeaturesShaderInputAttachmentArrayDynamicIndexing =
			siaad } f =
		withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
		f C.DescriptorIndexingFeatures {
			C.descriptorIndexingFeaturesPNext = pnxt',
			C.descriptorIndexingFeaturesShaderInputAttachmentArrayDynamicIndexing =
				boolToBool32 siaad }
	|]

mkBarType :: DecQ
mkBarType = sigD (mkName "descriptorIndexingFeatureToCore")
	(forallT []
		(cxt [conT ''WithPoked `appT`
			(conT ''TMaybe.M `appT` varT (mkName "mn"))])
		(conT (mkName "DescriptorIndexingFeatures") `appT` varT (mkName "mn") `arrT`
			(conT ''C.DescriptorIndexingFeatures `arrT` conT ''IO `appT` conT ''()) `arrT`
			conT ''IO `appT` conT ''()))

mkBarBody :: DecQ
mkBarBody = funD (mkName "descriptorIndexingFeaturesToCore")
	. (: []) . ($ []) . clause [mkBarPat, varP $ mkName "f"] . normalB
	$ varE 'withPoked' `appE` varE (mkName "mnxt") `appE`
		lamE [varP $ mkName "pnxt"] (
			varE 'withPtrS `appE` varE (mkName "pnxt") `appE`
			lamE [viewP (varE 'castPtr) (varP $ mkName "pnxt'")] (
				varE (mkName "f") `appE`
				recConE 'C.DescriptorIndexingFeatures [
					('C.descriptorIndexingFeaturesPNext ,) <$> varE (mkName "pnxt'"),
					('C.descriptorIndexingFeaturesShaderInputAttachmentArrayDynamicIndexing ,)
						<$> varE 'boolToBool32 `appE` varE (mkName "siaad")
					] ) )

mkBarPat :: PatQ
mkBarPat = recP (mkName "DescriptorIndexingFeatures") [
	fieldPat 'descriptorIndexingFeaturesNext (varP $ mkName "mnxt"),
	fieldPat 'descriptorIndexingFeaturesShaderInputAttachmentArrayDynamicIndexing
		(varP $ mkName "siaad")
	]

infixr 8 `arrT`

arrT :: TypeQ -> TypeQ -> TypeQ
t1 `arrT` t2 = arrowT `appT` t1 `appT` t2

descriptorIndexingFeaturesFromCore ::
	C.DescriptorIndexingFeatures -> DescriptorIndexingFeaturesNoNext
descriptorIndexingFeaturesFromCore C.DescriptorIndexingFeatures {
	C.descriptorIndexingFeaturesShaderInputAttachmentArrayDynamicIndexing =
		siaad } =
	DescriptorIndexingFeaturesNoNext {
		descriptorIndexingFeaturesNoNextShaderInputAttachmentArrayDynamicIndexing =
			bool32ToBool siaad }
