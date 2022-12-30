{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Attachment where

import Foreign.Storable
import Foreign.C.Enum
import Data.HeteroList
import Data.Word

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Attachment.Enum

import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.Sample.Enum as Sample
import qualified Gpu.Vulkan.Image.Enum as Image
import qualified Gpu.Vulkan.Attachment.Core as C

#include <vulkan/vulkan.h>

data DescriptionNew (fmt :: T.Format) = DescriptionNew {
	descriptionFlagsNew :: DescriptionFlags,
	descriptionSamplesNew :: Sample.CountFlagBits,
	descriptionLoadOpNew :: LoadOp,
	descriptionStoreOpNew :: StoreOp,
	descriptionStencilLoadOpNew :: LoadOp,
	descriptionStencilStoreOpNew :: StoreOp,
	descriptionInitialLayoutNew :: Image.Layout,
	descriptionFinalLayoutNew :: Image.Layout }
	deriving Show

class DescriptionsToCoreNew fmts where
	descriptionsToCoreNew ::
		HeteroVarList DescriptionNew fmts -> [C.Description]

instance DescriptionsToCoreNew '[] where descriptionsToCoreNew HVNil = []

instance (T.FormatToValue fmt, DescriptionsToCoreNew fmts) =>
	DescriptionsToCoreNew (fmt ': fmts) where
	descriptionsToCoreNew (dsc :...: dscs) =
		descriptionToCoreNew dsc : descriptionsToCoreNew dscs

descriptionToCoreNew ::
	forall fmt . T.FormatToValue fmt =>  DescriptionNew fmt -> C.Description
descriptionToCoreNew DescriptionNew {
	descriptionFlagsNew = DescriptionFlagBits flgs,
	descriptionSamplesNew = Sample.CountFlagBits smps,
	descriptionLoadOpNew = LoadOp lo,
	descriptionStoreOpNew = StoreOp so,
	descriptionStencilLoadOpNew = LoadOp slo,
	descriptionStencilStoreOpNew = StoreOp sso,
	descriptionInitialLayoutNew = Image.Layout il,
	descriptionFinalLayoutNew = Image.Layout fl
	} = C.Description {
		C.descriptionFlags = flgs,
		C.descriptionFormat = fmt,
		C.descriptionSamples = smps,
		C.descriptionLoadOp = lo,
		C.descriptionStoreOp = so,
		C.descriptionStencilLoadOp = slo,
		C.descriptionStencilStoreOp = sso,
		C.descriptionInitialLayout = il,
		C.descriptionFinalLayout = fl }
	where
	Format fmt = T.formatToValue @fmt

data Description = Description {
	descriptionFlags :: DescriptionFlags,
	descriptionFormat :: Format,
	descriptionSamples :: Sample.CountFlagBits,
	descriptionLoadOp :: LoadOp,
	descriptionStoreOp :: StoreOp,
	descriptionStencilLoadOp :: LoadOp,
	descriptionStencilStoreOp :: StoreOp,
	descriptionInitialLayout :: Image.Layout,
	descriptionFinalLayout :: Image.Layout }
	deriving Show

class DescriptionsFromNew fmts where
	descriptionsFromNew ::
		HeteroVarList DescriptionNew fmts -> [Description]

instance DescriptionsFromNew '[] where descriptionsFromNew HVNil = []

instance (T.FormatToValue fmt, DescriptionsFromNew fmts) =>
	DescriptionsFromNew (fmt ': fmts) where
	descriptionsFromNew (d :...: ds) =
		descriptionFromNew d : descriptionsFromNew ds

descriptionFromNew :: forall fmt . T.FormatToValue fmt => DescriptionNew fmt -> Description
descriptionFromNew DescriptionNew {
	descriptionFlagsNew = flgs,
	descriptionSamplesNew = smpls,
	descriptionLoadOpNew = lop,
	descriptionStoreOpNew = sop,
	descriptionStencilLoadOpNew = slop,
	descriptionStencilStoreOpNew = ssop,
	descriptionInitialLayoutNew = ilyt,
	descriptionFinalLayoutNew = flyt
	} = Description {
	descriptionFlags = flgs,
	descriptionFormat = T.formatToValue @fmt,
	descriptionSamples = smpls,
	descriptionLoadOp = lop,
	descriptionStoreOp = sop,
	descriptionStencilLoadOp = slop,
	descriptionStencilStoreOp = ssop,
	descriptionInitialLayout = ilyt,
	descriptionFinalLayout = flyt }

descriptionToCore :: Description -> C.Description
descriptionToCore Description {
	descriptionFlags = DescriptionFlagBits flgs,
	descriptionFormat = Format fmt,
	descriptionSamples = Sample.CountFlagBits smps,
	descriptionLoadOp = LoadOp lo,
	descriptionStoreOp = StoreOp so,
	descriptionStencilLoadOp = LoadOp slo,
	descriptionStencilStoreOp = StoreOp sso,
	descriptionInitialLayout = Image.Layout il,
	descriptionFinalLayout = Image.Layout fl
	} = C.Description {
		C.descriptionFlags = flgs,
		C.descriptionFormat = fmt,
		C.descriptionSamples = smps,
		C.descriptionLoadOp = lo,
		C.descriptionStoreOp = so,
		C.descriptionStencilLoadOp = slo,
		C.descriptionStencilStoreOp = sso,
		C.descriptionInitialLayout = il,
		C.descriptionFinalLayout = fl }

enum "A" ''#{type uint32_t} [''Show, ''Storable, ''Num]
	[("AUnused", #{const VK_ATTACHMENT_UNUSED})]

data Reference = Reference {
	referenceAttachment :: A,
	referenceLayout :: Image.Layout }
	deriving Show

referenceToCore :: Reference -> C.Reference
referenceToCore Reference {
	referenceAttachment = A a,
	referenceLayout = Image.Layout lyt } = C.Reference {
		C.referenceAttachment = a,
		C.referenceLayout = lyt }
