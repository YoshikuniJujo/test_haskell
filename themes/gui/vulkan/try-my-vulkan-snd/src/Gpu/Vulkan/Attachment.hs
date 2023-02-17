{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Attachment (
	DescriptionNew(..), DescriptionsFromNew(..),
	Reference(..), A ) where

import qualified Data.HeteroParList as HeteroParList
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:*), pattern (:**))

import Gpu.Vulkan.Attachment.Enum

import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.Sample.Enum as Sample
import qualified Gpu.Vulkan.Image.Enum as Image

import Gpu.Vulkan.Attachment.Middle

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

class DescriptionsFromNew fmts where
	descriptionsFromNew ::
		HeteroParList.HeteroParList DescriptionNew fmts -> [Description]

instance DescriptionsFromNew '[] where descriptionsFromNew HeteroParList.HNil = []

instance (T.FormatToValue fmt, DescriptionsFromNew fmts) =>
	DescriptionsFromNew (fmt ': fmts) where
	descriptionsFromNew (d :** ds) =
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
