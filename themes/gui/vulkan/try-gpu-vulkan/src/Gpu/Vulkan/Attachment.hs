{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Attachment (

	-- * DESCRIPTION

	Description(..), DescriptionListToMiddle(..),

	-- * REFERENCE

	M.Reference(..), M.A,

	-- * ENUM

	module Gpu.Vulkan.Attachment.Enum

	) where

import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:**))

import Gpu.Vulkan.Attachment.Enum

import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.Sample.Enum as Sample
import qualified Gpu.Vulkan.Image.Enum as Image

import Gpu.Vulkan.Attachment.Middle qualified as M

data Description (fmt :: T.Format) = Description {
	descriptionFlags :: DescriptionFlags,
	descriptionSamples :: Sample.CountFlagBits,
	descriptionLoadOp :: LoadOp,
	descriptionStoreOp :: StoreOp,
	descriptionStencilLoadOp :: LoadOp,
	descriptionStencilStoreOp :: StoreOp,
	descriptionInitialLayout :: Image.Layout,
	descriptionFinalLayout :: Image.Layout }
	deriving Show

class DescriptionListToMiddle fmts where
	descriptionListToMiddle ::
		HeteroParList.PL Description fmts -> [M.Description]

instance DescriptionListToMiddle '[] where descriptionListToMiddle HeteroParList.Nil = []

instance (T.FormatToValue fmt, DescriptionListToMiddle fmts) =>
	DescriptionListToMiddle (fmt ': fmts) where
	descriptionListToMiddle (d :** ds) =
		descriptionToMiddle d : descriptionListToMiddle ds

descriptionToMiddle :: forall fmt . T.FormatToValue fmt => Description fmt -> M.Description
descriptionToMiddle Description {
	descriptionFlags = flgs,
	descriptionSamples = smpls,
	descriptionLoadOp = lop,
	descriptionStoreOp = sop,
	descriptionStencilLoadOp = slop,
	descriptionStencilStoreOp = ssop,
	descriptionInitialLayout = ilyt,
	descriptionFinalLayout = flyt
	} = M.Description {
	M.descriptionFlags = flgs,
	M.descriptionFormat = T.formatToValue @fmt,
	M.descriptionSamples = smpls,
	M.descriptionLoadOp = lop,
	M.descriptionStoreOp = sop,
	M.descriptionStencilLoadOp = slop,
	M.descriptionStencilStoreOp = ssop,
	M.descriptionInitialLayout = ilyt,
	M.descriptionFinalLayout = flyt }
