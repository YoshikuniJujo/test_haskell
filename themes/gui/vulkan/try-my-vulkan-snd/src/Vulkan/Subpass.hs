{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Subpass where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.Word

import Vulkan.Subpass.Enum

import qualified Vulkan.Pipeline.Enum as Pipeline
import qualified Vulkan.Attachment as Attachment
import qualified Vulkan.Subpass.Core as C

data Description = Description {
	descriptionFlags :: DescriptionFlags,
	descriptionPipelineBindPoint :: Pipeline.BindPoint,
	descriptionInputAttachments :: [Attachment.Reference],
	descriptionColorAndResolveAttachments :: Either
		[Attachment.Reference]
		[(Attachment.Reference, Attachment.Reference)],
	descriptionDepthStencilAttachment :: Maybe Attachment.Reference,
	descriptionPreserveAttachments :: [Word32] }
	deriving Show

descriptionToCore :: Description -> ContT r IO (Ptr C.Description)
descriptionToCore Description {
	descriptionFlags = DescriptionFlagBits flgs,
	descriptionPipelineBindPoint = Pipeline.BindPoint bp,
	descriptionInputAttachments =
		length &&& (Attachment.referenceToCore <$>) -> (iac, ias),
	descriptionColorAndResolveAttachments =
		(length &&& id)
			. either (, Nothing) ((Just `second`) . unzip) ->
		(cac, (	(Attachment.referenceToCore <$>) -> cas,
			((Attachment.referenceToCore <$>) <$>) -> mras)),
	descriptionDepthStencilAttachment =
		(Attachment.referenceToCore <$>) -> mdsa,
	descriptionPreserveAttachments = length &&& id -> (pac, pas) } = do
	pias <- ContT $ allocaArray iac
	lift $ pokeArray pias ias
	pcas <- ContT $ allocaArray cac
	lift $ pokeArray pcas cas
	pras <- case mras of
		Nothing -> pure NullPtr
		Just ras -> ContT (allocaArray cac) >>= \p ->
			p <$ lift (pokeArray p ras)
	pdsa <- case mdsa of
		Nothing -> pure NullPtr
		Just dsa -> ContT alloca >>= \p -> p <$ lift (poke p dsa)
	ppas <- ContT $ allocaArray pac
	lift $ pokeArray ppas pas
	let	C.Description_ fDescription = C.Description {
			C.descriptionFlags = flgs,
			C.descriptionPipelineBindPoint = bp,
			C.descriptionInputAttachmentCount = fromIntegral iac,
			C.descriptionPInputAttachments = pias,
			C.descriptionColorAttachmentCount = fromIntegral cac,
			C.descriptionPColorAttachments = pcas,
			C.descriptionPResolveAttachments = pras,
			C.descriptionPDepthStencilAttachment = pdsa,
			C.descriptionPreserveAttachmentCount = fromIntegral pac,
			C.descriptionPPreserveAttachments = ppas }
	ContT $ withForeignPtr fDescription
