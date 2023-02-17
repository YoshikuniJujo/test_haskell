{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.ShaderStage.Middle.Internal (
	CreateInfo(..), createInfoToCore,
	CreateInfoListToCore, createInfoListToCore ) where

import Foreign.Ptr
import Foreign.Storable.PeekPoke
import Foreign.Storable.Hetero
import Data.TypeLevel
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:*), pattern (:**))

import qualified Data.ByteString as BS

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Pipeline.ShaderStage.Enum

import qualified Gpu.Vulkan.ShaderModule.Middle.Internal as ShaderModule
import qualified Gpu.Vulkan.Pipeline.ShaderStage.Core as C
import qualified Gpu.Vulkan.Specialization.Middle.Internal as Specialization

data CreateInfo n sknd vs = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoStage :: ShaderStageFlagBits,
	createInfoModule :: ShaderModule.M sknd,
	createInfoName :: BS.ByteString,
	createInfoSpecializationInfo :: Maybe (HeteroParList.L vs) }

deriving instance (Show n, Show (HeteroParList.L vs)) => Show (CreateInfo n sknd vs)

createInfoToCore ::
	forall n sknd vs r . (WithPoked n, PokableList vs) =>
	CreateInfo n sknd vs -> (C.CreateInfo -> IO r) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoStage = ShaderStageFlagBits stg,
	createInfoModule = ShaderModule.M mdl,
	createInfoName = nm,
	createInfoSpecializationInfo = mxs } f =
	withPokedMaybe' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	BS.useAsCString nm \cnm ->
	let	 ci pcsi = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt',
			C.createInfoFlags = flgs,
			C.createInfoStage = stg,
			C.createInfoModule = mdl,
			C.createInfoPName = cnm,
			C.createInfoPSpecializationInfo = pcsi } in
	case mxs of
		Nothing -> f $ ci NullPtr
		Just xs -> Specialization.infoToCore' xs \csi ->
			withPoked csi $ f . ci

class CreateInfoListToCore sss where
	createInfoListToCore ::
		HeteroParList.HeteroParList (V3 CreateInfo) sss ->
		([C.CreateInfo] -> IO r) -> IO ()

instance CreateInfoListToCore '[] where
	createInfoListToCore HeteroParList.HNil = (() <$) . ($ [])

instance (WithPoked n, PokableList vs, CreateInfoListToCore sss) =>
	CreateInfoListToCore ('(n, sknd, vs) ': sss) where
	createInfoListToCore (V3 ci :** cis) f =
		createInfoToCore ci \cci ->
		createInfoListToCore cis \ccis -> f $ cci : ccis
