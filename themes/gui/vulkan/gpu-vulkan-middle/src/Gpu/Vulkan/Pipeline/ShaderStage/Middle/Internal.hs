{-# LANGUAGE ImportQualifiedPost #-}
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
import Foreign.Storable.HeteroList
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.Tuple.Uncurry
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:**))

import qualified Data.ByteString as BS

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Pipeline.ShaderStage.Enum

import qualified Gpu.Vulkan.ShaderModule.Middle.Internal as ShaderModule
import qualified Gpu.Vulkan.Pipeline.ShaderStage.Core as C
import qualified Gpu.Vulkan.Specialization.Middle.Internal as Specialization

data CreateInfo mn sknd sivs = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoStage :: ShaderStageFlagBits,
	createInfoModule :: ShaderModule.S sknd,
	createInfoName :: BS.ByteString,
	createInfoSpecializationInfo :: Maybe (HeteroParList.L sivs) }

deriving instance (Show (TMaybe.M mn), Show (HeteroParList.L sivs)) =>
	Show (CreateInfo mn sknd sivs)

createInfoToCore ::
	forall mn sknd sivs r . (WithPoked (TMaybe.M mn), PokableList sivs) =>
	CreateInfo mn sknd sivs -> (C.CreateInfo -> IO r) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoStage = ShaderStageFlagBits stg,
	createInfoModule = ShaderModule.S mdl,
	createInfoName = nm,
	createInfoSpecializationInfo = mxs } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
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
		Just xs -> Specialization.infoToCore xs \csi ->
			withPoked csi $ f . ci

class CreateInfoListToCore cias where
	createInfoListToCore ::
		HeteroParList.PL (U3 CreateInfo) cias ->
			([C.CreateInfo] -> IO r) -> IO ()

instance CreateInfoListToCore '[] where
	createInfoListToCore HeteroParList.Nil = (() <$) . ($ [])

instance (WithPoked (TMaybe.M mn), PokableList sivs, CreateInfoListToCore cias) =>
	CreateInfoListToCore ('(mn, sknd, sivs) ': cias) where
	createInfoListToCore (U3 ci :** cis) f =
		createInfoToCore ci \cci ->
		createInfoListToCore cis \ccis -> f $ cci : ccis
