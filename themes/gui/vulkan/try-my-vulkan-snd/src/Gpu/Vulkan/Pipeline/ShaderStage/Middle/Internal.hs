{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.ShaderStage.Middle.Internal (
	CreateInfo(..), createInfoToCore,
	CreateInfoListToCore, createInfoListToCore ) where

import Foreign.Ptr
import Foreign.Storable.PeekPoke
import Foreign.Storable.Hetero
import Control.Monad.Cont
import Data.HeteroList

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
	createInfoSpecializationInfo :: Maybe (HeteroList' vs) }

deriving instance (Show n, Show (HeteroList' vs)) => Show (CreateInfo n sknd vs)

createInfoToCore ::
	forall n sknd vs r . (Pokable n, PokableList vs) =>
	CreateInfo n sknd vs -> (C.CreateInfo -> IO r) -> IO r
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoStage = ShaderStageFlagBits stg,
	createInfoModule = ShaderModule.M mdl,
	createInfoName = nm,
	createInfoSpecializationInfo = mxs } f =
	withPokedMaybe mnxt \(castPtr -> pnxt) ->
	BS.useAsCString nm \cnm ->
	let	 ci pcsi = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt,
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
		HeteroVarList (V3 CreateInfo) sss -> ContT r IO [C.CreateInfo]

instance CreateInfoListToCore '[] where createInfoListToCore HVNil = pure []

instance (Pokable n, PokableList vs, CreateInfoListToCore sss) =>
	CreateInfoListToCore ('(n, sknd, vs) ': sss) where
	createInfoListToCore (V3 ci :...: cis) = (:)
		<$> ContT (createInfoToCore ci)
		<*> createInfoListToCore cis
