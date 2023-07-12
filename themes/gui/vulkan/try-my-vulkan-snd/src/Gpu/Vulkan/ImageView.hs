{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImageView (

	-- * CREATE

	create, recreate, I, CreateInfo(..)

	) where

import Foreign.Storable.PeekPoke
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry

import Gpu.Vulkan.ImageView.Type
import Gpu.Vulkan.ImageView.Enum

import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Component as Component
import qualified Gpu.Vulkan.Image.Type as Image
import qualified Gpu.Vulkan.Image.Middle as Image.M
import qualified Gpu.Vulkan.ImageView.Middle as M

data CreateInfo n si sm nm ifmt (ivfmt :: T.Format) = CreateInfo {
	createInfoNext :: TMaybe.M n,
	createInfoFlags :: CreateFlags,
	createInfoImage :: Image.Binded si sm nm ifmt,
	createInfoViewType :: Type,
	createInfoComponents :: Component.Mapping,
	createInfoSubresourceRange :: Image.M.SubresourceRange }

createInfoToMiddle :: forall n si sm nm ifmt ivfmt . T.FormatToValue ivfmt =>
	CreateInfo n si sm nm ifmt ivfmt -> M.CreateInfo n
createInfoToMiddle CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoImage = Image.Binded img,
	createInfoViewType = tp,
	createInfoComponents = cps,
	createInfoSubresourceRange = srr } = M.CreateInfo {
	M.createInfoNext = mnxt,
	M.createInfoFlags = flgs,
	M.createInfoImage = img,
	M.createInfoViewType = tp,
	M.createInfoFormat = fmt,
	M.createInfoComponents = cps,
	M.createInfoSubresourceRange = srr }
	where fmt = T.formatToValue @ivfmt

create :: (
	T.FormatToValue ivfmt, WithPoked (TMaybe.M n),
	AllocationCallbacks.ToMiddle mscc ) =>
	Device.D sd -> CreateInfo n si sm nm ifmt ivfmt ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc ->
	(forall siv . I nm ivfmt siv -> IO a) -> IO a
create (Device.D dvc) ci
	(AllocationCallbacks.toMiddle -> macc) f = bracket
	(M.create dvc (createInfoToMiddle ci) macc)
	(\i -> M.destroy dvc i macc) (f . I)

recreate :: (
	T.FormatToValue ivfmt, WithPoked (TMaybe.M n),
	AllocationCallbacks.ToMiddle mscc ) =>
	Device.D sd -> CreateInfo n si sm nm ifmt ivfmt ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc ->
	I nm ivfmt s -> IO ()
recreate (Device.D dvc) ci
	(AllocationCallbacks.toMiddle -> macc) (I i) =
	M.recreate dvc (createInfoToMiddle ci) macc macc i
