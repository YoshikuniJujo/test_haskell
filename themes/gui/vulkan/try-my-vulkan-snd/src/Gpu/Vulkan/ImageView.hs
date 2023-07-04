{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImageView (
	I(..), createNew, recreateNew, CreateInfoNew(..) ) where

import GHC.TypeLits
import Foreign.Storable.PeekPoke
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry

import Gpu.Vulkan.ImageView.Enum

import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Component as Component
import qualified Gpu.Vulkan.Image.Type as Image
import qualified Gpu.Vulkan.Image.Middle as Image.M
import qualified Gpu.Vulkan.ImageView.Middle as M

newtype I (nm :: Symbol) (fmt :: T.Format) si = I M.I deriving Show

data CreateInfoNew n si sm nm ifmt (ivfmt :: T.Format) = CreateInfoNew {
	createInfoNextNew :: TMaybe.M n,
	createInfoFlagsNew :: CreateFlags,
	createInfoImageNew :: Image.Binded si sm nm ifmt,
	createInfoViewTypeNew :: Type,
	createInfoComponentsNew :: Component.Mapping,
	createInfoSubresourceRangeNew :: Image.M.SubresourceRange }

createInfoToMiddleNew :: forall n si sm nm ifmt ivfmt . T.FormatToValue ivfmt =>
	CreateInfoNew n si sm nm ifmt ivfmt -> M.CreateInfo n
createInfoToMiddleNew CreateInfoNew {
	createInfoNextNew = mnxt,
	createInfoFlagsNew = flgs,
	createInfoImageNew = Image.Binded img,
	createInfoViewTypeNew = tp,
	createInfoComponentsNew = cps,
	createInfoSubresourceRangeNew = srr } = M.CreateInfo {
	M.createInfoNext = mnxt,
	M.createInfoFlags = flgs,
	M.createInfoImage = img,
	M.createInfoViewType = tp,
	M.createInfoFormat = fmt,
	M.createInfoComponents = cps,
	M.createInfoSubresourceRange = srr }
	where fmt = T.formatToValue @ivfmt

createNew :: (
	T.FormatToValue ivfmt, WithPoked (TMaybe.M n),
	AllocationCallbacks.ToMiddle mscc ) =>
	Device.D sd -> CreateInfoNew n si sm nm ifmt ivfmt ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc ->
	(forall siv . I nm ivfmt siv -> IO a) -> IO a
createNew (Device.D dvc) ci
	(AllocationCallbacks.toMiddle -> macc) f = bracket
	(M.create dvc (createInfoToMiddleNew ci) macc)
	(\i -> M.destroy dvc i macc) (f . I)

recreateNew :: (
	T.FormatToValue ivfmt, WithPoked (TMaybe.M n),
	AllocationCallbacks.ToMiddle mscc ) =>
	Device.D sd -> CreateInfoNew n si sm nm ifmt ivfmt ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc ->
	I nm ivfmt s -> IO ()
recreateNew (Device.D dvc) ci
	(AllocationCallbacks.toMiddle -> macc) (I i) =
	M.recreate dvc (createInfoToMiddleNew ci) macc macc i
