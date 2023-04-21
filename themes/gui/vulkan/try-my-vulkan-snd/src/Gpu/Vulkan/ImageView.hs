{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImageView where

import GHC.TypeLits
import Foreign.Storable.PeekPoke
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe

import Gpu.Vulkan.Enum
import Gpu.Vulkan.ImageView.Enum

import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Component as Component
import qualified Gpu.Vulkan.Image.Type as Image
import qualified Gpu.Vulkan.Image.Middle as Image.M
import qualified Gpu.Vulkan.ImageView.Middle as M

newtype INew (fmt :: T.Format) (nm :: Symbol) si = INew M.I deriving Show

newtype I s = I M.I deriving Show

iToOld :: INew fmt nm si -> I s
iToOld (INew i) = I i

iToNew :: I s -> INew fmt nm si
iToNew (I i) = INew i

data CreateInfoNew n si sm nm ifmt (ivfmt :: T.Format) = CreateInfoNew {
	createInfoNextNew :: TMaybe.M n,
	createInfoFlagsNew :: CreateFlags,
	createInfoImageNew :: Image.BindedNew si sm nm ifmt,
	createInfoViewTypeNew :: Type,
	createInfoComponentsNew :: Component.Mapping,
	createInfoSubresourceRangeNew :: Image.M.SubresourceRange }

data CreateInfo si sm n = CreateInfo {
	createInfoNext :: TMaybe.M n,
	createInfoFlags :: CreateFlags,
	createInfoImage :: Image.Binded si sm,
	createInfoViewType :: Type,
	createInfoFormat :: Format,
	createInfoComponents :: Component.Mapping,
	createInfoSubresourceRange :: Image.M.SubresourceRange }

createInfoToMiddleNew :: forall n si sm nm ifmt ivfmt . T.FormatToValue ivfmt =>
	CreateInfoNew n si sm nm ifmt ivfmt -> M.CreateInfo n
createInfoToMiddleNew CreateInfoNew {
	createInfoNextNew = mnxt,
	createInfoFlagsNew = flgs,
	createInfoImageNew = Image.BindedNew img,
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

createInfoToMiddle :: CreateInfo si sm n -> M.CreateInfo n
createInfoToMiddle CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoImage = Image.Binded img,
	createInfoViewType = tp,
	createInfoFormat = fmt,
	createInfoComponents = cps,
	createInfoSubresourceRange = srr } = M.CreateInfo {
	M.createInfoNext = mnxt,
	M.createInfoFlags = flgs,
	M.createInfoImage = img,
	M.createInfoViewType = tp,
	M.createInfoFormat = fmt,
	M.createInfoComponents = cps,
	M.createInfoSubresourceRange = srr }

createNew :: (
	T.FormatToValue ivfmt,
	WithPoked (TMaybe.M n), Pokable c, Pokable d ) =>
	Device.D sd -> CreateInfoNew n si sm nm ifmt ivfmt ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall siv . INew ivfmt nm siv -> IO a) -> IO a
createNew (Device.D dvc) ci macc macd f = bracket
	(M.create dvc (createInfoToMiddleNew ci) macc)
	(\i -> M.destroy dvc i macd) (f . INew)

create :: (WithPoked (TMaybe.M n), Pokable c, Pokable d) =>
	Device.D sd -> CreateInfo si sm n ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall s . I s -> IO a) -> IO a
create (Device.D dvc) ci macc macd f = bracket
	(M.create dvc (createInfoToMiddle ci) macc)
	(\i -> M.destroy dvc i macd) (f . I)

recreateNew :: (
	T.FormatToValue ivfmt,
	WithPoked (TMaybe.M n), Pokable c, Pokable d ) =>
	Device.D sd -> CreateInfoNew n si sm nm ifmt ivfmt ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	INew ivfmt nm s -> IO ()
recreateNew (Device.D dvc) ci macc macd (INew i) =
	M.recreate dvc (createInfoToMiddleNew ci) macc macd i

recreate :: (WithPoked (TMaybe.M n), Pokable c, Pokable d) =>
	Device.D sd -> CreateInfo si sm n ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	I s -> IO ()
recreate (Device.D dvc) ci macc macd (I i) =
	M.recreate dvc (createInfoToMiddle ci) macc macd i
