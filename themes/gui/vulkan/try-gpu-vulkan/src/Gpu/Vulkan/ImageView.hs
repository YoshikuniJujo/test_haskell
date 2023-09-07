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

	create, recreate, recreate', I, CreateInfo(..),

	-- ** Manage Multiple Image View

	group, create', destroy, lookup, Group

	) where

import Prelude hiding (lookup)
import GHC.TypeLits
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

data CreateInfo n sm si nm ifmt (ivfmt :: T.Format) = CreateInfo {
	createInfoNext :: TMaybe.M n,
	createInfoFlags :: CreateFlags,
	createInfoImage :: Image.Binded sm si nm ifmt,
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
	WithPoked (TMaybe.M mn), T.FormatToValue ivfmt,
	AllocationCallbacks.ToMiddle mac ) =>
	Device.D sd -> CreateInfo mn sm si nm ifmt ivfmt ->
	TPMaybe.M (U2 AllocationCallbacks.A) mac ->
	(forall s . I nm ivfmt s -> IO a) -> IO a
create (Device.D d) ci (AllocationCallbacks.toMiddle -> mac) f = bracket
	(M.create d (createInfoToMiddle ci) mac)
	(\i -> M.destroy d i mac) (f . I)

group ::
	AllocationCallbacks.ToMiddle mac =>
	Device.D sd -> TPMaybe.M (U2 AllocationCallbacks.A) mac ->
	(forall s . Group s k nm ivfmt -> IO a) -> IO a
group (Device.D d) (AllocationCallbacks.toMiddle -> mac) f =
	M.group d mac $ f . Group

newtype Group s k (nm :: Symbol) (ivfmt :: T.Format) = Group (M.Group s k)

create' :: (
	Ord k, WithPoked (TMaybe.M mn), T.FormatToValue ivfmt,
	AllocationCallbacks.ToMiddle mac ) =>
	Device.D sd -> Group smng k nm ivfmt -> k ->
	CreateInfo mn sm si nm ifmt ivfmt ->
	TPMaybe.M (U2 AllocationCallbacks.A) mac ->
	IO (Either String (I nm ivfmt smng))
create' (Device.D d) (Group mng) k ci (AllocationCallbacks.toMiddle -> mac) =
	(I <$>) <$> M.create' d mng k (createInfoToMiddle ci) mac

destroy :: (Ord k, AllocationCallbacks.ToMiddle mac) =>
	Device.D sd -> Group sm k nm ivfmt  -> k ->
	TPMaybe.M (U2 AllocationCallbacks.A) mac -> IO (Either String ())
destroy (Device.D d) (Group mng) k (AllocationCallbacks.toMiddle -> mac) =
	M.destroy' d mng k mac

lookup :: Ord k => Group smng k nm ivfmt -> k -> IO (Maybe (I nm ivfmt smng))
lookup (Group mng) k = (I <$>) <$> M.lookup mng k

recreate :: (
	WithPoked (TMaybe.M mn), T.FormatToValue ivfmt,
	AllocationCallbacks.ToMiddle mac ) =>
	Device.D sd -> CreateInfo mn sm si nm ifmt ivfmt ->
	TPMaybe.M (U2 AllocationCallbacks.A) mac -> I nm ivfmt siv -> IO ()
recreate (Device.D d) ci
	(AllocationCallbacks.toMiddle -> mac) (I i) =
	M.recreate d (createInfoToMiddle ci) mac mac i

recreate' :: (
	WithPoked (TMaybe.M mn), T.FormatToValue ivfmt,
	AllocationCallbacks.ToMiddle mac ) =>
	Device.D sd -> CreateInfo mn sm si nm ifmt ivfmt ->
	TPMaybe.M (U2 AllocationCallbacks.A) mac -> I nm ivfmt siv -> IO a -> IO ()
recreate' (Device.D d) ci
	(AllocationCallbacks.toMiddle -> mac) (I i) =
	M.recreate' d (createInfoToMiddle ci) mac mac i
