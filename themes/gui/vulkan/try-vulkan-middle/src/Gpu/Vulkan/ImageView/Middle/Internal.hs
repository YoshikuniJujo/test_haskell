{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImageView.Middle.Internal (
	I, CreateInfo(..), create, recreate, recreate', destroy,

	group, create', destroy', lookup, Group,

	iToCore
	) where

import Prelude hiding (lookup)

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.Map qualified as M
import Data.IORef

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Component.Middle.Internal
import Gpu.Vulkan.ImageView.Enum

import Gpu.Vulkan.AllocationCallbacks.Middle.Internal
	qualified as AllocationCallbacks
import {-# SOURCE #-} qualified Gpu.Vulkan.Device.Middle.Internal as Device
import qualified Gpu.Vulkan.Image.Middle.Internal as Image
import qualified Gpu.Vulkan.ImageView.Core as C

data CreateInfo mn = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoImage :: Image.I,
	createInfoViewType :: Type,
	createInfoFormat :: Format,
	createInfoComponents :: Mapping,
	createInfoSubresourceRange :: Image.SubresourceRange }

createInfoToCore :: WithPoked (TMaybe.M mn) =>
	CreateInfo mn -> (Ptr C.CreateInfo -> IO a) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoImage = Image.I rimg,
	createInfoViewType = Type tp,
	createInfoFormat = Format fmt,
	createInfoComponents = cpns,
	createInfoSubresourceRange = srr
	} f = withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	readIORef rimg >>= \(_, img) -> let ci = C.CreateInfo {
		C.createInfoSType = (),
		C.createInfoPNext = pnxt',
		C.createInfoFlags = flgs,
		C.createInfoImage = img,
		C.createInfoViewType = tp,
		C.createInfoFormat = fmt,
		C.createInfoComponents = mappingToCore cpns,
		C.createInfoSubresourceRange =
			Image.subresourceRangeToCore srr } in
	withPoked ci f

newtype I = I (IORef C.I)

instance Show I where show _ = "Vk.ImageView.I"

iToCore :: I -> IO C.I
iToCore (I i) = readIORef i

iFromCore :: C.I -> IO I
iFromCore i = I <$> newIORef i

create :: WithPoked (TMaybe.M mn) =>
	Device.D -> CreateInfo mn -> TPMaybe.M AllocationCallbacks.A mc -> IO I
create (Device.D dvc) ci mac = iFromCore =<< alloca \pView -> do
	createInfoToCore ci \pci ->
		AllocationCallbacks.mToCore mac \pac -> do
			r <- C.create dvc pci pac pView
			throwUnlessSuccess $ Result r
	peek pView

group :: Device.D -> TPMaybe.M AllocationCallbacks.A mc ->
	(forall s . Group s k -> IO a) -> IO a
group dvc mac f = do
	(sem, mng) <- atomically $ (,) <$> newTSem 1 <*> newTVar M.empty
	rtn <- f $ Group sem mng
	((\iv -> destroy dvc iv mac) `mapM_`) =<< atomically (readTVar mng)
	pure rtn

data Group s k = Group TSem (TVar (M.Map k I))

create' :: (Ord k, WithPoked (TMaybe.M mn)) =>
	Device.D -> Group sm k -> k ->
	CreateInfo mn -> TPMaybe.M AllocationCallbacks.A mc -> IO (Either String I)
create' (Device.D dvc) (Group sem is) k ci mac = do
	ok <- atomically do
		mx <- (M.lookup k) <$> readTVar is
		case mx of
			Nothing -> waitTSem sem >> pure True
			Just _ -> pure False
	if ok
	then do	i <- iFromCore =<< alloca \pView -> do
			createInfoToCore ci \pci ->
				AllocationCallbacks.mToCore mac \pac -> do
					r <- C.create dvc pci pac pView
					throwUnlessSuccess $ Result r
			peek pView
		atomically $ modifyTVar is (M.insert k i) >> signalTSem sem
		pure $ Right i
	else pure . Left $ "Gpu.Vulkan.ImageView.create': The key already exist"

destroy' :: Ord k => Device.D ->
	Group sm k -> k -> TPMaybe.M AllocationCallbacks.A mc -> IO (Either String ())
destroy' dvc (Group sem is) k mac = do
	mi <- atomically do
		mx <- (M.lookup k) <$> readTVar is
		case mx of
			Nothing -> pure Nothing
			Just _ -> waitTSem sem >> pure mx
	case mi of
		Nothing -> pure $ Left "Gpu.Vulkan.ImageView.destroy: No such key"
		Just i -> do
			destroy dvc i mac
			atomically do
				modifyTVar is (M.delete k)
				signalTSem sem
				pure $ Right ()

lookup :: Ord k => Group sm k -> k -> IO (Maybe I)
lookup (Group _sem is) k = atomically $ M.lookup k <$> readTVar is

recreate :: WithPoked (TMaybe.M mn) =>
	Device.D -> CreateInfo mn ->
	TPMaybe.M AllocationCallbacks.A mc -> TPMaybe.M AllocationCallbacks.A md ->
	I -> IO ()
recreate (Device.D dvc) ci macc macd (I ri) = alloca \pView ->
	createInfoToCore ci \pci ->
	AllocationCallbacks.mToCore macc \pac -> do
		r <- C.create dvc pci pac pView
		throwUnlessSuccess $ Result r
		io <- readIORef ri
		AllocationCallbacks.mToCore macd $ C.destroy dvc io
		writeIORef ri =<< peek pView

recreate' :: WithPoked (TMaybe.M mn) =>
	Device.D -> CreateInfo mn ->
	TPMaybe.M AllocationCallbacks.A mc -> TPMaybe.M AllocationCallbacks.A md ->
	I -> IO a -> IO ()
recreate' (Device.D dvc) ci macc macd (I ri) act = alloca \pView ->
	createInfoToCore ci \pci ->
	AllocationCallbacks.mToCore macc \pac -> do
		r <- C.create dvc pci pac pView
		throwUnlessSuccess $ Result r
		io <- readIORef ri
		writeIORef ri =<< peek pView
		rtn <- act
		AllocationCallbacks.mToCore macd $ C.destroy dvc io
		pure rtn

destroy :: Device.D -> I -> TPMaybe.M AllocationCallbacks.A md -> IO ()
destroy (Device.D dvc) iv mac = do
	iv' <- iToCore iv
	AllocationCallbacks.mToCore mac $ C.destroy dvc iv'
