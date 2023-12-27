{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.BufferView.Internal (

	-- * CREATE

	create, B(..), CreateInfo(..), FormatOf,

	-- ** Buffer View Group
	Group, group, create', unsafeDestroy, lookup

	) where

import Prelude hiding (lookup)

import GHC.TypeLits
import Foreign.Storable.PeekPoke
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Exception
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.Map qualified as Map
import Data.HeteroParList qualified as HeteroParList
import Gpu.Vulkan.Object qualified as VObj

import Gpu.Vulkan.AllocationCallbacks qualified as AllocationCallbacks
import Gpu.Vulkan.AllocationCallbacks.Type qualified as AllocationCallbacks
import Gpu.Vulkan.Device qualified as Device
import Gpu.Vulkan.Device.Type qualified as Device
import Gpu.Vulkan.Device.Middle qualified as Device.M
import Gpu.Vulkan.TypeEnum qualified as TEnum
import Gpu.Vulkan.Buffer.Type qualified as Buffer
import Gpu.Vulkan.BufferView.Middle qualified as M

newtype B s (nm :: Symbol) t = B M.B deriving Show

create :: (
	WithPoked (TMaybe.M mn),
	TEnum.FormatToValue (FormatOf t),
	VObj.OffsetOfList t nm objs,
	AllocationCallbacks.ToMiddle mscc ) =>
	Device.D sd -> CreateInfo mn t nm '(sm, sb, bnm, objs) ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc ->
	(forall s . B s nm t -> IO a) -> IO a
create (Device.D dvc) ci
	(AllocationCallbacks.toMiddle -> macc) f = bracket
	(M.create dvc (createInfoToMiddle ci) macc)
	(\b -> M.destroy dvc b macc) (f . B)

data CreateInfo mn t (nm :: Symbol) snmobjs = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: M.CreateFlags,
	createInfoBuffer :: U4 Buffer.Binded snmobjs }

createInfoToMiddle :: forall n t nm sm sb bnm objs . (
	TEnum.FormatToValue (FormatOf t),
	VObj.OffsetOfList t nm objs ) =>
	CreateInfo n t nm '(sm, sb, bnm, objs) -> M.CreateInfo n
createInfoToMiddle CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoBuffer = U4 (Buffer.Binded lns b) } = M.CreateInfo {
	M.createInfoNext = mnxt,
	M.createInfoFlags = flgs,
	M.createInfoBuffer = b,
	M.createInfoFormat = TEnum.formatToValue @(FormatOf t),
	M.createInfoOffset = ost, M.createInfoRange = rng }
	where
	(ost, rng) = offsetRange @t @nm lns

type family FormatOf t :: TEnum.Format

offsetRange :: forall t nm objs .
	VObj.OffsetOfList t nm objs =>
	HeteroParList.PL VObj.Length objs -> (Device.M.Size, Device.M.Size)
offsetRange = VObj.offsetOfList @t @nm

group :: AllocationCallbacks.ToMiddle ma =>
	Device.D sd -> TPMaybe.M (U2 AllocationCallbacks.A) ma ->
	(forall s . Group ma s k nm t -> IO a) -> IO a
group (Device.D mdvc) mac@(AllocationCallbacks.toMiddle -> mmac) f = do
	(sem, m) <- atomically $ (,) <$> newTSem 1 <*> newTVar Map.empty
	rtn <- f $ Group mac sem m
	((\(B mb) -> M.destroy mdvc mb mmac) `mapM_`)
		=<< atomically (readTVar m)
	pure rtn

data Group ma s k nm t = Group
	(TPMaybe.M (U2 AllocationCallbacks.A) ma)
	TSem (TVar (Map.Map k (B s nm t)))

create' :: (
	Ord k,
	WithPoked (TMaybe.M mn),
	TEnum.FormatToValue (FormatOf t),
	VObj.OffsetOfList t nm objs,
	AllocationCallbacks.ToMiddle mscc ) =>
	Device.D sd -> Group mscc s k nm t -> k ->
	CreateInfo mn t nm '(sm, sb, bnm, objs) -> IO (Either String (B s nm t))
create' (Device.D dvc)
	(Group (AllocationCallbacks.toMiddle -> mac) sem bs) k ci = do
	ok <- atomically do
		mx <- (Map.lookup k) <$> readTVar bs
		case mx of
			Nothing -> waitTSem sem >> pure True
			Just _ -> pure False
	if ok
	then do	b <- M.create dvc (createInfoToMiddle ci) mac
		let	b' = B b
		atomically $ modifyTVar bs (Map.insert k b') >> signalTSem sem
		pure $ Right b'
	else pure . Left $ "Gpu.Vulkan.BufferView.create': The key already exist"

unsafeDestroy :: (Ord k, AllocationCallbacks.ToMiddle ma) =>
	Device.D sd -> Group ma sg k nm t -> k -> IO (Either String ())
unsafeDestroy (Device.D mdvc)
	(Group (AllocationCallbacks.toMiddle -> ma) sem bs) k = do
	mb <- atomically do
		mx <- Map.lookup k <$> readTVar bs
		case mx of
			Nothing -> pure Nothing
			Just _ -> waitTSem sem >> pure mx
	case mb of
		Nothing -> pure $ Left "Gp[u.Vulkan.BufferView.unsafeDestroy: No such key"
		Just (B b) -> do
			M.destroy mdvc b ma
			atomically do
				modifyTVar bs (Map.delete k)
				signalTSem sem
				pure $ Right ()

lookup :: Ord k => Group md sg k nm t -> k -> IO (Maybe (B sg nm t))
lookup (Group _ _sem bs) k = atomically $ Map.lookup k <$> readTVar bs
