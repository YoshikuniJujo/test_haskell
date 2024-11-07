{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet (

	-- * ALLOCATE

	allocateDs, D, AllocateInfo(..), DListFromMiddle, DefaultDynamicLengths,

	-- ** Descriptor Set Group

	Group, group, allocateDs', unsafeFreeDs, lookup,

	-- * UPDATE

	updateDs,

	-- ** Write

	W.Write(..), W.WriteListToMiddle,
	W.WriteListUpdateDynamicLengths, W.UpdateDynamicLength,
	W.WriteSources(..), W.WriteSourcesArg(..),
	W.WriteSourcesToMiddle,

	-- ** Copy

	Copy(..), CopyListToMiddle,

	-- ** BindingAndArrayElem

	BindingAndArrayElem, W.BindingAndArrayElemImage,
	W.BindingAndArrayElemImageWithImmutableSampler,
	W.BindingAndArrayElemBuffer, W.BindingAndArrayElemBufferView

	) where

import Prelude hiding (lookup)

import Foreign.Storable.PeekPoke
import Data.Default
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.Index qualified as TIndex
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:**))

import Gpu.Vulkan.DescriptorSet.Type

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.DescriptorPool.Type as Descriptor.Pool
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Layout
import qualified Gpu.Vulkan.DescriptorSetLayout.Middle as Layout.M
import qualified Gpu.Vulkan.DescriptorSet.Write as W
import qualified Gpu.Vulkan.DescriptorSet.Middle as M

import Gpu.Vulkan.DescriptorSet.Copy

import Gpu.Vulkan.Object.Base qualified as KObj

import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Data.Map qualified as Map

import Data.IORef.ToolsYj

import Data.TypeLevel.Tuple.MapIndex qualified

import Control.Monad
import Debug

layoutToMiddle :: U2 Layout.D slbts -> Layout.M.D
layoutToMiddle (U2 (Layout.D l)) = l

data AllocateInfo mn sp slbtss = AllocateInfo {
	allocateInfoNext :: TMaybe.M mn,
	allocateInfoDescriptorPool :: Descriptor.Pool.P sp,
	allocateInfoSetLayouts :: HeteroParList.PL (U2 Layout.D) slbtss }

deriving instance (Show (TMaybe.M n), Show (HeteroParList.PL (U2 Layout.D) slbtss)) =>
	Show (AllocateInfo n sp slbtss)

allocateInfoToMiddle :: AllocateInfo n sp slbtss -> M.AllocateInfo n
allocateInfoToMiddle AllocateInfo {
	allocateInfoNext = mnxt,
	allocateInfoDescriptorPool = Descriptor.Pool.P dp,
	allocateInfoSetLayouts = dscsls
	} = M.AllocateInfo {
		M.allocateInfoNext = mnxt,
		M.allocateInfoDescriptorPool = dp,
		M.allocateInfoSetLayouts =
			HeteroParList.toList layoutToMiddle dscsls }

class DListFromMiddle slbtss where
	dListFromMiddle :: [M.D] -> IO (HeteroParList.PL (D s) slbtss)

instance DListFromMiddle '[] where
	dListFromMiddle = \case [] -> pure HeteroParList.Nil; _ -> error "bad"

instance (
	DefaultDynamicLengths slbts,
	DListFromMiddle slbtss ) =>
	DListFromMiddle (slbts ': slbtss) where
	dListFromMiddle = \case
		(d : ds) -> (:**)
			<$> ((`D` d) <$> newDefaultIORef)
			<*> dListFromMiddle @slbtss ds
		_ -> error "bad"

type DefaultDynamicLengths slbts = Default
	(HeteroParList.PL
		(HeteroParList.PL KObj.Length)
		(Layout.BindingTypeListBufferOnlyDynamics (TIndex.I1_2 slbts)))

allocateDs :: (WithPoked (TMaybe.M mn), DListFromMiddle slbtss) =>
	Device.D sd -> AllocateInfo mn sp slbtss ->
	(forall s . HeteroParList.PL (D s) slbtss -> IO a) -> IO a
allocateDs (Device.D dvc) ai f = do
	dsm <- M.allocateDs dvc (allocateInfoToMiddle ai)
	ds <- dListFromMiddle dsm
	f ds <* M.freeDs dvc
		((\(Descriptor.Pool.P p) -> p) $ allocateInfoDescriptorPool ai)
		dsm

data Group sd s k sp slbtss = Group (Device.D sd) TSem
	(TVar (Map.Map k (Descriptor.Pool.P sp, HeteroParList.PL (D s) slbtss)))

group :: Device.D sd -> (forall s . Group sd s k sp slbtss -> IO a) -> IO a
group dv@(Device.D mdvc) f = do
	(sem, dsss) <- atomically $ (,) <$> newTSem 1 <*> newTVar Map.empty
	rtn <- f $ Group dv sem dsss
	((\(Descriptor.Pool.P dsp, dss) -> M.freeDs mdvc dsp $ dListToMiddle dss) `mapM_`) =<<
		(Map.elems <$> atomically (readTVar dsss))
	pure rtn

allocateDs' :: (Ord k, WithPoked (TMaybe.M mn), DListFromMiddle slbtss) =>
	Group sd sg k sp slbtss -> k -> AllocateInfo mn sp slbtss ->
	IO (Either String (HeteroParList.PL (D sg) slbtss))
allocateDs' (Group (Device.D dvc) sem mp) k ai = do
	ok <- atomically do
		mx <- (Map.lookup k) <$> readTVar mp
		case mx of
			Nothing -> waitTSem sem >> pure True
			Just _ -> pure False
	if ok
	then do	dsm <- M.allocateDs dvc (allocateInfoToMiddle ai)
		ds <- dListFromMiddle dsm
		atomically do
			modifyTVar mp (Map.insert k (sp, ds))
			signalTSem sem
		pure $ Right ds
	else pure . Left
		$ "Gpu.Vulkan.DescriptorSet.allocateDs': The key already exist"
--	where Descriptor.Pool.P sp  = allocateInfoDescriptorPool ai
	where sp  = allocateInfoDescriptorPool ai

unsafeFreeDs :: Ord k => Group sd sg k sp slbtss -> k -> IO (Either String ())
unsafeFreeDs (Group (Device.D mdvc) sem mp) k = do
	md <- atomically do
		mx <- Map.lookup k <$> readTVar mp
		case mx of
			Nothing -> pure Nothing
			Just _ -> waitTSem sem >> pure mx
	case md of
		Nothing -> pure $ Left "Gpu.Vulkan.DescriptorSet.unsafeFreeDs"
		Just (Descriptor.Pool.P p, ds) -> do
			M.freeDs mdvc p (dListToMiddle ds)
			atomically do
				modifyTVar mp (Map.delete k)
				signalTSem sem
				pure $ Right ()

lookup :: Ord k =>
	Group sd s k sp slbtss -> k -> IO (Maybe (HeteroParList.PL (D s) slbtss))
lookup (Group _ _sem mp) k = atomically $ (snd <$>) . Map.lookup k <$> readTVar mp

dListToMiddle :: HeteroParList.PL (D s) slbtss -> [M.D]
dListToMiddle = HeteroParList.toList \(D _ md) -> md

updateDs :: (
	Show (HeteroParList.PL M.Write (Data.TypeLevel.Tuple.MapIndex.M0_5 writeArgs)),
	W.WriteListToMiddle writeArgs,
	W.WriteListUpdateDynamicLengths writeArgs,
	CopyListToMiddle copyArgs) =>
	Device.D sd ->
	HeteroParList.PL (U5 W.Write) writeArgs ->
	HeteroParList.PL (U8 Copy) copyArgs  -> IO ()
updateDs (Device.D dvc) ws cs = when debug (print ws') >>
	W.writeListUpdateDynamicLength ws >> M.updateDs dvc ws' cs'
	where ws' = W.writeListToMiddle ws; cs' = copyListToMiddle cs
