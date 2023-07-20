{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import qualified Gpu.Vulkan.Memory as Vk.Mem

import Foreign.Storable
import Data.Kind
import Gpu.Vulkan.Object qualified as VObj
import Data.Default
import Data.Bits
import Data.Bits.Utils
import Data.List.Length
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Word
import System.Environment

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Utils as V

import Shaderc.TH
import Shaderc.EnumAuto
import Gpu.Vulkan.Misc

import qualified Gpu.Vulkan as Vk
import qualified "try-my-vulkan-snd" Gpu.Vulkan.Enum as Vk
import qualified Gpu.Vulkan.Instance as Vk.Inst
import qualified Gpu.Vulkan.PhysicalDevice as Vk.PhDvc
import qualified Gpu.Vulkan.PhysicalDevice.Struct as Vk.PhDvc
import qualified Gpu.Vulkan.Queue as Vk.Queue
import qualified Gpu.Vulkan.Queue.Enum as Vk.Queue
import qualified Gpu.Vulkan.QueueFamily as Vk.QFam
import qualified Gpu.Vulkan.Device as Vk.Dvc
import qualified Gpu.Vulkan.CommandPool as Vk.CmdPl
import qualified "try-my-vulkan-snd" Gpu.Vulkan.CommandPool.Enum as Vk.CmdPl
import qualified "try-my-vulkan-snd" Gpu.Vulkan.Buffer.Enum as Vk.Bffr
import qualified Gpu.Vulkan.Memory.Enum as Vk.Mem
import qualified Gpu.Vulkan.Memory.Middle as Vk.Mem.M
import qualified Gpu.Vulkan.Descriptor as Vk.Dsc
import qualified "try-my-vulkan-snd" Gpu.Vulkan.Descriptor.Enum as Vk.Dsc
import qualified Gpu.Vulkan.DescriptorPool as Vk.DscPool
import qualified "try-my-vulkan-snd" Gpu.Vulkan.DescriptorPool.Enum as Vk.DscPool
import qualified Gpu.Vulkan.ShaderModule as Vk.ShaderMod
import qualified "try-my-vulkan-snd" Gpu.Vulkan.Pipeline.Enum as Vk.Ppl
import qualified Gpu.Vulkan.PipelineLayout as Vk.Ppl.Lyt
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShaderSt
import qualified Gpu.Vulkan.Pipeline.Compute as Vk.Ppl.Cmpt
import qualified Gpu.Vulkan.DescriptorSet as Vk.DscSet
import qualified Gpu.Vulkan.CommandBuffer as Vk.CmdBuf
import qualified "try-my-vulkan-snd" Gpu.Vulkan.CommandBuffer.Enum as Vk.CmdBuf
import qualified Gpu.Vulkan.Cmd as Vk.Cmd

import qualified Gpu.Vulkan.Buffer as Vk.Bffr
import qualified Gpu.Vulkan.Memory as Vk.Dvc.Mem.ImgBffr
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscSetLyt

import qualified Gpu.Vulkan.Khr as Vk.Khr

import qualified Gpu.Vulkan.PushConstant as Vk.PushConstant

main :: IO ()
main = do
	args <- getArgs
	case args of
		[arg] -> do
			(r1, r2, r3) <- crtDevice \phdvc qFam dvc mxX ->
				let (da, db, dc) = mkData mxX in
					Vk.DscSetLyt.create dvc dscSetLayoutInfo
						nil' \dslyt ->
					prepDscSets arg phdvc dvc dslyt da db dc
						$ calc dvc qFam dslyt mxX
			print . take 20 $ unW1 <$> r1
			print . take 20 $ unW2 <$> r2
			print . take 20 $ unW3 <$> r3
		_ -> error "bad args"

newtype W1 = W1 { unW1 :: Word32 } deriving (Show, Storable)
newtype W2 = W2 { unW2 :: Word32 } deriving (Show, Storable)
newtype W3 = W3 { unW3 :: Word32 } deriving (Show, Storable)

type ListW1 =VObj.List 256 W1 ""
type ListW2 =VObj.List 256 W2 ""
type ListW3 =VObj.List 256 W3 ""

crtDevice :: (forall sd .
	Vk.PhDvc.P -> Vk.QFam.Index -> Vk.Dvc.D sd -> Word32 -> IO a) -> IO a
crtDevice f = Vk.Inst.create @_ @'Nothing instInfo nil' \inst -> do
	phdvc <- head <$> Vk.PhDvc.enumerate inst
	qf <- findQueueFamily phdvc Vk.Queue.ComputeBit
	lmts <- Vk.PhDvc.propertiesLimits <$> Vk.PhDvc.getProperties phdvc
	let	mxX :. _ = Vk.PhDvc.limitsMaxComputeWorkGroupCount lmts
	Vk.Dvc.create phdvc (dvcInfo qf) nil' $ \dvc ->
		f phdvc qf dvc mxX
	where
	instInfo :: Vk.Inst.CreateInfo 'Nothing 'Nothing
	instInfo = def {
		Vk.Inst.createInfoEnabledLayerNames =
			[Vk.Khr.validationLayerName] }
	dvcInfo qf = Vk.Dvc.CreateInfo {
		Vk.Dvc.createInfoNext = TMaybe.N,
		Vk.Dvc.createInfoFlags = zeroBits,
		Vk.Dvc.createInfoQueueCreateInfos = HeteroParList.Singleton $ queueInfo qf,
		Vk.Dvc.createInfoEnabledLayerNames =
			[Vk.Khr.validationLayerName],
		Vk.Dvc.createInfoEnabledExtensionNames = [],
		Vk.Dvc.createInfoEnabledFeatures = Nothing }
	queueInfo qf = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = zeroBits,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qf,
		Vk.Dvc.queueCreateInfoQueuePriorities = [0] }

findQueueFamily :: Vk.PhDvc.P -> Vk.Queue.FlagBits -> IO Vk.QFam.Index
findQueueFamily phdvc qb = (<$> Vk.PhDvc.getQueueFamilyProperties phdvc)
	$ fst . head . filter
		((/= zeroBits) . (.&. qb) . Vk.QFam.propertiesQueueFlags . snd)

mkData :: Word32 -> (V.Vector W1, V.Vector W2, V.Vector W3)
mkData n = (
	V.genericReplicate n $ W1 3,
	V.fromList $ W2 <$> [1 .. n],
	V.genericReplicate n $ W3 0 )

type DscSetLytLstW123 = '[
	'Vk.DscSetLyt.Buffer '[ListW1, ListW2, ListW3],
	'Vk.DscSetLyt.Buffer '[VObj.Atom 256 Word32 'Nothing] ]

dscSetLayoutInfo :: Vk.DscSetLyt.CreateInfo 'Nothing DscSetLytLstW123
dscSetLayoutInfo = Vk.DscSetLyt.CreateInfo {
	Vk.DscSetLyt.createInfoNext = TMaybe.N,
	Vk.DscSetLyt.createInfoFlags = zeroBits,
	Vk.DscSetLyt.createInfoBindings = bdng :** bdng :** HeteroParList.Nil }
	where bdng = Vk.DscSetLyt.BindingBuffer {
		Vk.DscSetLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeStorageBuffer,
		Vk.DscSetLyt.bindingBufferStageFlags =
			Vk.ShaderStageComputeBit }

prepDscSets ::
	String -> Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.DscSetLyt.L sl DscSetLytLstW123 ->
	V.Vector W1 -> V.Vector W2 -> V.Vector W3 -> (forall sds sm1 sm2 sm3 sb1 sb2 sb3 .
		Vk.DscSet.D sds '(sl, DscSetLytLstW123) ->
		Vk.Dvc.Mem.ImgBffr.M sm1 '[ '(sb1, 'Vk.Dvc.Mem.ImgBffr.BufferArg nm1 '[ListW1])] ->
		Vk.Dvc.Mem.ImgBffr.M sm2 '[ '(sb2, 'Vk.Dvc.Mem.ImgBffr.BufferArg nm2 '[ListW2])] ->
		Vk.Dvc.Mem.ImgBffr.M sm3 '[ '(sb3, 'Vk.Dvc.Mem.ImgBffr.BufferArg nm3 '[ListW3])] -> IO a) -> IO a
prepDscSets arg phdvc dvc dslyt da db dc f =
	Vk.DscPool.create dvc dscPoolInfo nil' \dp ->
	Vk.DscSet.allocateDs dvc (dscSetInfo dp dslyt) \(HeteroParList.Singleton ds) ->
	storageBufferNew3 phdvc dvc da db dc \(ba, ma) (bb, mb) (bc, mc) ->
	storageBufferNew3Objs @Word32
		@(VObj.Atom 256 Word32 ('Just "x0"))
		@(VObj.Atom 256 Word32 ('Just "x1"))
		@(VObj.Atom 256 Word32 ('Just "x2"))
		phdvc dvc 3 5 7 \bx _mx -> case arg of
			"0" -> do
				Vk.DscSet.updateDs dvc (
					U5 (writeDscSet ds ba bb bc) :**
					U5 (writeDscSet2 @"x0" ds bx) :**
					HeteroParList.Nil )
					HeteroParList.Nil
				f ds ma mb mc
			"1" -> do
				Vk.DscSet.updateDs dvc (
					U5 (writeDscSet ds ba bb bc) :**
					U5 (writeDscSet2 @"x1" ds bx) :**
					HeteroParList.Nil )
					HeteroParList.Nil
				f ds ma mb mc
			"2" -> do
				Vk.DscSet.updateDs dvc (
					U5 (writeDscSet ds ba bb bc) :**
					U5 (writeDscSet2 @"x2" ds bx) :**
					HeteroParList.Nil )
					HeteroParList.Nil
				f ds ma mb mc
			_ -> error "bad arg"

dscPoolInfo :: Vk.DscPool.CreateInfo 'Nothing
dscPoolInfo = Vk.DscPool.CreateInfo {
	Vk.DscPool.createInfoNext = TMaybe.N,
	Vk.DscPool.createInfoFlags = Vk.DscPool.CreateFreeDescriptorSetBit,
	Vk.DscPool.createInfoMaxSets = 1,
	Vk.DscPool.createInfoPoolSizes = [poolSize] }
	where poolSize = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeStorageBuffer,
		Vk.DscPool.sizeDescriptorCount = 10 }

dscSetInfo :: Vk.DscPool.P sp -> Vk.DscSetLyt.L sl DscSetLytLstW123 ->
	Vk.DscSet.AllocateInfo 'Nothing sp '[ '(sl, DscSetLytLstW123)]
dscSetInfo pl lyt = Vk.DscSet.AllocateInfo {
	Vk.DscSet.allocateInfoNext = TMaybe.N,
	Vk.DscSet.allocateInfoDescriptorPool = pl,
	Vk.DscSet.allocateInfoSetLayouts = U2 lyt :** HeteroParList.Nil }

type BffMem sm sb nm w = (
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 w ""],
	Vk.Dvc.Mem.ImgBffr.M sm '[ '(sb, 'Vk.Dvc.Mem.ImgBffr.BufferArg nm '[VObj.List 256 w ""])] )

storageBufferNew3 :: Vk.PhDvc.P -> Vk.Dvc.D sd ->
	V.Vector W1 -> V.Vector W2 -> V.Vector W3 -> (
		forall sb1 sm1 sb2 sm2 sb3 sm3 .
		BffMem sm1 sb1 nm1 W1 ->
		BffMem sm2 sb2 nm2 W2 ->
		BffMem sm3 sb3 nm3 W3 -> IO a ) -> IO a
storageBufferNew3 phdvc dvc x y z f =
	storageBufferNews phdvc dvc (x :** y :** z :** HeteroParList.Nil)
		$ Arg \b1 m1 -> Arg \b2 m2 -> Arg \b3 m3 ->
			f (b1, m1) (b2, m2) (b3, m3)

class StorageBufferNews f a where
	type Vectors f :: [Type]
	storageBufferNews :: Vk.PhDvc.P -> Vk.Dvc.D sd ->
		HeteroParList.PL V.Vector (Vectors f) -> f -> IO a

data Arg nm w f = Arg (forall sb sm .
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 w ""] ->
	Vk.Dvc.Mem.ImgBffr.M sm '[ '(sb, 'Vk.Dvc.Mem.ImgBffr.BufferArg nm '[VObj.List 256 w ""])] -> f)

instance StorageBufferNews (IO a) a where
	type Vectors (IO a) = '[]; storageBufferNews _phdvc _dvc HeteroParList.Nil f = f

instance (Storable w, StorageBufferNews f a) =>
	StorageBufferNews (Arg nm w f) a where
	type Vectors (Arg nm w f) = w ': Vectors f
	storageBufferNews phdvc dvc (vs :** vss) (Arg f) =
		storageBufferNew phdvc dvc vs \buf mem ->
		storageBufferNews @f @a phdvc dvc vss $ f buf mem

type KBuffer = 'Vk.Dvc.Mem.ImgBffr.BufferArg

storageBufferNew :: forall {sd} v {nm} obj {a} . (
	VObj.StoreObject v obj, VObj.SizeAlignment obj ) =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> v -> (forall sb sm .
		Vk.Bffr.Binded sm sb nm '[obj]  ->
		Vk.Dvc.Mem.ImgBffr.M sm '[ '(sb, KBuffer nm '[obj])] ->
		IO a) -> IO a
storageBufferNew phdvc dvc xs f =
	Vk.Bffr.create dvc (bufferInfo xs) nil' \bff -> do
		mi <- getMemoryInfo phdvc dvc bff
		Vk.Dvc.Mem.ImgBffr.allocateBind dvc (HeteroParList.Singleton . U2 $ Vk.Dvc.Mem.ImgBffr.Buffer bff) mi
			nil' \(HeteroParList.Singleton (U2 (Vk.Dvc.Mem.ImgBffr.BufferBinded bnd))) m -> do
			Vk.Dvc.Mem.ImgBffr.write @nm @obj dvc m zeroBits xs
			f bnd m

storageBufferNew3Objs :: forall {sd} v {nm} obj0 obj1 obj2 {a} . (
	VObj.StoreObject v obj0, VObj.SizeAlignment obj0,
	VObj.StoreObject v obj1, VObj.SizeAlignment obj1,
	VObj.StoreObject v obj2, VObj.SizeAlignment obj2,
	VObj.OffsetRange obj0 '[obj0, obj1, obj2],
	VObj.OffsetRange obj1 '[obj0, obj1, obj2],
	VObj.OffsetRange obj2 '[obj0, obj1, obj2],
	VObj.ObjectLengthOf obj0 '[obj0, obj1, obj2],
	VObj.ObjectLengthOf obj1 '[obj0, obj1, obj2],
	VObj.ObjectLengthOf obj2 '[obj0, obj1, obj2]
	) =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> v -> v -> v -> (forall sb sm .
		Vk.Bffr.Binded sm sb nm '[obj0, obj1, obj2]  ->
		Vk.Dvc.Mem.ImgBffr.M sm '[ '(sb, KBuffer nm '[obj0, obj1, obj2])] ->
		IO a) -> IO a
storageBufferNew3Objs phdvc dvc x y z f =
	Vk.Bffr.create dvc (bufferInfo' x y z) nil' \bff -> do
		mi <- getMemoryInfo phdvc dvc bff
		Vk.Dvc.Mem.ImgBffr.allocateBind dvc (HeteroParList.Singleton . U2 $ Vk.Dvc.Mem.ImgBffr.Buffer bff) mi
			nil' \(HeteroParList.Singleton (U2 (Vk.Dvc.Mem.ImgBffr.BufferBinded bnd))) m -> do
			Vk.Dvc.Mem.ImgBffr.write @nm @obj0 dvc m zeroBits x
			Vk.Dvc.Mem.ImgBffr.write @nm @obj1 dvc m zeroBits y
			Vk.Dvc.Mem.ImgBffr.write @nm @obj2 dvc m zeroBits z
			f bnd m

bufferInfo :: VObj.StoreObject v obj => v -> Vk.Bffr.CreateInfo 'Nothing '[obj]
bufferInfo xs = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N,
	Vk.Bffr.createInfoFlags = def,
	Vk.Bffr.createInfoLengths = HeteroParList.Singleton $ VObj.objectLength xs,
	Vk.Bffr.createInfoUsage = Vk.Bffr.UsageStorageBufferBit,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

bufferInfo' :: (
	VObj.StoreObject v obj0, VObj.StoreObject v obj1, VObj.StoreObject v obj2 ) =>
	v -> v -> v -> Vk.Bffr.CreateInfo 'Nothing '[obj0, obj1, obj2]
bufferInfo' x y z = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N,
	Vk.Bffr.createInfoFlags = def,
	Vk.Bffr.createInfoLengths =
		VObj.objectLength x :** VObj.objectLength y :**
		VObj.objectLength z :** HeteroParList.Nil,
	Vk.Bffr.createInfoUsage = Vk.Bffr.UsageStorageBufferBit,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

getMemoryInfo :: Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Bffr.B sb nm objs ->
	IO (Vk.Mem.AllocateInfo 'Nothing)
getMemoryInfo phdvc dvc buffer = do
	reqs <- Vk.Bffr.getMemoryRequirements dvc buffer
	mt <- findMemoryTypeIndex phdvc reqs (
		Vk.Mem.PropertyHostVisibleBit .|.
		Vk.Mem.PropertyHostCoherentBit )
	pure Vk.Mem.AllocateInfo {
		Vk.Mem.allocateInfoNext = TMaybe.N,
		Vk.Mem.allocateInfoMemoryTypeIndex = mt }

findMemoryTypeIndex ::
	Vk.PhDvc.P -> Vk.Mem.M.Requirements -> Vk.Mem.PropertyFlags ->
	IO Vk.Mem.M.TypeIndex
findMemoryTypeIndex phdvc reqs mprop = do
	mprops <- Vk.PhDvc.getMemoryProperties phdvc
	let	rqts = Vk.Mem.M.requirementsMemoryTypeBits reqs
		mpts = (fst <$>) . filter
			(checkBits mprop . Vk.Mem.M.mTypePropertyFlags . snd)
			$ Vk.PhDvc.memoryPropertiesMemoryTypes mprops
	case filter (`Vk.Mem.M.elemTypeIndex` rqts) mpts of
		i : _ -> pure i
		[] -> error "No available memory types"

writeDscSet :: forall sl sm1 sb1 nm1 sm2 sb2 nm2 sm3 sb3 nm3 sds .
	Vk.DscSet.D sds '(sl, DscSetLytLstW123) ->
	Vk.Bffr.Binded sm1 sb1 nm1 '[ListW1] ->
	Vk.Bffr.Binded sm2 sb2 nm2 '[ListW2] ->
	Vk.Bffr.Binded sm3 sb3 nm3 '[ListW3] ->
	Vk.DscSet.Write 'Nothing sds '(sl, DscSetLytLstW123) (
		'Vk.DscSet.WriteSourcesArgBuffer '[
			'(sm1, sb1, nm1, ListW1),
			'(sm2, sb2, nm2, ListW2),
			'(sm3, sb3, nm3, ListW3) ] ) 0
writeDscSet ds ba bb bc = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = ds,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		U4 (bil @W1 ba) :** U4 (bil @W2 bb) :** U4 (bil @W3 bc) :** HeteroParList.Nil }
	where
	bil :: forall t {sb} {sm} {nm} {objs} . (
		Show (HeteroParList.PL VObj.ObjectLength objs),
		VObj.OffsetRange (VObj.List 256 t "") objs ) =>
		Vk.Bffr.Binded sm sb nm objs ->
		Vk.Dsc.BufferInfo sm sb nm (VObj.List 256 t "")
	bil = Vk.Dsc.BufferInfo

writeDscSet2 :: forall nm objs sl sm4 sb4 nm4 sds . (
	Show (HeteroParList.PL VObj.ObjectLength objs),
	VObj.OffsetRange (VObj.Atom 256 Word32 ('Just nm)) objs ) =>
	Vk.DscSet.D sds '(sl, DscSetLytLstW123) ->
	Vk.Bffr.Binded sm4 sb4 nm4 objs ->
	Vk.DscSet.Write 'Nothing sds '(sl, DscSetLytLstW123) (
		'Vk.DscSet.WriteSourcesArgBuffer '[
			'(sm4, sb4, nm4, VObj.Atom 256 Word32 ('Just nm)) ] ) 0
writeDscSet2 ds bx = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = ds,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		U4 (Vk.Dsc.BufferInfo bx) :** HeteroParList.Nil }

calc :: Vk.Dvc.D sd -> Vk.QFam.Index -> Vk.DscSetLyt.L sl DscSetLytLstW123 ->
	Word32 -> Vk.DscSet.D sds '(sl, DscSetLytLstW123) ->
	Vk.Dvc.Mem.ImgBffr.M sm1 '[ '(sb1, 'Vk.Dvc.Mem.ImgBffr.BufferArg nm1 '[ListW1])] ->
	Vk.Dvc.Mem.ImgBffr.M sm2 '[ '(sb2, 'Vk.Dvc.Mem.ImgBffr.BufferArg nm2 '[ListW2])] ->
	Vk.Dvc.Mem.ImgBffr.M sm3 '[ '(sb3, 'Vk.Dvc.Mem.ImgBffr.BufferArg nm3 '[ListW3])] -> IO ([W1], [W2], [W3])
calc dvc qFam dslyt ln dss ma mb mc =
	Vk.Ppl.Lyt.createNew dvc (pplLayoutInfoNew dslyt) nil' \plyt ->
	Vk.Ppl.Cmpt.createCsNew
		dvc Nothing
		(HeteroParList.Singleton . U4 $ computePipelineInfo plyt)
		nil' \(ppl :** HeteroParList.Nil) ->
	Vk.CmdPl.create dvc (commandPoolInfo qFam) nil' \cp ->
	Vk.CmdBuf.allocate dvc (commandBufferInfo cp) \(cmdBuf :*. HeteroParList.Nil) ->
		run dvc qFam cmdBuf ppl plyt dss ln ma mb mc

pplLayoutInfoNew :: Vk.DscSetLyt.L sl DscSetLytLstW123 ->
	Vk.Ppl.Lyt.CreateInfoNew 'Nothing '[ '(sl, DscSetLytLstW123)]
		('Vk.PushConstant.PushConstantLayout '[] '[])
pplLayoutInfoNew dslyt = Vk.Ppl.Lyt.CreateInfoNew {
	Vk.Ppl.Lyt.createInfoNextNew = TMaybe.N,
	Vk.Ppl.Lyt.createInfoFlagsNew = zeroBits,
	Vk.Ppl.Lyt.createInfoSetLayoutsNew = HeteroParList.Singleton $ U2 dslyt }

computePipelineInfo :: Vk.Ppl.Lyt.L sl '[ '(sdsl, DscSetLytLstW123)] '[] ->
	Vk.Ppl.Cmpt.CreateInfo 'Nothing '( 'Nothing, 'Nothing, 'GlslComputeShader, 'Nothing,
		'[Word32, Word32]) '(sl, '[ '(sdsl, DscSetLytLstW123)], '[]) sbph
computePipelineInfo pl = Vk.Ppl.Cmpt.CreateInfo {
	Vk.Ppl.Cmpt.createInfoNext = TMaybe.N,
	Vk.Ppl.Cmpt.createInfoFlags = zeroBits,
	Vk.Ppl.Cmpt.createInfoStage = U5 shaderStageInfo,
	Vk.Ppl.Cmpt.createInfoLayout = U3 pl,
	Vk.Ppl.Cmpt.createInfoBasePipelineHandleOrIndex = Nothing }

shaderStageInfo :: Vk.Ppl.ShaderSt.CreateInfoNew
	'Nothing 'Nothing 'GlslComputeShader 'Nothing '[Word32, Word32]
shaderStageInfo = Vk.Ppl.ShaderSt.CreateInfoNew {
	Vk.Ppl.ShaderSt.createInfoNextNew = TMaybe.N,
	Vk.Ppl.ShaderSt.createInfoFlagsNew = zeroBits,
	Vk.Ppl.ShaderSt.createInfoStageNew = Vk.ShaderStageComputeBit,
	Vk.Ppl.ShaderSt.createInfoModuleNew = Vk.ShaderMod.M shaderModInfo nil',
	Vk.Ppl.ShaderSt.createInfoNameNew = "main",
	Vk.Ppl.ShaderSt.createInfoSpecializationInfoNew =
		Just $ HeteroParList.Id 3 :** HeteroParList.Id 10 :** HeteroParList.Nil }
	where shaderModInfo = Vk.ShaderMod.CreateInfo {
		Vk.ShaderMod.createInfoNext = TMaybe.N,
		Vk.ShaderMod.createInfoFlags = zeroBits,
		Vk.ShaderMod.createInfoCode = glslComputeShaderMain }

commandPoolInfo :: Vk.QFam.Index -> Vk.CmdPl.CreateInfo 'Nothing
commandPoolInfo qFam = Vk.CmdPl.CreateInfo {
	Vk.CmdPl.createInfoNext = TMaybe.N,
	Vk.CmdPl.createInfoFlags = Vk.CmdPl.CreateResetCommandBufferBit,
	Vk.CmdPl.createInfoQueueFamilyIndex = qFam }

commandBufferInfo :: Vk.CmdPl.C s -> Vk.CmdBuf.AllocateInfo 'Nothing s '[ '()]
commandBufferInfo cmdPool = Vk.CmdBuf.AllocateInfo {
	Vk.CmdBuf.allocateInfoNext = TMaybe.N,
	Vk.CmdBuf.allocateInfoCommandPool = cmdPool,
	Vk.CmdBuf.allocateInfoLevel = Vk.CmdBuf.LevelPrimary }

run :: forall sd sc sg sl sdsl sm1 sb1 nm1 sm2 sb2 nm2 sm3 sb3 nm3 sds .
	Vk.Dvc.D sd -> Vk.QFam.Index -> Vk.CmdBuf.C sc ->
	Vk.Ppl.Cmpt.C sg '(sl, '[ '(sdsl, DscSetLytLstW123)], '[]) ->
	Vk.Ppl.Lyt.L sl '[ '(sdsl, DscSetLytLstW123)] '[] ->
	Vk.DscSet.D sds '(sdsl, DscSetLytLstW123)  -> Word32 ->
	Vk.Dvc.Mem.ImgBffr.M sm1 '[ '(sb1, 'Vk.Dvc.Mem.ImgBffr.BufferArg nm1 '[ListW1])] ->
	Vk.Dvc.Mem.ImgBffr.M sm2 '[ '(sb2, 'Vk.Dvc.Mem.ImgBffr.BufferArg nm2 '[ListW2])] ->
	Vk.Dvc.Mem.ImgBffr.M sm3 '[ '(sb3, 'Vk.Dvc.Mem.ImgBffr.BufferArg nm3 '[ListW3])] ->
	IO ([W1], [W2], [W3])
run dvc qf cb ppl plyt dss ln ma mb mc = Vk.Dvc.getQueue dvc qf 0 >>= \q -> do
	Vk.CmdBuf.begin @'Nothing @'Nothing cb def $
		Vk.Cmd.bindPipelineCompute cb Vk.Ppl.BindPointCompute ppl \ccb -> do
			Vk.Cmd.bindDescriptorSetsCompute ccb plyt
				(HeteroParList.Singleton $ U2 dss)
				(HeteroParList.Singleton $
					HeteroParList.Nil :** HeteroParList.Nil :**
					HeteroParList.Nil )
			Vk.Cmd.dispatch ccb ln 1 1
	Vk.Queue.submit q (U4 sinfo :** HeteroParList.Nil) Nothing
	Vk.Queue.waitIdle q
	(,,)	<$> Vk.Dvc.Mem.ImgBffr.read @nm1 @ListW1 @[W1] dvc ma zeroBits
		<*> Vk.Dvc.Mem.ImgBffr.read @nm2 @ListW2 @[W2] dvc mb zeroBits
		<*> Vk.Dvc.Mem.ImgBffr.read @nm3 @ListW3 @[W3] dvc mc zeroBits
	where	sinfo :: Vk.SubmitInfo 'Nothing _ _ _
		sinfo = Vk.SubmitInfo {
			Vk.submitInfoNext = TMaybe.N,
			Vk.submitInfoWaitSemaphoreDstStageMasks = HeteroParList.Nil,
			Vk.submitInfoCommandBuffers = cb :** HeteroParList.Nil,
			Vk.submitInfoSignalSemaphores = HeteroParList.Nil }

[glslComputeShader|

#version 460
layout(local_size_x = 1, local_size_y = 1) in;
layout(binding = 0) buffer Data {
	uint val[];
} data[3];

layout(binding = 1) buffer Foo {
	uint x;
} x;

layout(constant_id = 0) const uint sc = 2;
layout(constant_id = 1) const uint sc2 = 3;

void
main()
{
	int index = int(gl_GlobalInvocationID.x);
	data[2].val[index] =
		(data[0].val[index] + data[1].val[index]) * sc * sc2 + x.x;
}

|]
