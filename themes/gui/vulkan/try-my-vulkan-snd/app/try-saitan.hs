{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.Storable
import Data.Kind
import Data.Kind.Object
import Data.Default
import Data.Bits
import Data.HeteroList
import Data.Word

import qualified Data.Vector.Storable as V

import Shaderc.TH
import Shaderc.EnumAuto
import Gpu.Vulkan.Base

import qualified Gpu.Vulkan as Vk
import qualified Gpu.Vulkan.Enum as Vk
import qualified Gpu.Vulkan.Instance as Vk.Inst
import qualified Gpu.Vulkan.PhysicalDevice as Vk.PhDvc
import qualified Gpu.Vulkan.Queue as Vk.Queue
import qualified Gpu.Vulkan.Queue.Enum as Vk.Queue
import qualified Gpu.Vulkan.QueueFamily as Vk.QFam
import qualified Gpu.Vulkan.QueueFamily.EnumManual as Vk.QFam
import qualified Gpu.Vulkan.Device.Queue as Vk.Dvc.Queue
import qualified Gpu.Vulkan.Device as Vk.Dvc
import qualified Gpu.Vulkan.CommandPool as Vk.CommandPool
import qualified Gpu.Vulkan.CommandPool.Enum as Vk.CommandPool
import qualified Gpu.Vulkan.Buffer.Enum as Vk.Buffer
import qualified Gpu.Vulkan.Memory as Vk.Memory
import qualified Gpu.Vulkan.Memory.Enum as Vk.Memory
import qualified Gpu.Vulkan.Memory.Middle as Vk.Memory.M
import qualified Gpu.Vulkan.Descriptor as Vk.Dsc
import qualified Gpu.Vulkan.Descriptor.Enum as Vk.Dsc
import qualified Gpu.Vulkan.DescriptorPool as Vk.DscPool
import qualified Gpu.Vulkan.DescriptorPool.Enum as Vk.DscPool
import qualified Gpu.Vulkan.ShaderModule as Vk.ShaderMod
import qualified Gpu.Vulkan.Pipeline.Enum as Vk.Ppl
import qualified Gpu.Vulkan.Pipeline.Layout as Vk.Ppl.Lyt
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShaderSt
import qualified Gpu.Vulkan.Pipeline.Compute as Vk.Ppl.Cmpt
import qualified Gpu.Vulkan.DescriptorSet as Vk.DscSet
import qualified Gpu.Vulkan.DescriptorSet.TypeLevel as Vk.DscSet
import qualified Gpu.Vulkan.CommandBuffer as Vk.CmdBuf
import qualified Gpu.Vulkan.CommandBuffer.Enum as Vk.CmdBuf
import qualified Gpu.Vulkan.Command as Vk.Cmd
import qualified Gpu.Vulkan.Command.TypeLevel as Vk.Cmd

import qualified Gpu.Vulkan.Buffer as Vk.Buffer
import qualified Gpu.Vulkan.Device.Memory.Buffer as Vk.Dvc.Memory.Buffer
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscSetLyt
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Vk.DscSetLyt
import qualified Gpu.Vulkan.Device.Memory.Buffer.TypeLevel as Vk.Device.Memory.Buffer

main :: IO ()
main = do
	(r1, r2, r3) <- calc dataSize datA datB datC
	print . take 20 $ unW1 <$> r1
	print . take 20 $ unW2 <$> r2
	print . take 20 $ unW3 <$> r3

calc :: Word32 ->
	V.Vector W1 -> V.Vector W2 -> V.Vector W3 -> IO ([W1], [W2], [W3])
calc dsz da db dc = withDevice \phdvc qFam dvc ->
	Vk.DscSetLyt.create dvc dscSetLayoutInfo nil nil \dscSetLyt ->

--	prepareMems phdvc dvc dscSetLyt da db dc \(dscSet, ma, mb, mc) ->
	prepareMems' phdvc dvc dscSetLyt da db dc \(dscSet, ma, mb, mc) ->

	Vk.Ppl.Lyt.create dvc (pplLayoutInfo dscSetLyt) nil nil \pplLyt ->
	Vk.Ppl.Cmpt.createCs @'[ '((), _, _, _)] dvc Nothing
		(Vk.Ppl.Cmpt.CreateInfo_
			(computePipelineInfo pplLyt) :...: HVNil)
		nil nil \(Vk.Ppl.Cmpt.Pipeline ppl :...: HVNil) ->
	Vk.CommandPool.create dvc (commandPoolInfo qFam) nil nil \cmdPool ->
	Vk.CmdBuf.allocate dvc (commandBufferInfo cmdPool) \case
		[cmdBuf] -> run dvc qFam cmdBuf ppl pplLyt dscSet dsz ma mb mc
		_ -> error "never occur"

run :: (
	Vk.Device.Memory.Buffer.OffsetSize ('List W1) objss1,
	Vk.Device.Memory.Buffer.OffsetSize ('List W2) objss2,
	Vk.Device.Memory.Buffer.OffsetSize ('List W3) objss3,
	Vk.Cmd.SetPos '[slbts] sbtss ) =>
	Vk.Dvc.D sd -> Vk.QFam.Index -> Vk.CmdBuf.C sc vs -> Vk.Ppl.Cmpt.C sg ->
	Vk.Ppl.Lyt.LL sl sbtss -> Vk.DscSet.S sd sp slbts -> Word32 ->
	Vk.Dvc.Memory.Buffer.M sm1 objss1 ->
	Vk.Dvc.Memory.Buffer.M sm2 objss2 ->
	Vk.Dvc.Memory.Buffer.M sm3 objss3 -> IO ([W1], [W2], [W3])
run dvc qFam cmdBuf ppl pplLyt dscSet dsz memA memB memC = do
	queue <- Vk.Dvc.getQueue dvc qFam 0
	Vk.CmdBuf.begin @() @() cmdBuf def do
		Vk.Cmd.bindPipelineCompute cmdBuf Vk.Ppl.BindPointCompute ppl
		Vk.Cmd.bindDescriptorSets cmdBuf Vk.Ppl.BindPointCompute pplLyt
			(Vk.Cmd.DescriptorSet dscSet :...: HVNil) []
		Vk.Cmd.dispatch cmdBuf dsz 1 1
	Vk.Queue.submit @() queue [submitInfo] Nothing
	Vk.Queue.waitIdle queue
	(,,)	<$> Vk.Dvc.Memory.Buffer.read @[W1] @('List W1) dvc memA def
		<*> Vk.Dvc.Memory.Buffer.read @[W2] @('List W2) dvc memB def
		<*> Vk.Dvc.Memory.Buffer.read @[W3] @('List W3) dvc memC def
	where	submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = Nothing,
			Vk.submitInfoWaitSemaphoreDstStageMasks = [],
			Vk.submitInfoCommandBuffers = [cmdBuf],
			Vk.submitInfoSignalSemaphores = [] }

withDevice ::
	(forall sd . Vk.PhDvc.P -> Vk.QFam.Index -> Vk.Dvc.D sd -> IO a) -> IO a
withDevice f = Vk.Inst.create @() @() def nil nil \inst -> do
	phdvc <- head <$> Vk.PhDvc.enumerate inst
	qFam <- findQueueFamily phdvc Vk.Queue.ComputeBit
	Vk.Dvc.create @() @() phdvc (dvcInfo qFam) nil nil $ f phdvc qFam
	where
	dvcInfo qFam = Vk.Dvc.CreateInfo {
		Vk.Dvc.createInfoNext = Nothing,
		Vk.Dvc.createInfoFlags = def,
		Vk.Dvc.createInfoQueueCreateInfos = [queueInfo qFam],
		Vk.Dvc.createInfoEnabledLayerNames = [],
		Vk.Dvc.createInfoEnabledExtensionNames = [],
		Vk.Dvc.createInfoEnabledFeatures = Nothing }
	queueInfo qFam = Vk.Dvc.Queue.CreateInfo {
		Vk.Dvc.Queue.createInfoNext = Nothing,
		Vk.Dvc.Queue.createInfoFlags = def,
		Vk.Dvc.Queue.createInfoQueueFamilyIndex = qFam,
		Vk.Dvc.Queue.createInfoQueuePriorities = [0] }

findQueueFamily ::
	Vk.PhDvc.P -> Vk.Queue.FlagBits -> IO Vk.QFam.Index
findQueueFamily phdvc qb = do
	qFamProperties <- Vk.PhDvc.getQueueFamilyProperties phdvc
	pure . fst . head $ filter ((/= zeroBits)
			. (.&. qb) . Vk.QFam.propertiesQueueFlags . snd)
		qFamProperties

dscSetLayoutInfo :: Vk.DscSetLyt.CreateInfo ()
	'[ 'Vk.DscSetLyt.Buffer '[ 'List W1, 'List W2, 'List W3]]
dscSetLayoutInfo = Vk.DscSetLyt.CreateInfo {
	Vk.DscSetLyt.createInfoNext = Nothing,
	Vk.DscSetLyt.createInfoFlags = def,
	Vk.DscSetLyt.createInfoBindings = binding0 :...: HVNil }

binding0 :: Vk.DscSetLyt.Binding ('Vk.DscSetLyt.Buffer objs)
binding0 = Vk.DscSetLyt.BindingBuffer {
	Vk.DscSetLyt.bindingBufferDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSetLyt.bindingBufferStageFlags = Vk.ShaderStageComputeBit }

prepareMems ::
	Vk.DscSet.BindingAndArrayElem bts '[ 'List W1, 'List W2, 'List W3] =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.DscSetLyt.L sl bts ->
	V.Vector W1 -> V.Vector W2 -> V.Vector W3 -> (forall s sm1 sm2 sm3 . (
		Vk.DscSet.S sd s '(sl, bts),
		Vk.Dvc.Memory.Buffer.M sm1 '[ '[ 'List W1]],
		Vk.Dvc.Memory.Buffer.M sm2 '[ '[ 'List W2]],
		Vk.Dvc.Memory.Buffer.M sm3 '[ '[ 'List W3]] ) -> IO a) -> IO a
prepareMems phdvc dvc dscSetLyt da db dc f =
	Vk.DscPool.create dvc dscPoolInfo nil nil \dscPool ->
	Vk.DscSet.allocateSs dvc (dscSetInfo dscPool dscSetLyt)
		>>= \(dscSet :...: HVNil) ->
	storageBufferNew3' dvc phdvc da db dc \ba ma bb mb bc mc ->
	Vk.DscSet.updateDs @() @() dvc (Vk.DscSet.Write_
		(writeDscSet dscSet ba bb bc) :...: HVNil) [] >>
	f (dscSet, ma, mb, mc)

prepareMems' ::
	Vk.DscSet.BindingAndArrayElem bts '[ 'List W1, 'List W2, 'List W3] =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.DscSetLyt.L sl bts ->
	V.Vector W1 -> V.Vector W2 -> V.Vector W3 -> (forall s sm1 sm2 sm3 . (
		Vk.DscSet.S sd s '(sl, bts),
		Vk.Dvc.Memory.Buffer.M sm1 '[ '[ 'List W1], '[ 'List W2], '[ 'List W3]],
		Vk.Dvc.Memory.Buffer.M sm2 '[ '[ 'List W1], '[ 'List W2], '[ 'List W3]],
		Vk.Dvc.Memory.Buffer.M sm3 '[ '[ 'List W1], '[ 'List W2], '[ 'List W3]] ) -> IO a) -> IO a
prepareMems' phdvc dvc dscSetLyt da db dc f =
	Vk.DscPool.create dvc dscPoolInfo nil nil \dscPool ->
	Vk.DscSet.allocateSs dvc (dscSetInfo dscPool dscSetLyt)
		>>= \(dscSet :...: HVNil) ->
	storage3BufferNew dvc phdvc da db dc \ba bb bc m ->
	Vk.DscSet.updateDs @() @() dvc (Vk.DscSet.Write_
		(writeDscSet dscSet ba bb bc) :...: HVNil) [] >>
	f (dscSet, m, m, m)

storageBufferNew3' :: (Storable w1, Storable w2, Storable w3) =>
	Vk.Dvc.D sd -> Vk.PhDvc.P ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (
		forall sb1 sm1 sb2 sm2 sb3 sm3 .
		Vk.Buffer.Binded sb1 sm1 '[ 'List w1] ->
		Vk.Dvc.Memory.Buffer.M sm1 '[ '[ 'List w1]] ->
		Vk.Buffer.Binded sb2 sm2 '[ 'List w2] ->
		Vk.Dvc.Memory.Buffer.M sm2 '[ '[ 'List w2]] ->
		Vk.Buffer.Binded sb3 sm3 '[ 'List w3] ->
		Vk.Dvc.Memory.Buffer.M sm3 '[ '[ 'List w3]] -> IO a ) -> IO a
storageBufferNew3' dvc phdvc x y z f =
	storageBufferNews dvc phdvc (x :...: y :...: z :...: HVNil) $ addArg3 f

addArg3 :: (forall sb1 sm1 sb2 sm2 sb3 sm3 .
	Vk.Buffer.Binded sb1 sm1 '[ 'List w1] ->
	Vk.Dvc.Memory.Buffer.M sm1 '[ '[ 'List w1]] ->
	Vk.Buffer.Binded sb2 sm2 '[ 'List w2] ->
	Vk.Dvc.Memory.Buffer.M sm2 '[ '[ 'List w2]] ->
	Vk.Buffer.Binded sb3 sm3 '[ 'List w3] ->
	Vk.Dvc.Memory.Buffer.M sm3 '[ '[ 'List w3]] -> r) ->
	Arg w1 (Arg w2 (Arg w3 r))
addArg3 f = Arg \b1 m1 -> Arg \b2 m2 -> Arg \b3 m3 -> f b1 m1 b2 m2 b3 m3

class StorageBufferNews f a where
	type Vectors f :: [Type]
	storageBufferNews :: Vk.Dvc.D sd -> Vk.PhDvc.P ->
		HeteroVarList V.Vector (Vectors f) -> f -> IO a

data Arg w f = Arg (forall sb sm .
	Vk.Buffer.Binded sb sm '[ 'List w] ->
	Vk.Dvc.Memory.Buffer.M sm '[ '[ 'List w]] -> f)

instance StorageBufferNews (IO a) a where
	type Vectors (IO a) = '[]
	storageBufferNews _dvc _phdvc HVNil f = f

instance (Storable w, StorageBufferNews f a) =>
	StorageBufferNews (Arg w f) a where
	type Vectors (Arg w f) = w ': Vectors f
	storageBufferNews dvc phdvc (vs :...: vss) (Arg f) =
		storageBufferNew dvc phdvc vs \buf mem ->
		storageBufferNews @f @a dvc phdvc vss (f buf mem)

storageBufferNew :: forall sd w a . Storable w =>
	Vk.Dvc.D sd -> Vk.PhDvc.P -> V.Vector w -> (
		forall sb sm .
		Vk.Buffer.Binded sb sm '[ 'List w]  ->
		Vk.Dvc.Memory.Buffer.M sm '[ '[ 'List w]] -> IO a ) -> IO a
storageBufferNew dvc phdvc xs f =
	Vk.Buffer.create dvc (bufferInfo xs) nil nil \buffer -> do
		memoryInfo <- getMemoryInfo phdvc dvc (Vk.Buffer.BB buffer)
		Vk.Buffer.allocateBind dvc (Vk.Buffer.BB buffer :...: HVNil) memoryInfo
			nil nil \(Vk.Buffer.Bnd binded :...: HVNil) memory -> do
			Vk.Dvc.Memory.Buffer.write @('List w) dvc memory def xs
			f binded memory

storage3BufferNew :: forall sd w1 w2 w3 a . (
	Storable w1, Storable w2, Storable w3,
	Vk.Device.Memory.Buffer.OffsetSize ('List w2) '[ '[ 'List w1], '[ 'List w2], '[ 'List w3]],
	Vk.Device.Memory.Buffer.OffsetSize ('List w3) '[ '[ 'List w1], '[ 'List w2], '[ 'List w3]]
	) =>
	Vk.Dvc.D sd -> Vk.PhDvc.P ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (
		forall sb1 sb2 sb3 sm .
		Vk.Buffer.Binded sb1 sm '[ 'List w1] ->
		Vk.Buffer.Binded sb2 sm '[ 'List w2] ->
		Vk.Buffer.Binded sb3 sm '[ 'List w3] ->
		Vk.Dvc.Memory.Buffer.M sm
			'[ '[ 'List w1], '[ 'List w2], '[ 'List w3]] -> IO a
		) -> IO a
storage3BufferNew dvc phdvc xs ys zs f =
	Vk.Buffer.create dvc (bufferInfo xs) nil nil \buf1 -> do
		memInfo1 <- getMemoryInfo phdvc dvc $ Vk.Buffer.BB buf1
		Vk.Buffer.create dvc (bufferInfo ys) nil nil \buf2 -> do
			memInfo2 <- getMemoryInfo phdvc dvc $ Vk.Buffer.BB buf2
			Vk.Buffer.create dvc (bufferInfo zs) nil nil \buf3 -> do
				memInfo3 <- getMemoryInfo phdvc dvc $ Vk.Buffer.BB buf3
				if (memInfo1 == memInfo2 && memInfo2 == memInfo3) then
					Vk.Buffer.allocateBind dvc (
						Vk.Buffer.BB buf1 :...:
						Vk.Buffer.BB buf2 :...:
						Vk.Buffer.BB buf3 :...: HVNil
						) memInfo1 nil nil
						\(	Vk.Buffer.Bnd bnd1 :...:
							Vk.Buffer.Bnd bnd2 :...:
							Vk.Buffer.Bnd bnd3 :...: HVNil ) mem -> do
						Vk.Dvc.Memory.Buffer.write @('List w1) dvc mem def xs
						Vk.Dvc.Memory.Buffer.write @('List w2) dvc mem def ys
						Vk.Dvc.Memory.Buffer.write @('List w3) dvc mem def zs
						f bnd1 bnd2 bnd3 mem
					else error "bad"

bufferInfo :: Storable w => V.Vector w -> Vk.Buffer.CreateInfo () '[ 'List w]
bufferInfo xs = Vk.Buffer.CreateInfo {
	Vk.Buffer.createInfoNext = Nothing,
	Vk.Buffer.createInfoFlags = def,
	Vk.Buffer.createInfoLengths =
		ObjectLengthList (V.length xs) :...: HVNil,
	Vk.Buffer.createInfoUsage = Vk.Buffer.UsageStorageBufferBit,
	Vk.Buffer.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Buffer.createInfoQueueFamilyIndices = [] }

pplLayoutInfo :: Vk.DscSetLyt.L sl bts -> Vk.Ppl.Lyt.CreateInfo () '[ '(sl, bts)]
pplLayoutInfo dsl = Vk.Ppl.Lyt.CreateInfo {
	Vk.Ppl.Lyt.createInfoNext = Nothing,
	Vk.Ppl.Lyt.createInfoFlags = def,
	Vk.Ppl.Lyt.createInfoSetLayouts = Vk.Ppl.Lyt.Layout dsl :...: HVNil,
	Vk.Ppl.Lyt.createInfoPushConstantRanges = [] }

computePipelineInfo :: Vk.Ppl.Lyt.LL sl sbtss ->
	Vk.Ppl.Cmpt.CreateInfo () () () () () vs sl sbtss sbph
computePipelineInfo pl = Vk.Ppl.Cmpt.CreateInfo {
			Vk.Ppl.Cmpt.createInfoNext = Nothing,
			Vk.Ppl.Cmpt.createInfoFlags = def,
			Vk.Ppl.Cmpt.createInfoStage = shaderStageInfo,
			Vk.Ppl.Cmpt.createInfoLayout = pl,
			Vk.Ppl.Cmpt.createInfoBasePipelineHandle = Nothing,
			Vk.Ppl.Cmpt.createInfoBasePipelineIndex = Nothing }

commandPoolInfo :: Vk.QFam.Index -> Vk.CommandPool.CreateInfo ()
commandPoolInfo qFam = Vk.CommandPool.CreateInfo {
	Vk.CommandPool.createInfoNext = Nothing,
	Vk.CommandPool.createInfoFlags =
		Vk.CommandPool.CreateResetCommandBufferBit,
	Vk.CommandPool.createInfoQueueFamilyIndex = qFam }

dscSetInfo :: Vk.DscPool.P sp -> Vk.DscSetLyt.L sl bts ->
	Vk.DscSet.AllocateInfo () sp '[ '(sl, bts)]
dscSetInfo pl lyt = Vk.DscSet.AllocateInfo {
	Vk.DscSet.allocateInfoNext = Nothing,
	Vk.DscSet.allocateInfoDescriptorPool = pl,
	Vk.DscSet.allocateInfoSetLayouts = Vk.DscSet.Layout lyt :...: HVNil }

commandBufferInfo :: Vk.CommandPool.C s -> Vk.CmdBuf.AllocateInfo () s
commandBufferInfo cmdPool = Vk.CmdBuf.AllocateInfo {
	Vk.CmdBuf.allocateInfoNext = Nothing,
	Vk.CmdBuf.allocateInfoCommandPool = cmdPool,
	Vk.CmdBuf.allocateInfoLevel = Vk.CmdBuf.LevelPrimary,
	Vk.CmdBuf.allocateInfoCommandBufferCount = 1 }

dscPoolInfo :: Vk.DscPool.CreateInfo ()
dscPoolInfo = Vk.DscPool.CreateInfo {
	Vk.DscPool.createInfoNext = Nothing,
	Vk.DscPool.createInfoFlags = Vk.DscPool.CreateFreeDescriptorSetBit,
	Vk.DscPool.createInfoMaxSets = 1,
	Vk.DscPool.createInfoPoolSizes = [poolSize] }
	where poolSize = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeStorageBuffer,
		Vk.DscPool.sizeDescriptorCount = 10 }

getMemoryInfo :: Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Buffer.BB sobjs ->
	IO (Vk.Dvc.Memory.Buffer.AllocateInfo ())
getMemoryInfo phdvc dvc buffer = do
	requirements <- Vk.Buffer.getMemoryRequirements dvc buffer
	memTypeIdx <- findMemoryTypeIndex phdvc requirements (
		Vk.Memory.PropertyHostVisibleBit .|.
		Vk.Memory.PropertyHostCoherentBit )
	pure Vk.Dvc.Memory.Buffer.AllocateInfo {
		Vk.Dvc.Memory.Buffer.allocateInfoNext = Nothing,
		Vk.Dvc.Memory.Buffer.allocateInfoMemoryTypeIndex = memTypeIdx }

findMemoryTypeIndex ::
	Vk.PhDvc.P -> Vk.Memory.M.Requirements -> Vk.Memory.PropertyFlags ->
	IO Vk.Memory.TypeIndex
findMemoryTypeIndex physicalDevice requirements memoryProp = do
	memoryProperties <- Vk.PhDvc.getMemoryProperties physicalDevice
	let	reqTypes = Vk.Memory.M.requirementsMemoryTypeBits requirements
		memPropTypes = (fst <$>)
			. filter (checkBits memoryProp
				. Vk.Memory.M.mTypePropertyFlags . snd)
			$ Vk.PhDvc.memoryPropertiesMemoryTypes memoryProperties
	case filter (`Vk.Memory.M.elemTypeIndex` reqTypes) memPropTypes of
		[] -> error "No available memory types"
		i : _ -> pure i

checkBits :: Bits bs => bs -> bs -> Bool
checkBits bs0 = (== bs0) . (.&. bs0)

writeDscSet :: Vk.DscSet.S sd sp slbts ->
	Vk.Buffer.Binded sb1 sm1 objs1 -> Vk.Buffer.Binded sb2 sm2 objs2 ->
	Vk.Buffer.Binded sb3 sm3 objs3 ->
	Vk.DscSet.Write () sd sp slbts '[
		'(sb1, sm1, objs1, 'List W1), '(sb2, sm2, objs2, 'List W2),
		'(sb3, sm3, objs3, 'List W3) ]
writeDscSet ds ba bb bc = Vk.DscSet.Write {
	Vk.DscSet.writeNext = Nothing,
	Vk.DscSet.writeDstSet = ds,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		bufferInfoList @W1 ba :...: bufferInfoList @W2 bb :...:
		bufferInfoList @W3 bc :...: HVNil }

bufferInfoList :: forall t {sb} {sm} {objs} .
	Vk.Buffer.Binded sb sm objs ->
	Vk.Dsc.BufferInfo '(sb, sm, objs, 'List t)
bufferInfoList = Vk.Dsc.BufferInfoList

type BindedMem sm sb w = (
	Vk.Buffer.Binded sb sm '[ 'List w],
	Vk.Dvc.Memory.Buffer.M sm '[ '[ 'List w]] )

type BindedMem3 sb1 sm1 w1 sb2 sm2 w2 sb3 sm3 w3 =
	(BindedMem sm1 sb1 w1, BindedMem sm2 sb2 w2, BindedMem sm3 sb3 w3)

shaderStageInfo :: Vk.Ppl.ShaderSt.CreateInfo () () 'GlslComputeShader () () vs
shaderStageInfo = Vk.Ppl.ShaderSt.CreateInfo {
	Vk.Ppl.ShaderSt.createInfoNext = Nothing,
	Vk.Ppl.ShaderSt.createInfoFlags = def,
	Vk.Ppl.ShaderSt.createInfoStage = Vk.ShaderStageComputeBit,
	Vk.Ppl.ShaderSt.createInfoModule = Vk.ShaderMod.M shaderModInfo nil nil,
	Vk.Ppl.ShaderSt.createInfoName = "main",
	Vk.Ppl.ShaderSt.createInfoSpecializationInfo = Nothing }
	where shaderModInfo = Vk.ShaderMod.CreateInfo {
		Vk.ShaderMod.createInfoNext = Nothing,
		Vk.ShaderMod.createInfoFlags = def,
		Vk.ShaderMod.createInfoCode = glslComputeShaderMain }

[glslComputeShader|

#version 460
layout(local_size_x = 1, local_size_y = 1) in;
layout(binding = 0) buffer Data {
	uint val[];
} data[3];

void
main()
{
	int index = int(gl_GlobalInvocationID.x);
	data[2].val[index] = data[0].val[index] + data[1].val[index];
}

|]

newtype W1 = W1 { unW1 :: Word32 } deriving (Show, Storable)
newtype W2 = W2 { unW2 :: Word32 } deriving (Show, Storable)
newtype W3 = W3 { unW3 :: Word32 } deriving (Show, Storable)

dataSize :: Integral n => n
dataSize = 1000000

datA :: V.Vector W1; datA = V.replicate dataSize $ W1 3
datB :: V.Vector W2; datB = V.fromList $ W2 <$> [1 .. dataSize]
datC :: V.Vector W3; datC = V.replicate dataSize $ W3 0
