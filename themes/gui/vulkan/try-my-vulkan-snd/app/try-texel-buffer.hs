{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
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
import Data.List.Length
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
import qualified Gpu.Vulkan.PhysicalDevice.Struct as Vk.PhDvc
import qualified Gpu.Vulkan.Queue as Vk.Queue
import qualified Gpu.Vulkan.Queue.Enum as Vk.Queue
import qualified Gpu.Vulkan.QueueFamily as Vk.QFam
import qualified Gpu.Vulkan.QueueFamily.EnumManual as Vk.QFam
import qualified Gpu.Vulkan.Device as Vk.Dvc
import qualified Gpu.Vulkan.CommandPool as Vk.CommandPool
import qualified Gpu.Vulkan.CommandPool.Enum as Vk.CommandPool
import qualified Gpu.Vulkan.Buffer.Enum as Vk.Buffer
import qualified Gpu.Vulkan.Memory as Vk.Mem
import qualified Gpu.Vulkan.Memory.Kind as Vk.Mem.K
import qualified Gpu.Vulkan.Memory.Tmp as Vk.Mem
import qualified Gpu.Vulkan.Memory.Enum as Vk.Mem
import qualified Gpu.Vulkan.Memory.Middle as Vk.Mem.M
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
import qualified Gpu.Vulkan.Memory.AllocateInfo as Vk.Dvc.Mem.Buffer
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscSetLyt
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Vk.DscSetLyt

import qualified Gpu.Vulkan.Khr as Vk.Khr

import qualified Gpu.Vulkan.BufferView.Middle as Vk.BufferView.M

main :: IO ()
main = do
	(r1, r2, r3) <- calc datA datB datC
	print . take 20 $ unW1 <$> r1
	print . take 20 $ unW2 <$> r2
	print . take 20 $ unW3 <$> r3

newtype W1 = W1 { unW1 :: Word32 } deriving (Show, Storable)
newtype W2 = W2 { unW2 :: Word32 } deriving (Show, Storable)
newtype W3 = W3 { unW3 :: Word32 } deriving (Show, Storable)

dataSize :: Integral n => n
dataSize = 1000000

datA :: V.Vector W1; datA = V.replicate dataSize $ W1 3
datB :: V.Vector W2; datB = V.fromList $ W2 <$> [1 .. dataSize]
datC :: V.Vector W3; datC = V.replicate dataSize $ W3 0

calc :: forall w1 w2 w3 . (Storable w1, Storable w2, Storable w3) =>
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> IO ([w1], [w2], [w3])
calc da db dc = withDevice \phdvc qFam dvc mxx ->
	Vk.DscSetLyt.create dvc (dscSetLayoutInfo @w1 @w2 @w3) nil nil \dscSetLyt ->
	let	n = fromIntegral mxx
		da' = V.take n da; db' = V.take n db; dc' = V.take n dc in
	prepareMems phdvc dvc dscSetLyt da' db' dc' dc' \dscSet
		(ma :: MemoryList sm1 sb1 nm1 w1)
		(mb :: MemoryList sm2 sb2 nm2 w2)
		(mc :: MemoryList sm3 sb3 nm3 w3) ->
	calc' @nm1 @nm2 @nm3 dvc qFam dscSetLyt dscSet mxx ma mb mc

type MemoryList sm sb nm w =
	Vk.Mem.M sm '[ '( sb, 'Vk.Mem.K.Buffer nm '[ 'List 256 w ""])]

calc' :: forall nm1 nm2 nm3 w1 w2 w3 objss1 objss2 objss3 sm1 sm2 sm3
		slbts sl bts sd sp . (
	Storable w1, Storable w2, Storable w3,
	Vk.Mem.OffsetSize' nm1 ('List 256 w1 "") objss1,
	Vk.Mem.OffsetSize' nm2 ('List 256 w2 "") objss2,
	Vk.Mem.OffsetSize' nm3 ('List 256 w3 "") objss3,
	Vk.Cmd.SetPos '[slbts] '[ '(sl, bts)]) =>
	Vk.Dvc.D sd -> Vk.QFam.Index -> Vk.DscSetLyt.L sl bts ->
	Vk.DscSet.S sd sp slbts -> Word32 ->
	Vk.Mem.M sm1 objss1 -> Vk.Mem.M sm2 objss2 -> Vk.Mem.M sm3 objss3 ->
	IO ([w1], [w2], [w3])
calc' dvc qFam dscSetLyt dscSet dsz ma mb mc =
	Vk.Ppl.Lyt.create dvc (pplLayoutInfo dscSetLyt) nil nil \pplLyt ->
	Vk.Ppl.Cmpt.createCs @'[ '(Word32 :.: Word32 :.: (), _, _, _)]
		dvc Nothing
		(Vk.Ppl.Cmpt.CreateInfo_
			(computePipelineInfo pplLyt) :...: HVNil)
		nil nil \(Vk.Ppl.Cmpt.Pipeline ppl :...: HVNil) ->
	Vk.CommandPool.create dvc (commandPoolInfo qFam) nil nil \cmdPool ->
	Vk.CmdBuf.allocate dvc (commandBufferInfo cmdPool) \case
		[cmdBuf] -> run @nm1 @nm2 @nm3
			dvc qFam cmdBuf ppl pplLyt dscSet dsz ma mb mc
		_ -> error "never occur"

pplLayoutInfo :: Vk.DscSetLyt.L sl bts -> Vk.Ppl.Lyt.CreateInfo () '[ '(sl, bts)]
pplLayoutInfo dsl = Vk.Ppl.Lyt.CreateInfo {
	Vk.Ppl.Lyt.createInfoNext = Nothing,
	Vk.Ppl.Lyt.createInfoFlags = def,
	Vk.Ppl.Lyt.createInfoSetLayouts = Vk.Ppl.Lyt.Layout dsl :...: HVNil,
	Vk.Ppl.Lyt.createInfoPushConstantRanges = [] }

computePipelineInfo :: Vk.Ppl.Lyt.LL sl sbtss ->
	Vk.Ppl.Cmpt.CreateInfo () () () () () (Word32 :.: Word32 :.: ()) sl sbtss sbph
computePipelineInfo pl = Vk.Ppl.Cmpt.CreateInfo {
	Vk.Ppl.Cmpt.createInfoNext = Nothing,
	Vk.Ppl.Cmpt.createInfoFlags = def,
	Vk.Ppl.Cmpt.createInfoStage = shaderStageInfo,
	Vk.Ppl.Cmpt.createInfoLayout = pl,
	Vk.Ppl.Cmpt.createInfoBasePipelineHandle = Nothing,
	Vk.Ppl.Cmpt.createInfoBasePipelineIndex = Nothing }

shaderStageInfo :: Vk.Ppl.ShaderSt.CreateInfo () () 'GlslComputeShader () () (Word32 :.: Word32 :.: ())
shaderStageInfo = Vk.Ppl.ShaderSt.CreateInfo {
	Vk.Ppl.ShaderSt.createInfoNext = Nothing,
	Vk.Ppl.ShaderSt.createInfoFlags = zeroBits,
	Vk.Ppl.ShaderSt.createInfoStage = Vk.ShaderStageComputeBit,
	Vk.Ppl.ShaderSt.createInfoModule = Vk.ShaderMod.M shaderModInfo nil nil,
	Vk.Ppl.ShaderSt.createInfoName = "main",
	Vk.Ppl.ShaderSt.createInfoSpecializationInfo = Just $ 3 :.: 10 :.: () }
	where shaderModInfo = Vk.ShaderMod.CreateInfo {
		Vk.ShaderMod.createInfoNext = Nothing,
		Vk.ShaderMod.createInfoFlags = zeroBits,
		Vk.ShaderMod.createInfoCode = glslComputeShaderMain }

commandPoolInfo :: Vk.QFam.Index -> Vk.CommandPool.CreateInfo ()
commandPoolInfo qFam = Vk.CommandPool.CreateInfo {
	Vk.CommandPool.createInfoNext = Nothing,
	Vk.CommandPool.createInfoFlags =
		Vk.CommandPool.CreateResetCommandBufferBit,
	Vk.CommandPool.createInfoQueueFamilyIndex = qFam }

commandBufferInfo :: Vk.CommandPool.C s -> Vk.CmdBuf.AllocateInfo () s
commandBufferInfo cmdPool = Vk.CmdBuf.AllocateInfo {
	Vk.CmdBuf.allocateInfoNext = Nothing,
	Vk.CmdBuf.allocateInfoCommandPool = cmdPool,
	Vk.CmdBuf.allocateInfoLevel = Vk.CmdBuf.LevelPrimary,
	Vk.CmdBuf.allocateInfoCommandBufferCount = 1 }

run :: forall nm1 nm2 nm3 w1 w2 w3
	objss1 objss2 objss3 slbts sbtss sd sc vs sg sl sp sm1 sm2 sm3 . (
	Storable w1, Storable w2, Storable w3,
	Vk.Mem.OffsetSize' nm1 ('List 256 w1 "") objss1,
	Vk.Mem.OffsetSize' nm2 ('List 256 w2 "") objss2,
	Vk.Mem.OffsetSize' nm3 ('List 256 w3 "") objss3,
	Vk.Cmd.SetPos '[slbts] sbtss ) =>
	Vk.Dvc.D sd -> Vk.QFam.Index -> Vk.CmdBuf.C sc vs -> Vk.Ppl.Cmpt.C sg ->
	Vk.Ppl.Lyt.LL sl sbtss -> Vk.DscSet.S sd sp slbts -> Word32 ->
	Vk.Mem.M sm1 objss1 -> Vk.Mem.M sm2 objss2 ->
	Vk.Mem.M sm3 objss3 -> IO ([w1], [w2], [w3])
run dvc qFam cmdBuf ppl pplLyt dscSet dsz memA memB memC = do
	queue <- Vk.Dvc.getQueue dvc qFam 0
	Vk.CmdBuf.begin @() @() cmdBuf def do
		Vk.Cmd.bindPipelineCompute cmdBuf Vk.Ppl.BindPointCompute ppl
		Vk.Cmd.bindDescriptorSets cmdBuf Vk.Ppl.BindPointCompute pplLyt
			(Vk.Cmd.DescriptorSet dscSet :...: HVNil) []
		Vk.Cmd.dispatch cmdBuf dsz 1 1
	Vk.Queue.submit @() queue [submitInfo] Nothing
	Vk.Queue.waitIdle queue
	(,,)	<$> Vk.Mem.read @nm1 @('List 256 w1 "") @[w1] dvc memA def
		<*> Vk.Mem.read @nm2 @('List 256 w2 "") @[w2] dvc memB def
		<*> Vk.Mem.read @nm3 @('List 256 w3 "") @[w3] dvc memC def
	where	submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = Nothing,
			Vk.submitInfoWaitSemaphoreDstStageMasks = HVNil,
			Vk.submitInfoCommandBuffers = [cmdBuf],
			Vk.submitInfoSignalSemaphores = [] }

withDevice ::
	(forall sd . Vk.PhDvc.P -> Vk.QFam.Index -> Vk.Dvc.D sd -> Word32 -> IO a) -> IO a
withDevice f = Vk.Inst.create @() @() instInfo nil nil \inst -> do
	phdvc <- head <$> Vk.PhDvc.enumerate inst
	limits <- Vk.PhDvc.propertiesLimits <$> Vk.PhDvc.getProperties phdvc
	let	maxGroupCountX :. _ =
			Vk.PhDvc.limitsMaxComputeWorkGroupCount limits
	putStrLn $ "maxGroupCountX: " ++ show maxGroupCountX
	qFam <- findQueueFamily phdvc Vk.Queue.ComputeBit
	Vk.Dvc.create @() @'[()] phdvc (dvcInfo qFam) nil nil $ \dvc -> f phdvc qFam dvc maxGroupCountX
	where
	dvcInfo qFam = Vk.Dvc.CreateInfo {
		Vk.Dvc.createInfoNext = Nothing,
		Vk.Dvc.createInfoFlags = def,
		Vk.Dvc.createInfoQueueCreateInfos = Singleton $ queueInfo qFam,
		Vk.Dvc.createInfoEnabledLayerNames =
			[Vk.Khr.validationLayerName],
		Vk.Dvc.createInfoEnabledExtensionNames = [],
		Vk.Dvc.createInfoEnabledFeatures = Nothing }
	queueInfo qFam = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = Nothing,
		Vk.Dvc.queueCreateInfoFlags = def,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qFam,
		Vk.Dvc.queueCreateInfoQueuePriorities = [0] }

instInfo :: Vk.Inst.CreateInfo () ()
instInfo = def {
	Vk.Inst.createInfoEnabledLayerNames = [Vk.Khr.validationLayerName] }

findQueueFamily ::
	Vk.PhDvc.P -> Vk.Queue.FlagBits -> IO Vk.QFam.Index
findQueueFamily phdvc qb = do
	qFamProperties <- Vk.PhDvc.getQueueFamilyProperties phdvc
	pure . fst . head $ filter ((/= zeroBits)
			. (.&. qb) . Vk.QFam.propertiesQueueFlags . snd)
		qFamProperties

dscSetLayoutInfo :: Vk.DscSetLyt.CreateInfo ()
	'[ 'Vk.DscSetLyt.Buffer '[ 'List 256 w1 "", 'List 256 w2 "", 'List 256 w3 ""]]
dscSetLayoutInfo = Vk.DscSetLyt.CreateInfo {
	Vk.DscSetLyt.createInfoNext = Nothing,
	Vk.DscSetLyt.createInfoFlags = def,
	Vk.DscSetLyt.createInfoBindings = binding0 :...: HVNil }

binding0 :: Vk.DscSetLyt.Binding ('Vk.DscSetLyt.Buffer objs)
binding0 = Vk.DscSetLyt.BindingBuffer {
	Vk.DscSetLyt.bindingBufferDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSetLyt.bindingBufferStageFlags = Vk.ShaderStageComputeBit }

prepareMems ::
	forall bts w1 w2 w3 w4 sd sl nm1 nm2 nm3 a .
	(Storable w1, Storable w2, Storable w3, Storable w4) =>
	Vk.DscSet.BindingAndArrayElem bts
		'[ 'List 256 w1 "", 'List 256 w2 "", 'List 256 w3 ""] =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.DscSetLyt.L sl bts ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> V.Vector w4 -> (
		forall s sm1 sb1 sm2 sb2 sm3 sb3 .
		Vk.DscSet.S sd s '(sl, bts) ->
		Vk.Mem.M sm1 '[ '( sb1, 'Vk.Mem.K.Buffer nm1 '[ 'List 256 w1 ""])] ->
		Vk.Mem.M sm2 '[ '( sb2, 'Vk.Mem.K.Buffer nm2 '[ 'List 256 w2 ""])] ->
		Vk.Mem.M sm3 '[ '( sb3, 'Vk.Mem.K.Buffer nm3 '[ 'List 256 w3 ""])] ->
		IO a) -> IO a
prepareMems phdvc dvc dscSetLyt da db dc dd f =
	Vk.DscPool.create dvc dscPoolInfo nil nil \dscPool ->
	Vk.DscSet.allocateSs dvc (dscSetInfo dscPool dscSetLyt)
		>>= \(dscSet :...: HVNil) ->
	storageBufferNew4 dvc phdvc da db dc dd \ba ma bb mb bc mc bd md -> do
	let	bufferViewInfo = Vk.BufferView.M.CreateInfo {
			Vk.BufferView.M.createInfoNext = Nothing,
			Vk.BufferView.M.createInfoFlags = zeroBits
			}
	Vk.DscSet.updateDs @() @() dvc (Vk.DscSet.Write_
		(writeDscSet @w1 @w2 @w3 dscSet ba bb bc) :...: HVNil) []
	f dscSet ma mb mc

dscPoolInfo :: Vk.DscPool.CreateInfo ()
dscPoolInfo = Vk.DscPool.CreateInfo {
	Vk.DscPool.createInfoNext = Nothing,
	Vk.DscPool.createInfoFlags = Vk.DscPool.CreateFreeDescriptorSetBit,
	Vk.DscPool.createInfoMaxSets = 1,
	Vk.DscPool.createInfoPoolSizes = [poolSize] }
	where poolSize = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeStorageBuffer,
		Vk.DscPool.sizeDescriptorCount = 10 }

dscSetInfo :: Vk.DscPool.P sp -> Vk.DscSetLyt.L sl bts ->
	Vk.DscSet.AllocateInfo () sp '[ '(sl, bts)]
dscSetInfo pl lyt = Vk.DscSet.AllocateInfo {
	Vk.DscSet.allocateInfoNext = Nothing,
	Vk.DscSet.allocateInfoDescriptorPool = pl,
	Vk.DscSet.allocateInfoSetLayouts = Vk.DscSet.Layout lyt :...: HVNil }

writeDscSet ::
	forall w1 w2 w3 sd sp slbts sb1 sb2 sb3 sm1 sm2 sm3 nm1 nm2 nm3 objs1 objs2 objs3 .
	Vk.DscSet.S sd sp slbts ->
	Vk.Buffer.Binded sm1 sb1 nm1 objs1 -> Vk.Buffer.Binded sm2 sb2 nm2 objs2 ->
	Vk.Buffer.Binded sm3 sb3 nm3 objs3 ->
	Vk.DscSet.Write () sd sp slbts ('Vk.DscSet.WriteSourcesArgBuffer '[
		'(sb1, sm1, nm1, objs1, 'List 256 w1 ""), '(sb2, sm2, nm2, objs2, 'List 256 w2 ""),
		'(sb3, sm3, nm3, objs3, 'List 256 w3 "") ])
writeDscSet ds ba bb bc = Vk.DscSet.Write {
	Vk.DscSet.writeNext = Nothing,
	Vk.DscSet.writeDstSet = ds,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		bufferInfoList @w1 ba :...: bufferInfoList @w2 bb :...:
		bufferInfoList @w3 bc :...: HVNil }

bufferInfoList :: forall t {sb} {sm} {nm} {objs} .
	Vk.Buffer.Binded sm sb nm objs ->
	Vk.Dsc.BufferInfo '(sb, sm, nm, objs, 'List 256 t "")
bufferInfoList = Vk.Dsc.BufferInfoList

storageBufferNew4 :: (Storable w1, Storable w2, Storable w3, Storable w4) =>
	Vk.Dvc.D sd -> Vk.PhDvc.P ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> V.Vector w4 -> (
		forall sb1 sm1 sb2 sm2 sb3 sm3 sb4 sm4 .
		Vk.Buffer.Binded sb1 sm1 nm1 '[ 'List 256 w1 ""] ->
		Vk.Mem.M sm1 '[ '(sb1, 'Vk.Mem.K.Buffer nm1 '[ 'List 256 w1 ""])] ->
		Vk.Buffer.Binded sb2 sm2 nm2 '[ 'List 256 w2 ""] ->
		Vk.Mem.M sm2 '[ '(sb2, 'Vk.Mem.K.Buffer nm2 '[ 'List 256 w2 ""])] ->
		Vk.Buffer.Binded sb3 sm3 nm3 '[ 'List 256 w3 ""] ->
		Vk.Mem.M sm3 '[ '(sb3, 'Vk.Mem.K.Buffer nm3 '[ 'List 256 w3 ""])] ->
		Vk.Buffer.Binded sb4 sm4 nm4 '[ 'List 256 w4 ""] ->
		Vk.Mem.M sm4 '[ '(sb4, 'Vk.Mem.K.Buffer nm4 '[ 'List 256 w4 ""])] ->
		IO a ) -> IO a
storageBufferNew4 dvc phdvc x y z w f = storageBufferNews
	dvc phdvc (x :...: y :...: z :...: w :...: HVNil) $ addArg4 f

addArg4 :: (forall sb1 sm1 sb2 sm2 sb3 sm3 sb4 sm4 .
	Vk.Buffer.Binded sb1 sm1 nm1 '[ 'List 256 w1 ""] ->
	Vk.Mem.M sm1 '[ '(sb1, 'Vk.Mem.K.Buffer nm1 '[ 'List 256 w1 ""])] ->
	Vk.Buffer.Binded sb2 sm2 nm2 '[ 'List 256 w2 ""] ->
	Vk.Mem.M sm2 '[ '(sb2, 'Vk.Mem.K.Buffer nm2 '[ 'List 256 w2 ""])] ->
	Vk.Buffer.Binded sb3 sm3 nm3 '[ 'List 256 w3 ""] ->
	Vk.Mem.M sm3 '[ '(sb3, 'Vk.Mem.K.Buffer nm3 '[ 'List 256 w3 ""])] ->
	Vk.Buffer.Binded sb4 sm4 nm4 '[ 'List 256 w4 ""] ->
	Vk.Mem.M sm4 '[ '(sb4, 'Vk.Mem.K.Buffer nm4 '[ 'List 256 w4 ""])] ->
	r) -> Arg nm1 w1 (Arg nm2 w2 (Arg nm3 w3 (Arg nm4 w4 r)))
addArg4 f = Arg \b1 m1 ->
	Arg \b2 m2 -> Arg \b3 m3 -> Arg \b4 m4 -> f b1 m1 b2 m2 b3 m3 b4 m4

storageBufferNew3 :: (Storable w1, Storable w2, Storable w3) =>
	Vk.Dvc.D sd -> Vk.PhDvc.P ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (
		forall sb1 sm1 sb2 sm2 sb3 sm3 .
		Vk.Buffer.Binded sb1 sm1 nm1 '[ 'List 256 w1 ""] ->
		Vk.Mem.M sm1 '[ '(sb1, 'Vk.Mem.K.Buffer nm1 '[ 'List 256 w1 ""])] ->
		Vk.Buffer.Binded sb2 sm2 nm2 '[ 'List 256 w2 ""] ->
		Vk.Mem.M sm2 '[ '(sb2, 'Vk.Mem.K.Buffer nm2 '[ 'List 256 w2 ""])] ->
		Vk.Buffer.Binded sb3 sm3 nm3 '[ 'List 256 w3 ""] ->
		Vk.Mem.M sm3 '[ '(sb3, 'Vk.Mem.K.Buffer nm3 '[ 'List 256 w3 ""])] -> IO a ) -> IO a
storageBufferNew3 dvc phdvc x y z f =
	storageBufferNews dvc phdvc (x :...: y :...: z :...: HVNil) $ addArg3 f

addArg3 :: (forall sb1 sm1 sb2 sm2 sb3 sm3 .
	Vk.Buffer.Binded sb1 sm1 nm1 '[ 'List 256 w1 ""] ->
	Vk.Mem.M sm1 '[ '(sb1, 'Vk.Mem.K.Buffer nm1 '[ 'List 256 w1 ""])] ->
	Vk.Buffer.Binded sb2 sm2 nm2 '[ 'List 256 w2 ""] ->
	Vk.Mem.M sm2 '[ '(sb2, 'Vk.Mem.K.Buffer nm2 '[ 'List 256 w2 ""])] ->
	Vk.Buffer.Binded sb3 sm3 nm3 '[ 'List 256 w3 ""] ->
	Vk.Mem.M sm3 '[ '(sb3, 'Vk.Mem.K.Buffer nm3 '[ 'List 256 w3 ""])] -> r) ->
	Arg nm1 w1 (Arg nm2 w2 (Arg nm3 w3 r))
addArg3 f = Arg \b1 m1 -> Arg \b2 m2 -> Arg \b3 m3 -> f b1 m1 b2 m2 b3 m3

class StorageBufferNews f a where
	type Vectors f :: [Type]
	storageBufferNews :: Vk.Dvc.D sd -> Vk.PhDvc.P ->
		HeteroVarList V.Vector (Vectors f) -> f -> IO a

data Arg nm w f = Arg (forall sb sm .
	Vk.Buffer.Binded sb sm nm '[ 'List 256 w ""] ->
	Vk.Mem.M sm '[ '(sb, 'Vk.Mem.K.Buffer nm '[ 'List 256 w ""])] -> f)

instance StorageBufferNews (IO a) a where
	type Vectors (IO a) = '[]
	storageBufferNews _dvc _phdvc HVNil f = f

instance (Storable w, StorageBufferNews f a) =>
	StorageBufferNews (Arg nm w f) a where
	type Vectors (Arg nm w f) = w ': Vectors f
	storageBufferNews dvc phdvc (vs :...: vss) (Arg f) =
		storageBufferNew dvc phdvc vs \buf mem ->
		storageBufferNews @f @a dvc phdvc vss (f buf mem)

storageBufferNew :: forall sd nm w a . Storable w =>
	Vk.Dvc.D sd -> Vk.PhDvc.P -> V.Vector w -> (
		forall sb sm .
		Vk.Buffer.Binded sb sm nm '[ 'List 256 w ""]  ->
		Vk.Mem.M sm '[ '(sb, 'Vk.Mem.K.Buffer nm '[ 'List 256 w ""])] -> IO a ) -> IO a
storageBufferNew dvc phdvc xs f =
	Vk.Buffer.create dvc (bufferInfo xs) nil nil \buffer -> do
		memoryInfo <- getMemoryInfo phdvc dvc buffer
		Vk.Mem.allocateBind dvc (V2 (Vk.Mem.Buffer buffer) :...: HVNil) memoryInfo
			nil nil \(V2 (Vk.Mem.BufferBinded binded) :...: HVNil) memory -> do
			Vk.Mem.write @nm @('List 256 w "") dvc memory def xs
			f binded memory

bufferInfo :: Storable w => V.Vector w -> Vk.Buffer.CreateInfo () '[ 'List 256 w ""]
bufferInfo xs = Vk.Buffer.CreateInfo {
	Vk.Buffer.createInfoNext = Nothing,
	Vk.Buffer.createInfoFlags = def,
	Vk.Buffer.createInfoLengths =
		ObjectLengthList (V.length xs) :...: HVNil,
	Vk.Buffer.createInfoUsage = Vk.Buffer.UsageStorageBufferBit,
	Vk.Buffer.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Buffer.createInfoQueueFamilyIndices = [] }

getMemoryInfo :: Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Buffer.B sb nm objs ->
	IO (Vk.Dvc.Mem.Buffer.AllocateInfo ())
getMemoryInfo phdvc dvc buffer = do
	requirements <- Vk.Buffer.getMemoryRequirements dvc buffer
	memTypeIdx <- findMemoryTypeIndex phdvc requirements (
		Vk.Mem.PropertyHostVisibleBit .|.
		Vk.Mem.PropertyHostCoherentBit )
	pure Vk.Dvc.Mem.Buffer.AllocateInfo {
		Vk.Dvc.Mem.Buffer.allocateInfoNext = Nothing,
		Vk.Dvc.Mem.Buffer.allocateInfoMemoryTypeIndex = memTypeIdx }

findMemoryTypeIndex ::
	Vk.PhDvc.P -> Vk.Mem.M.Requirements -> Vk.Mem.PropertyFlags ->
	IO Vk.Mem.TypeIndex
findMemoryTypeIndex physicalDevice requirements memoryProp = do
	memoryProperties <- Vk.PhDvc.getMemoryProperties physicalDevice
	let	reqTypes = Vk.Mem.M.requirementsMemoryTypeBits requirements
		memPropTypes = (fst <$>)
			. filter (checkBits memoryProp
				. Vk.Mem.M.mTypePropertyFlags . snd)
			$ Vk.PhDvc.memoryPropertiesMemoryTypes memoryProperties
	case filter (`Vk.Mem.M.elemTypeIndex` reqTypes) memPropTypes of
		[] -> error "No available memory types"
		i : _ -> pure i

checkBits :: Bits bs => bs -> bs -> Bool
checkBits bs0 = (== bs0) . (.&. bs0)

[glslComputeShader|

#version 460
layout(local_size_x = 1, local_size_y = 1) in;
layout(binding = 0) buffer Data {
	uint val[];
} data[3];

layout(constant_id = 0) const uint sc = 2;
layout(constant_id = 1) const uint sc2 = 3;

void
main()
{
	int index = int(gl_GlobalInvocationID.x);
	data[2].val[index] = (data[0].val[index] + data[1].val[index]) * sc * sc2;
}

|]
