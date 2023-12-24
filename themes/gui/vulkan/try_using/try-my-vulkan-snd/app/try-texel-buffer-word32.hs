{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
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
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Data.Kind
import Gpu.Vulkan.Object.Base qualified as KObj
import Gpu.Vulkan.Object qualified as VObj
import Data.Default
import Data.Bits
import Data.List.Length
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.Index qualified as TIndex
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.List
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Word

import qualified Data.Vector.Storable as V

import Language.SpirV.Shaderc.TH
import Language.SpirV.ShaderKind
import Data.TypeLevel.ParMaybe (nil)

import qualified Gpu.Vulkan as Vk
import qualified "try-gpu-vulkan" Gpu.Vulkan.Enum as Vk
import qualified Gpu.Vulkan.Instance as Vk.Inst
import qualified Gpu.Vulkan.PhysicalDevice as Vk.PhDvc
import qualified Gpu.Vulkan.PhysicalDevice.Struct as Vk.PhDvc
import qualified Gpu.Vulkan.Queue as Vk.Queue
import qualified Gpu.Vulkan.Queue.Enum as Vk.Queue
import qualified Gpu.Vulkan.QueueFamily as Vk.QFam
import qualified Gpu.Vulkan.Device as Vk.Dvc
import qualified Gpu.Vulkan.CommandPool as Vk.CommandPool
import qualified "try-gpu-vulkan" Gpu.Vulkan.CommandPool.Enum as Vk.CommandPool
import qualified "try-gpu-vulkan" Gpu.Vulkan.Buffer.Enum as Vk.Buffer
import qualified Gpu.Vulkan.Memory as Vk.Mem
import qualified Gpu.Vulkan.Memory.Enum as Vk.Mem
import qualified Gpu.Vulkan.Memory as Vk.Mem.M
import qualified Gpu.Vulkan.Descriptor as Vk.Dsc
import qualified "try-gpu-vulkan" Gpu.Vulkan.Descriptor.Enum as Vk.Dsc
import qualified Gpu.Vulkan.DescriptorPool as Vk.DscPool
import qualified "try-gpu-vulkan" Gpu.Vulkan.DescriptorPool.Enum as Vk.DscPool
import qualified Gpu.Vulkan.ShaderModule as Vk.ShaderMod
import qualified "try-gpu-vulkan" Gpu.Vulkan.Pipeline.Enum as Vk.Ppl
import qualified Gpu.Vulkan.PipelineLayout as Vk.Ppl.Lyt
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShaderSt
import qualified Gpu.Vulkan.Pipeline.Compute as Vk.Ppl.Cmpt
import qualified Gpu.Vulkan.DescriptorSet as Vk.DscSet
import qualified Gpu.Vulkan.CommandBuffer as Vk.CmdBuf
import qualified "try-gpu-vulkan" Gpu.Vulkan.CommandBuffer.Enum as Vk.CmdBuf
import qualified Gpu.Vulkan.Cmd as Vk.Cmd

import qualified Gpu.Vulkan.Buffer as Vk.Buffer
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscSetLyt

import qualified Gpu.Vulkan.BufferView as Vk.BufferView
import qualified Gpu.Vulkan.PushConstant as Vk.PushConstant

import Gpu.Vulkan.TypeEnum qualified as Vk.TEnum

main :: IO ()
main = do
	(r1, r2, r3) <- calc datA datB datC
	print . take 20 $ unW1 <$> r1
	print . take 20 $ unW2 <$> r2
	print . take 20 $ unW3 <$> r3

newtype W1 = W1 { unW1 :: Word32 } deriving (Show, Storable)
newtype W2 = W2 { unW2 :: Word32 } deriving (Show, Storable)
newtype W3 = W3 { unW3 :: Word32 } deriving (Show, Storable)

dataSize :: Num n => n
dataSize = 1000000

datA :: V.Vector W1; datA = V.replicate dataSize $ W1 3
datB :: V.Vector W2; datB = V.fromList $ W2 <$> [1 .. dataSize]
datC :: V.Vector W3; datC = V.replicate dataSize $ W3 0

calc :: forall w1 w2 w3 . (Storable w1, Storable w2, Storable w3) =>
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> IO ([w1], [w2], [w3])
calc da db dc = withDevice \phdvc qFam dvc mxx ->
	Vk.DscSetLyt.create dvc (dscSetLayoutInfo @w1 @w2 @w3) nil \dscSetLyt ->
	let	n = fromIntegral mxx
		da' = V.take n da; db' = V.take n db; dc' = V.take n dc
		dd = V.fromList . take (fromIntegral mxx) $ cycle [
			MyPixel 123 321 111 333,
			MyPixel 555 444 333 222,
			MyPixel 999 888 777 666 ] in
	prepareMems phdvc dvc dscSetLyt da' db' dc' dd mxx \dscSet
		(ma :: MemoryList sm1 sb1 nm1 w1)
		(mb :: MemoryList sm2 sb2 nm2 w2)
		(mc :: MemoryList sm3 sb3 nm3 w3) ->
	calc' @nm1 @nm2 @nm3 dvc qFam dscSetLyt dscSet mxx ma mb mc

type MemoryList sm sb nm w =
	Vk.Mem.M sm '[ '( sb, 'Vk.Mem.BufferArg nm '[VObj.List 256 w ""])]

calc' :: forall nm1 nm2 nm3 w1 w2 w3 objss1 objss2 objss3 sm1 sm2 sm3
		slbts sl bts sd sds . (
	Vk.DscSetLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[], '[]],
	slbts ~ '(sl, bts),
	Show (HeteroParList.PL
		(HeteroParList.PL KObj.Length)
		(Vk.DscSetLyt.BindingTypeListBufferOnlyDynamics (TIndex.I1_2 slbts))),
	Storable w1, Storable w2, Storable w3,
	Vk.Mem.OffsetSize nm1 (VObj.List 256 w1 "") objss1,
	Vk.Mem.OffsetSize nm2 (VObj.List 256 w2 "") objss2,
	Vk.Mem.OffsetSize nm3 (VObj.List 256 w3 "") objss3,
	InfixIndex '[slbts] '[ '(sl, bts)]) =>
	Vk.Dvc.D sd -> Vk.QFam.Index -> Vk.DscSetLyt.D sl bts ->
	Vk.DscSet.D sds slbts -> Word32 ->
	Vk.Mem.M sm1 objss1 -> Vk.Mem.M sm2 objss2 -> Vk.Mem.M sm3 objss3 ->
	IO ([w1], [w2], [w3])
calc' dvc qFam dscSetLyt dscSet dsz ma mb mc =
	Vk.Ppl.Lyt.create dvc (pplLayoutInfo dscSetLyt) nil \pplLyt ->
	Vk.Ppl.Cmpt.createCs
		dvc Nothing
		(U4 (computePipelineInfo pplLyt) :** HeteroParList.Nil)
		nil \(ppl :** HeteroParList.Nil) ->
	Vk.CommandPool.create dvc (commandPoolInfo qFam) nil \cmdPool ->
	Vk.CmdBuf.allocate dvc (commandBufferInfo cmdPool) \(cmdBuf :*. HeteroParList.Nil) ->
		run @nm1 @nm2 @nm3 dvc qFam cmdBuf ppl pplLyt dscSet dsz ma mb mc

pplLayoutInfo :: Vk.DscSetLyt.D sl bts ->
	Vk.Ppl.Lyt.CreateInfo 'Nothing '[ '(sl, bts)]
		('Vk.PushConstant.Layout '[] '[])
pplLayoutInfo dsl = Vk.Ppl.Lyt.CreateInfo {
	Vk.Ppl.Lyt.createInfoNext = TMaybe.N,
	Vk.Ppl.Lyt.createInfoFlags = zeroBits,
	Vk.Ppl.Lyt.createInfoSetLayouts = U2 dsl :** HeteroParList.Nil }

computePipelineInfo :: Vk.Ppl.Lyt.P sl sbtss '[] ->
	Vk.Ppl.Cmpt.CreateInfo 'Nothing
		'( 'Nothing, 'Nothing, 'GlslComputeShader, 'Nothing, '[Word32, Word32])
		'(sl, sbtss, '[]) sbph
computePipelineInfo pl = Vk.Ppl.Cmpt.CreateInfo {
	Vk.Ppl.Cmpt.createInfoNext = TMaybe.N,
	Vk.Ppl.Cmpt.createInfoFlags = def,
	Vk.Ppl.Cmpt.createInfoStage = U5 shaderStageInfo,
	Vk.Ppl.Cmpt.createInfoLayout = U3 pl,
	Vk.Ppl.Cmpt.createInfoBasePipelineHandleOrIndex = Nothing }

shaderStageInfo :: Vk.Ppl.ShaderSt.CreateInfo 'Nothing 'Nothing 'GlslComputeShader 'Nothing '[Word32, Word32]
shaderStageInfo = Vk.Ppl.ShaderSt.CreateInfo {
	Vk.Ppl.ShaderSt.createInfoNext = TMaybe.N,
	Vk.Ppl.ShaderSt.createInfoFlags = zeroBits,
	Vk.Ppl.ShaderSt.createInfoStage = Vk.ShaderStageComputeBit,
	Vk.Ppl.ShaderSt.createInfoModule = (shaderModInfo, nil),
	Vk.Ppl.ShaderSt.createInfoName = "main",
	Vk.Ppl.ShaderSt.createInfoSpecializationInfo = Just $ HeteroParList.Id 3 :** HeteroParList.Id 10 :** HeteroParList.Nil }
	where shaderModInfo = Vk.ShaderMod.CreateInfo {
		Vk.ShaderMod.createInfoNext = TMaybe.N,
		Vk.ShaderMod.createInfoFlags = zeroBits,
		Vk.ShaderMod.createInfoCode = glslComputeShaderMain }

commandPoolInfo :: Vk.QFam.Index -> Vk.CommandPool.CreateInfo 'Nothing
commandPoolInfo qFam = Vk.CommandPool.CreateInfo {
	Vk.CommandPool.createInfoNext = TMaybe.N,
	Vk.CommandPool.createInfoFlags =
		Vk.CommandPool.CreateResetCommandBufferBit,
	Vk.CommandPool.createInfoQueueFamilyIndex = qFam }

commandBufferInfo :: Vk.CommandPool.C s -> Vk.CmdBuf.AllocateInfo 'Nothing s '[ '()]
commandBufferInfo cmdPool = Vk.CmdBuf.AllocateInfo {
	Vk.CmdBuf.allocateInfoNext = TMaybe.N,
	Vk.CmdBuf.allocateInfoCommandPool = cmdPool,
	Vk.CmdBuf.allocateInfoLevel = Vk.CmdBuf.LevelPrimary }

run :: forall nm1 nm2 nm3 w1 w2 w3
	objss1 objss2 objss3 slbts sbtss sd sc sg sl sm1 sm2 sm3 sds . (
	Vk.DscSetLyt.BindingTypeListBufferOnlyDynamics (TIndex.I1_2 slbts) ~ '[ '[], '[]],
	sbtss ~ '[slbts],
	Show (HeteroParList.PL
		(HeteroParList.PL KObj.Length)
		(Vk.DscSetLyt.BindingTypeListBufferOnlyDynamics (TIndex.I1_2 slbts))),
	Storable w1, Storable w2, Storable w3,
	Vk.Mem.OffsetSize nm1 (VObj.List 256 w1 "") objss1,
	Vk.Mem.OffsetSize nm2 (VObj.List 256 w2 "") objss2,
	Vk.Mem.OffsetSize nm3 (VObj.List 256 w3 "") objss3,
	InfixIndex '[slbts] sbtss ) =>
	Vk.Dvc.D sd -> Vk.QFam.Index -> Vk.CmdBuf.C sc ->
	Vk.Ppl.Cmpt.C sg '(sl, sbtss, '[]) ->
	Vk.Ppl.Lyt.P sl sbtss '[] -> Vk.DscSet.D sds slbts -> Word32 ->
	Vk.Mem.M sm1 objss1 -> Vk.Mem.M sm2 objss2 ->
	Vk.Mem.M sm3 objss3 -> IO ([w1], [w2], [w3])
run dvc qFam cmdBuf ppl pplLyt dscSet dsz memA memB memC = do
	queue <- Vk.Dvc.getQueue dvc qFam 0
	Vk.CmdBuf.begin @'Nothing @'Nothing cmdBuf def $
		Vk.Cmd.bindPipelineCompute cmdBuf Vk.Ppl.BindPointCompute ppl \ccb -> do
			Vk.Cmd.bindDescriptorSetsCompute ccb pplLyt
				(U2 dscSet :** HeteroParList.Nil)
				(HeteroParList.Singleton $
					HeteroParList.Nil :** HeteroParList.Nil :**
					HeteroParList.Nil)
			Vk.Cmd.dispatch ccb dsz 1 1
	Vk.Queue.submit queue (U4 submitInfo :** HeteroParList.Nil) Nothing
	Vk.Queue.waitIdle queue
	(,,)	<$> Vk.Mem.read @nm1 @(VObj.List 256 w1 "") @[w1] dvc memA def
		<*> Vk.Mem.read @nm2 @(VObj.List 256 w2 "") @[w2] dvc memB def
		<*> Vk.Mem.read @nm3 @(VObj.List 256 w3 "") @[w3] dvc memC def
	where	submitInfo :: Vk.SubmitInfo 'Nothing _ _ _
		submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = TMaybe.N,
			Vk.submitInfoWaitSemaphoreDstStageMasks = HeteroParList.Nil,
			Vk.submitInfoCommandBuffers = cmdBuf :** HeteroParList.Nil,
			Vk.submitInfoSignalSemaphores = HeteroParList.Nil }

withDevice ::
	(forall sd . Vk.PhDvc.P -> Vk.QFam.Index -> Vk.Dvc.D sd -> Word32 -> IO a) -> IO a
withDevice f = Vk.Inst.create @_ @'Nothing instInfo nil \inst -> do
	phdvc <- head <$> Vk.PhDvc.enumerate inst
	limits <- Vk.PhDvc.propertiesLimits <$> Vk.PhDvc.getProperties phdvc
	let	maxGroupCountX :. _ =
			Vk.PhDvc.limitsMaxComputeWorkGroupCount limits
	putStrLn $ "maxGroupCountX: " ++ show maxGroupCountX
	qFam <- findQueueFamily phdvc Vk.Queue.ComputeBit
	Vk.Dvc.create phdvc (dvcInfo qFam) nil $ \dvc -> f phdvc qFam dvc maxGroupCountX
	where
	dvcInfo qFam = Vk.Dvc.CreateInfo {
		Vk.Dvc.createInfoNext = TMaybe.N,
		Vk.Dvc.createInfoFlags = def,
		Vk.Dvc.createInfoQueueCreateInfos = HeteroParList.Singleton $ queueInfo qFam,
		Vk.Dvc.createInfoEnabledLayerNames =
			[Vk.layerKhronosValidation],
		Vk.Dvc.createInfoEnabledExtensionNames = [],
		Vk.Dvc.createInfoEnabledFeatures = Nothing }
	queueInfo qFam = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = def,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qFam,
		Vk.Dvc.queueCreateInfoQueuePriorities = [0] }

instInfo :: Vk.Inst.CreateInfo 'Nothing 'Nothing
instInfo = def {
	Vk.Inst.createInfoEnabledLayerNames = [Vk.layerKhronosValidation] }

findQueueFamily ::
	Vk.PhDvc.P -> Vk.Queue.FlagBits -> IO Vk.QFam.Index
findQueueFamily phdvc qb = do
	qFamProperties <- Vk.PhDvc.getQueueFamilyProperties phdvc
	pure . fst . head $ filter ((/= zeroBits)
			. (.&. qb) . Vk.QFam.propertiesQueueFlags . snd)
		qFamProperties

dscSetLayoutInfo :: Vk.DscSetLyt.CreateInfo 'Nothing '[
	'Vk.DscSetLyt.Buffer '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""],
	'Vk.DscSetLyt.BufferView '[ '("", MyPixel)] ]
dscSetLayoutInfo = Vk.DscSetLyt.CreateInfo {
	Vk.DscSetLyt.createInfoNext = TMaybe.N,
	Vk.DscSetLyt.createInfoFlags = def,
	Vk.DscSetLyt.createInfoBindings = binding0 :** binding1 :** HeteroParList.Nil }

binding0 :: Vk.DscSetLyt.Binding ('Vk.DscSetLyt.Buffer objs)
binding0 = Vk.DscSetLyt.BindingBuffer {
	Vk.DscSetLyt.bindingBufferDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSetLyt.bindingBufferStageFlags = Vk.ShaderStageComputeBit }

binding1 :: Vk.DscSetLyt.Binding ('Vk.DscSetLyt.BufferView '[ '("", MyPixel) ])
binding1 = Vk.DscSetLyt.BindingBufferView {
	Vk.DscSetLyt.bindingBufferViewDescriptorType =
		Vk.Dsc.TypeStorageTexelBuffer,
	Vk.DscSetLyt.bindingBufferViewStageFlags = Vk.ShaderStageComputeBit }

prepareMems ::
	forall bts w1 w2 w3 sd sl nm1 nm2 nm3 a . (
	Default (HeteroParList.PL
		(HeteroParList.PL KObj.Length)
		(Vk.DscSetLyt.BindingTypeListBufferOnlyDynamics bts)),
	Storable w1, Storable w2, Storable w3,
	Vk.DscSet.BindingAndArrayElemBufferView bts '[ '("", MyPixel)] 0,
	Vk.DscSet.BindingAndArrayElemBuffer bts
		'[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""] 0,
	Vk.DscSet.UpdateDynamicLength bts
		'[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""] ) =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.DscSetLyt.D sl bts ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> V.Vector MyPixel -> Word32 -> (
		forall sds sm1 sb1 sm2 sb2 sm3 sb3 .
		Vk.DscSet.D sds '(sl, bts) ->
		Vk.Mem.M sm1 '[ '( sb1, 'Vk.Mem.BufferArg nm1 '[VObj.List 256 w1 ""])] ->
		Vk.Mem.M sm2 '[ '( sb2, 'Vk.Mem.BufferArg nm2 '[VObj.List 256 w2 ""])] ->
		Vk.Mem.M sm3 '[ '( sb3, 'Vk.Mem.BufferArg nm3 '[VObj.List 256 w3 ""])] ->
		IO a) -> IO a
prepareMems phdvc dvc dscSetLyt da db dc dd _mxx f =
	Vk.DscPool.create dvc dscPoolInfo nil \dscPool ->
	Vk.DscSet.allocateDs dvc (dscSetInfo dscPool dscSetLyt)
		\(dscSet :** HeteroParList.Nil) ->
	storageBufferNew4 dvc phdvc da db dc dd \ba ma bb mb bc mc
		(bd :: Vk.Buffer.Binded sm sb nm '[VObj.List 256 MyPixel ""]) _md ->
	let	bufferViewInfo' :: Vk.BufferView.CreateInfo 'Nothing MyPixel ""
			'(sm, sb, nm, '[VObj.List 256 MyPixel ""])
		bufferViewInfo' = Vk.BufferView.CreateInfo {
			Vk.BufferView.createInfoNext = TMaybe.N,
			Vk.BufferView.createInfoFlags = zeroBits,
			Vk.BufferView.createInfoBuffer = U4 bd } in
	Vk.BufferView.create dvc bufferViewInfo' nil \bv -> do
	let	wds' :: Vk.DscSet.Write _ _ _ _ 0
		wds' = Vk.DscSet.Write {
			Vk.DscSet.writeNext = TMaybe.N,
			Vk.DscSet.writeDstSet = dscSet,
			Vk.DscSet.writeDescriptorType =
				Vk.Dsc.TypeStorageTexelBuffer,
			Vk.DscSet.writeSources = Vk.DscSet.TexelBufferViews
				. HeteroParList.Singleton $ U3 bv }
	Vk.DscSet.updateDs dvc (
		U5 (writeDscSet @w1 @w2 @w3 dscSet ba bb bc) :** U5 wds' :**
		HeteroParList.Nil )
		HeteroParList.Nil
	f dscSet ma mb mc

data MyPixel = MyPixel Word32 Word32 Word32 Word32 deriving Show

type instance Vk.BufferView.FormatOf MyPixel = 'Vk.TEnum.FormatR32g32b32a32Uint

instance Storable MyPixel where
	sizeOf _ = 4 * sizeOf @Word32 undefined
	alignment _ = alignment @Word32 undefined
	peek p = do
		[r, g, b, a] <- peekArray 4 $ castPtr p
		pure $ MyPixel r g b a
	poke p (MyPixel r g b a) = pokeArray (castPtr p) [r, g, b, a]

dscPoolInfo :: Vk.DscPool.CreateInfo 'Nothing
dscPoolInfo = Vk.DscPool.CreateInfo {
	Vk.DscPool.createInfoNext = TMaybe.N,
	Vk.DscPool.createInfoFlags = Vk.DscPool.CreateFreeDescriptorSetBit,
	Vk.DscPool.createInfoMaxSets = 1,
	Vk.DscPool.createInfoPoolSizes = [poolSize, poolSize'] }
	where
	poolSize = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeStorageBuffer,
		Vk.DscPool.sizeDescriptorCount = 10 }
	poolSize' = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeStorageTexelBuffer,
		Vk.DscPool.sizeDescriptorCount = 10 }

dscSetInfo :: Vk.DscPool.P sp -> Vk.DscSetLyt.D sl bts ->
	Vk.DscSet.AllocateInfo 'Nothing sp '[ '(sl, bts)]
dscSetInfo pl lyt = Vk.DscSet.AllocateInfo {
	Vk.DscSet.allocateInfoNext = TMaybe.N,
	Vk.DscSet.allocateInfoDescriptorPool = pl,
	Vk.DscSet.allocateInfoSetLayouts = U2 lyt :** HeteroParList.Nil }

writeDscSet ::
	forall w1 w2 w3 slbts sb1 sb2 sb3 sm1 sm2 sm3 nm1 nm2 nm3 objs1 objs2 objs3 sds . (
	Show (HeteroParList.PL VObj.Length objs1),
	Show (HeteroParList.PL VObj.Length objs2),
	Show (HeteroParList.PL VObj.Length objs3),
	VObj.OffsetRange (VObj.List 256 w1 "") objs1,
	VObj.OffsetRange (VObj.List 256 w2 "") objs2,
	VObj.OffsetRange (VObj.List 256 w3 "") objs3 ) =>
	Vk.DscSet.D sds slbts ->
	Vk.Buffer.Binded sm1 sb1 nm1 objs1 -> Vk.Buffer.Binded sm2 sb2 nm2 objs2 ->
	Vk.Buffer.Binded sm3 sb3 nm3 objs3 ->
	Vk.DscSet.Write 'Nothing sds slbts ('Vk.DscSet.WriteSourcesArgBuffer '[
		'(sm1, sb1, nm1, VObj.List 256 w1 ""), '(sm2, sb2, nm2, VObj.List 256 w2 ""),
		'(sm3, sb3, nm3, VObj.List 256 w3 "") ]) 0
writeDscSet ds ba bb bc = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = ds,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		U4 (bufferInfoList @w1 ba) :** U4 (bufferInfoList @w2 bb) :**
		U4 (bufferInfoList @w3 bc) :** HeteroParList.Nil }

bufferInfoList :: forall t {sb} {sm} {nm} {objs} . (
	Show (HeteroParList.PL VObj.Length objs),
	VObj.OffsetRange (VObj.List 256 t "") objs ) =>
	Vk.Buffer.Binded sm sb nm objs ->
	Vk.Dsc.BufferInfo sm sb nm (VObj.List 256 t "")
bufferInfoList = Vk.Dsc.BufferInfo

storageBufferNew4 :: (Storable w1, Storable w2, Storable w3, Storable w4) =>
	Vk.Dvc.D sd -> Vk.PhDvc.P ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> V.Vector w4 -> (
		forall sb1 sm1 sb2 sm2 sb3 sm3 sb4 sm4 .
		Vk.Buffer.Binded sm1 sb1 nm1 '[VObj.List 256 w1 ""] ->
		Vk.Mem.M sm1 '[ '(sb1, 'Vk.Mem.BufferArg nm1 '[VObj.List 256 w1 ""])] ->
		Vk.Buffer.Binded sm2 sb2 nm2 '[VObj.List 256 w2 ""] ->
		Vk.Mem.M sm2 '[ '(sb2, 'Vk.Mem.BufferArg nm2 '[VObj.List 256 w2 ""])] ->
		Vk.Buffer.Binded sm3 sb3 nm3 '[VObj.List 256 w3 ""] ->
		Vk.Mem.M sm3 '[ '(sb3, 'Vk.Mem.BufferArg nm3 '[VObj.List 256 w3 ""])] ->
		Vk.Buffer.Binded sm4 sb4 nm4 '[VObj.List 256 w4 ""] ->
		Vk.Mem.M sm4 '[ '(sb4, 'Vk.Mem.BufferArg nm4 '[VObj.List 256 w4 ""])] ->
		IO a ) -> IO a
storageBufferNew4 dvc phdvc x y z w f = storageBufferNews
	dvc phdvc (x :** y :** z :** w :** HeteroParList.Nil) $ addArg4 f

addArg4 :: (forall sb1 sm1 sb2 sm2 sb3 sm3 sb4 sm4 .
	Vk.Buffer.Binded sm1 sb1 nm1 '[VObj.List 256 w1 ""] ->
	Vk.Mem.M sm1 '[ '(sb1, 'Vk.Mem.BufferArg nm1 '[VObj.List 256 w1 ""])] ->
	Vk.Buffer.Binded sm2 sb2 nm2 '[VObj.List 256 w2 ""] ->
	Vk.Mem.M sm2 '[ '(sb2, 'Vk.Mem.BufferArg nm2 '[VObj.List 256 w2 ""])] ->
	Vk.Buffer.Binded sm3 sb3 nm3 '[VObj.List 256 w3 ""] ->
	Vk.Mem.M sm3 '[ '(sb3, 'Vk.Mem.BufferArg nm3 '[VObj.List 256 w3 ""])] ->
	Vk.Buffer.Binded sm4 sb4 nm4 '[VObj.List 256 w4 ""] ->
	Vk.Mem.M sm4 '[ '(sb4, 'Vk.Mem.BufferArg nm4 '[VObj.List 256 w4 ""])] ->
	r) -> Arg nm1 w1 (Arg nm2 w2 (Arg nm3 w3 (Arg nm4 w4 r)))
addArg4 f = Arg \b1 m1 ->
	Arg \b2 m2 -> Arg \b3 m3 -> Arg \b4 m4 -> f b1 m1 b2 m2 b3 m3 b4 m4

storageBufferNew3 :: (Storable w1, Storable w2, Storable w3) =>
	Vk.Dvc.D sd -> Vk.PhDvc.P ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (
		forall sb1 sm1 sb2 sm2 sb3 sm3 .
		Vk.Buffer.Binded sm1 sb1 nm1 '[VObj.List 256 w1 ""] ->
		Vk.Mem.M sm1 '[ '(sb1, 'Vk.Mem.BufferArg nm1 '[VObj.List 256 w1 ""])] ->
		Vk.Buffer.Binded sm2 sb2 nm2 '[VObj.List 256 w2 ""] ->
		Vk.Mem.M sm2 '[ '(sb2, 'Vk.Mem.BufferArg nm2 '[VObj.List 256 w2 ""])] ->
		Vk.Buffer.Binded sm3 sb3 nm3 '[VObj.List 256 w3 ""] ->
		Vk.Mem.M sm3 '[ '(sb3, 'Vk.Mem.BufferArg nm3 '[VObj.List 256 w3 ""])] -> IO a ) -> IO a
storageBufferNew3 dvc phdvc x y z f =
	storageBufferNews dvc phdvc (x :** y :** z :** HeteroParList.Nil) $ addArg3 f

addArg3 :: (forall sb1 sm1 sb2 sm2 sb3 sm3 .
	Vk.Buffer.Binded sm1 sb1 nm1 '[VObj.List 256 w1 ""] ->
	Vk.Mem.M sm1 '[ '(sb1, 'Vk.Mem.BufferArg nm1 '[VObj.List 256 w1 ""])] ->
	Vk.Buffer.Binded sm2 sb2 nm2 '[VObj.List 256 w2 ""] ->
	Vk.Mem.M sm2 '[ '(sb2, 'Vk.Mem.BufferArg nm2 '[VObj.List 256 w2 ""])] ->
	Vk.Buffer.Binded sm3 sb3 nm3 '[VObj.List 256 w3 ""] ->
	Vk.Mem.M sm3 '[ '(sb3, 'Vk.Mem.BufferArg nm3 '[VObj.List 256 w3 ""])] -> r) ->
	Arg nm1 w1 (Arg nm2 w2 (Arg nm3 w3 r))
addArg3 f = Arg \b1 m1 -> Arg \b2 m2 -> Arg \b3 m3 -> f b1 m1 b2 m2 b3 m3

class StorageBufferNews f a where
	type Vectors f :: [Type]
	storageBufferNews :: Vk.Dvc.D sd -> Vk.PhDvc.P ->
		HeteroParList.PL V.Vector (Vectors f) -> f -> IO a

data Arg nm w f = Arg (forall sb sm .
	Vk.Buffer.Binded sm sb nm '[VObj.List 256 w ""] ->
	Vk.Mem.M sm '[ '(sb, 'Vk.Mem.BufferArg nm '[VObj.List 256 w ""])] -> f)

instance StorageBufferNews (IO a) a where
	type Vectors (IO a) = '[]
	storageBufferNews _dvc _phdvc HeteroParList.Nil f = f

instance (Storable w, StorageBufferNews f a) =>
	StorageBufferNews (Arg nm w f) a where
	type Vectors (Arg nm w f) = w ': Vectors f
	storageBufferNews dvc phdvc (vs :** vss) (Arg f) =
		storageBufferNew dvc phdvc vs \buf mem ->
		storageBufferNews @f @a dvc phdvc vss (f buf mem)

storageBufferNew :: forall sd nm w a . Storable w =>
	Vk.Dvc.D sd -> Vk.PhDvc.P -> V.Vector w -> (
		forall sb sm .
		Vk.Buffer.Binded sm sb nm '[VObj.List 256 w ""]  ->
		Vk.Mem.M sm '[ '(sb, 'Vk.Mem.BufferArg nm '[VObj.List 256 w ""])] -> IO a ) -> IO a
storageBufferNew dvc phdvc xs f =
	Vk.Buffer.create dvc (bufferInfo xs) nil \buffer -> do
		memoryInfo <- getMemoryInfo phdvc dvc buffer
		Vk.Mem.allocateBind dvc (U2 (Vk.Mem.Buffer buffer) :** HeteroParList.Nil) memoryInfo
			nil \(U2 (Vk.Mem.BufferBinded binded) :** HeteroParList.Nil) memory -> do
			Vk.Mem.write @nm @(VObj.List 256 w "") dvc memory def xs
			f binded memory

bufferInfo :: Storable w => V.Vector w -> Vk.Buffer.CreateInfo 'Nothing '[VObj.List 256 w ""]
bufferInfo xs = Vk.Buffer.CreateInfo {
	Vk.Buffer.createInfoNext = TMaybe.N,
	Vk.Buffer.createInfoFlags = def,
	Vk.Buffer.createInfoLengths =
		VObj.LengthList (fromIntegral $ V.length xs) :** HeteroParList.Nil,
	Vk.Buffer.createInfoUsage =
		Vk.Buffer.UsageStorageBufferBit .|.
		Vk.Buffer.UsageStorageTexelBufferBit,
	Vk.Buffer.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Buffer.createInfoQueueFamilyIndices = [] }

getMemoryInfo :: Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Buffer.B sb nm objs ->
	IO (Vk.Mem.AllocateInfo 'Nothing)
getMemoryInfo phdvc dvc buffer = do
	requirements <- Vk.Buffer.getMemoryRequirements dvc buffer
	memTypeIdx <- findMemoryTypeIndex phdvc requirements (
		Vk.Mem.PropertyHostVisibleBit .|.
		Vk.Mem.PropertyHostCoherentBit )
	pure Vk.Mem.AllocateInfo {
		Vk.Mem.allocateInfoNext = TMaybe.N,
		Vk.Mem.allocateInfoMemoryTypeIndex = memTypeIdx }

findMemoryTypeIndex ::
	Vk.PhDvc.P -> Vk.Mem.M.Requirements -> Vk.Mem.PropertyFlags ->
	IO Vk.Mem.M.TypeIndex
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

layout(binding = 1, rgba32ui) uniform uimageBuffer storageTexelBuffer;

void
main()
{
	int index = int(gl_GlobalInvocationID.x);
	uvec4 some = imageLoad(storageTexelBuffer, index);
//	data[2].val[index] = (data[0].val[index] + data[1].val[index]) * sc * sc2;
	data[0].val[index] = some.x;
	data[1].val[index] = some.y;
	data[2].val[index] = some.z;
}

|]
