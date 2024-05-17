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

module Main (main) where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Data.Kind
import Gpu.Vulkan.Object.Base qualified as KObj
import Gpu.Vulkan.Object qualified as Obj
import Data.Default
import Data.Bits
import Data.List.Length
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.Index qualified as TIndex
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.List
import Data.HeteroParList qualified as HPList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Word

import qualified Data.Vector.Storable as V

import Language.SpirV.Shaderc.TH
import Language.SpirV.ShaderKind
import Data.TypeLevel.ParMaybe (nil)

import qualified Gpu.Vulkan as Vk
import qualified Gpu.Vulkan.Instance as Vk.Inst
import qualified Gpu.Vulkan.PhysicalDevice as Vk.Phd
import qualified Gpu.Vulkan.Queue as Vk.Q
import qualified Gpu.Vulkan.QueueFamily as Vk.QFam
import qualified Gpu.Vulkan.Device as Vk.Dvc
import qualified Gpu.Vulkan.CommandPool as Vk.CmdPl
import qualified Gpu.Vulkan.Memory as Vk.Mm
import qualified Gpu.Vulkan.Memory as Vk.Mm.M
import qualified Gpu.Vulkan.Descriptor as Vk.Dsc
import qualified Gpu.Vulkan.DescriptorPool as Vk.DscPool
import qualified Gpu.Vulkan.ShaderModule as Vk.ShaderMod
import qualified "try-gpu-vulkan" Gpu.Vulkan.Pipeline as Vk.Ppl
import qualified Gpu.Vulkan.PipelineLayout as Vk.Ppl.Lyt
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShaderSt
import qualified Gpu.Vulkan.Pipeline.Compute as Vk.Ppl.Cmpt
import qualified Gpu.Vulkan.DescriptorSet as Vk.DscSet
import qualified Gpu.Vulkan.CommandBuffer as Vk.CmdBuf
import qualified Gpu.Vulkan.Cmd as Vk.Cmd

import qualified Gpu.Vulkan.Buffer as Vk.Buffer
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscSetLyt

import qualified Gpu.Vulkan.BufferView as Vk.BufferView
import qualified Gpu.Vulkan.PushConstant as Vk.PushConstant

import Gpu.Vulkan.TypeEnum qualified as Vk.TEnum

main :: IO ()
main = withDvc \pd d q cpl mgcx ->
	Vk.DscSetLyt.create d (dscSetLayoutInfo @W1 @W2 @W3) nil \dscSetLyt ->
	let	da = V.replicate mgcx 0
		db = V.replicate mgcx 0
		dc = V.replicate mgcx 0
		dd = V.fromList . take mgcx $ cycle [
			MyPixel 123 321 111 333,
			MyPixel 555 444 333 222,
			MyPixel 999 888 777 666 ] in
	prepareMems pd d dscSetLyt da db dc dd mgcx \dscSet
		(ma :: MemoryList sm1 sb1 nm1 w1)
		(mb :: MemoryList sm2 sb2 nm2 w2)
		(mc :: MemoryList sm3 sb3 nm3 w3) -> do
	calc' @nm1 @nm2 @nm3 @w1 @w2 @w3
		@(MmBffrArg sb1 nm1 w1) @(MmBffrArg sb2 nm2 w2)
		@(MmBffrArg sb3 nm3 w3) d q cpl dscSetLyt dscSet mgcx
	(r1, r2, r3) <- (,,)
		<$> Vk.Mm.read @nm1 @(Obj.List 256 w1 "") @0 @[w1] d ma def
		<*> Vk.Mm.read @nm2 @(Obj.List 256 w2 "") @0 @[w2] d mb def
		<*> Vk.Mm.read @nm3 @(Obj.List 256 w3 "") @0 @[w3] d mc def
	print . take 20 $ unW1 <$> r1
	print . take 20 $ unW2 <$> r2
	print . take 20 $ unW3 <$> r3

newtype W1 = W1 { unW1 :: Float } deriving (Show, Storable, Num)
newtype W2 = W2 { unW2 :: Float } deriving (Show, Storable, Num)
newtype W3 = W3 { unW3 :: Float } deriving (Show, Storable, Num)

type MmBffrArg sb nm w = '[ '(sb, 'Vk.Mm.BufferArg nm '[OList w])]
type OList t = Obj.List 256 t ""

type MemoryList sm sb nm w =
	Vk.Mm.M sm '[ '( sb, 'Vk.Mm.BufferArg nm '[Obj.List 256 w ""])]

calc' :: forall nm1 nm2 nm3 w1 w2 w3 objss1 objss2 objss3
		slbts sl bts sd sds scpl . (
	Vk.DscSetLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[], '[]],
	slbts ~ '(sl, bts),
	Show (HPList.PL
		(HPList.PL KObj.Length)
		(Vk.DscSetLyt.BindingTypeListBufferOnlyDynamics (TIndex.I1_2 slbts))),
	Storable w1, Storable w2, Storable w3,
	Vk.Mm.OffsetSize nm1 (Obj.List 256 w1 "") objss1 0,
	Vk.Mm.OffsetSize nm2 (Obj.List 256 w2 "") objss2 0,
	Vk.Mm.OffsetSize nm3 (Obj.List 256 w3 "") objss3 0,
	InfixIndex '[slbts] '[ '(sl, bts)]) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C scpl ->
	Vk.DscSetLyt.D sl bts ->
	Vk.DscSet.D sds slbts -> Word32 -> IO ()
calc' dvc queue cmdPool dscSetLyt dscSet dsz =
	Vk.Ppl.Lyt.create dvc (pplLayoutInfo dscSetLyt) nil \pplLyt ->
	Vk.Ppl.Cmpt.createCs
		dvc Nothing
		(U4 (computePipelineInfo pplLyt) :** HPList.Nil)
		nil \(ppl :** HPList.Nil) ->
	Vk.CmdBuf.allocate dvc (commandBufferInfo cmdPool) \(cmdBuf :*. HPList.Nil) ->
	run @nm1 @nm2 @nm3 @w1 @w2 @w3 @objss1 @objss2 @objss3 queue cmdBuf ppl pplLyt dscSet dsz

pplLayoutInfo :: Vk.DscSetLyt.D sl bts ->
	Vk.Ppl.Lyt.CreateInfo 'Nothing '[ '(sl, bts)]
		('Vk.PushConstant.Layout '[] '[])
pplLayoutInfo dsl = Vk.Ppl.Lyt.CreateInfo {
	Vk.Ppl.Lyt.createInfoNext = TMaybe.N,
	Vk.Ppl.Lyt.createInfoFlags = zeroBits,
	Vk.Ppl.Lyt.createInfoSetLayouts = U2 dsl :** HPList.Nil }

computePipelineInfo :: Vk.Ppl.Lyt.P sl sbtss '[] ->
	Vk.Ppl.Cmpt.CreateInfo 'Nothing
		'( 'Nothing, 'Nothing, 'GlslComputeShader, 'Nothing, '[Float, Float])
		'(sl, sbtss, '[]) sbph
computePipelineInfo pl = Vk.Ppl.Cmpt.CreateInfo {
	Vk.Ppl.Cmpt.createInfoNext = TMaybe.N,
	Vk.Ppl.Cmpt.createInfoFlags = def,
	Vk.Ppl.Cmpt.createInfoStage = U5 shaderStageInfo,
	Vk.Ppl.Cmpt.createInfoLayout = U3 pl,
	Vk.Ppl.Cmpt.createInfoBasePipelineHandleOrIndex = Nothing }

shaderStageInfo :: Vk.Ppl.ShaderSt.CreateInfo 'Nothing 'Nothing 'GlslComputeShader 'Nothing '[Float, Float]
shaderStageInfo = Vk.Ppl.ShaderSt.CreateInfo {
	Vk.Ppl.ShaderSt.createInfoNext = TMaybe.N,
	Vk.Ppl.ShaderSt.createInfoFlags = zeroBits,
	Vk.Ppl.ShaderSt.createInfoStage = Vk.ShaderStageComputeBit,
	Vk.Ppl.ShaderSt.createInfoModule = (shaderModInfo, nil),
	Vk.Ppl.ShaderSt.createInfoName = "main",
	Vk.Ppl.ShaderSt.createInfoSpecializationInfo = Just $ HPList.Id 3 :** HPList.Id 10 :** HPList.Nil }
	where shaderModInfo = Vk.ShaderMod.CreateInfo {
		Vk.ShaderMod.createInfoNext = TMaybe.N,
		Vk.ShaderMod.createInfoFlags = zeroBits,
		Vk.ShaderMod.createInfoCode = glslComputeShaderMain }

commandBufferInfo :: Vk.CmdPl.C s -> Vk.CmdBuf.AllocateInfo 'Nothing s '[ '()]
commandBufferInfo cmdPool = Vk.CmdBuf.AllocateInfo {
	Vk.CmdBuf.allocateInfoNext = TMaybe.N,
	Vk.CmdBuf.allocateInfoCommandPool = cmdPool,
	Vk.CmdBuf.allocateInfoLevel = Vk.CmdBuf.LevelPrimary }

run :: forall nm1 nm2 nm3 w1 w2 w3
	objss1 objss2 objss3 slbts sbtss sc sg sl sds . (
	Vk.DscSetLyt.BindingTypeListBufferOnlyDynamics (TIndex.I1_2 slbts) ~ '[ '[], '[]],
	sbtss ~ '[slbts],
	Show (HPList.PL
		(HPList.PL KObj.Length)
		(Vk.DscSetLyt.BindingTypeListBufferOnlyDynamics (TIndex.I1_2 slbts))),
	Storable w1, Storable w2, Storable w3,
	Vk.Mm.OffsetSize nm1 (Obj.List 256 w1 "") objss1 0,
	Vk.Mm.OffsetSize nm2 (Obj.List 256 w2 "") objss2 0,
	Vk.Mm.OffsetSize nm3 (Obj.List 256 w3 "") objss3 0,
	InfixIndex '[slbts] sbtss ) =>
	Vk.Q.Q -> Vk.CmdBuf.C sc ->
	Vk.Ppl.Cmpt.C sg '(sl, sbtss, '[]) ->
	Vk.Ppl.Lyt.P sl sbtss '[] -> Vk.DscSet.D sds slbts -> Word32 -> IO ()
run queue cmdBuf ppl pplLyt dscSet dsz = do
	Vk.CmdBuf.begin @'Nothing @'Nothing cmdBuf def $
		Vk.Cmd.bindPipelineCompute cmdBuf Vk.Ppl.BindPointCompute ppl \ccb -> do
			Vk.Cmd.bindDescriptorSetsCompute ccb pplLyt
				(U2 dscSet :** HPList.Nil)
				(HPList.Singleton $
					HPList.Nil :** HPList.Nil :**
					HPList.Nil)
			Vk.Cmd.dispatch ccb dsz 1 1
	Vk.Q.submit queue (U4 submitInfo :** HPList.Nil) Nothing
	Vk.Q.waitIdle queue
	where	submitInfo :: Vk.SubmitInfo 'Nothing _ _ _
		submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = TMaybe.N,
			Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
			Vk.submitInfoCommandBuffers = cmdBuf :** HPList.Nil,
			Vk.submitInfoSignalSemaphores = HPList.Nil }

withDvc :: (forall sd scpl .
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C scpl ->
	(forall c . Integral c => c) -> IO a) -> IO a
withDvc a = Vk.Inst.create instInfo nil \inst -> do
	pd <- head' <$> Vk.Phd.enumerate inst
	mgcx :. _ <- Vk.Phd.limitsMaxComputeWorkGroupCount
		. Vk.Phd.propertiesLimits <$> Vk.Phd.getProperties pd
	qfi <- fst . head' . filter (
			checkBits Vk.Q.ComputeBit .
			Vk.QFam.propertiesQueueFlags . snd )
		<$> Vk.Phd.getQueueFamilyProperties pd
	Vk.Dvc.create pd (dvcInfo qfi) nil \dv ->
		Vk.Dvc.getQueue dv qfi 0 >>= \q ->
		Vk.CmdPl.create dv (cpinfo qfi) nil \cpl ->
			a pd dv q cpl $ fromIntegral mgcx
	where
	cpinfo qfi = Vk.CmdPl.CreateInfo {
		Vk.CmdPl.createInfoNext = TMaybe.N,
		Vk.CmdPl.createInfoFlags = Vk.CmdPl.CreateResetCommandBufferBit,
		Vk.CmdPl.createInfoQueueFamilyIndex = qfi }

instInfo :: Vk.Inst.CreateInfo 'Nothing 'Nothing
instInfo = def {
	Vk.Inst.createInfoEnabledLayerNames = [Vk.layerKhronosValidation] }

dvcInfo :: Vk.QFam.Index -> Vk.Dvc.CreateInfo 'Nothing '[ 'Nothing]
dvcInfo qfi = Vk.Dvc.CreateInfo {
	Vk.Dvc.createInfoNext = TMaybe.N, Vk.Dvc.createInfoFlags = zeroBits,
	Vk.Dvc.createInfoQueueCreateInfos = HPList.Singleton qinfo,
	Vk.Dvc.createInfoEnabledLayerNames = [Vk.layerKhronosValidation],
	Vk.Dvc.createInfoEnabledExtensionNames = [],
	Vk.Dvc.createInfoEnabledFeatures = Nothing }
	where qinfo = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = zeroBits,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qfi,
		Vk.Dvc.queueCreateInfoQueuePriorities = [0] }

dscSetLayoutInfo :: Vk.DscSetLyt.CreateInfo 'Nothing '[
	'Vk.DscSetLyt.Buffer '[Obj.List 256 w1 "",Obj.List 256 w2 "",Obj.List 256 w3 ""],
	'Vk.DscSetLyt.BufferView '[ '("", MyPixel)] ]
dscSetLayoutInfo = Vk.DscSetLyt.CreateInfo {
	Vk.DscSetLyt.createInfoNext = TMaybe.N,
	Vk.DscSetLyt.createInfoFlags = def,
	Vk.DscSetLyt.createInfoBindings = binding0 :** binding1 :** HPList.Nil }

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
	Default (HPList.PL
		(HPList.PL KObj.Length)
		(Vk.DscSetLyt.BindingTypeListBufferOnlyDynamics bts)),
	Storable w1, Storable w2, Storable w3,
	Vk.DscSet.BindingAndArrayElemBufferView bts '[ '("", MyPixel)] 0,

	Vk.DscSet.BindingAndArrayElemBuffer bts
		'[Obj.List 256 w1 "",Obj.List 256 w2 "",Obj.List 256 w3 ""] 0,

	Vk.DscSet.UpdateDynamicLength bts
		'[Obj.List 256 w1 "",Obj.List 256 w2 "",Obj.List 256 w3 ""] ) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.DscSetLyt.D sl bts ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> V.Vector MyPixel -> Word32 -> (
		forall sds sm1 sb1 sm2 sb2 sm3 sb3 .
		Vk.DscSet.D sds '(sl, bts) ->
		Vk.Mm.M sm1 '[ '( sb1, 'Vk.Mm.BufferArg nm1 '[Obj.List 256 w1 ""])] ->
		Vk.Mm.M sm2 '[ '( sb2, 'Vk.Mm.BufferArg nm2 '[Obj.List 256 w2 ""])] ->
		Vk.Mm.M sm3 '[ '( sb3, 'Vk.Mm.BufferArg nm3 '[Obj.List 256 w3 ""])] ->
		IO a) -> IO a
prepareMems phdvc dvc dscSetLyt da db dc dd _mxx f =
	Vk.DscPool.create dvc dscPoolInfo nil \dscPool ->
	Vk.DscSet.allocateDs dvc (dscSetInfo dscPool dscSetLyt)
		\(dscSet :** HPList.Nil) ->
	storageBufferNew4 dvc phdvc da db dc dd \ba ma bb mb bc mc
		(bd :: Vk.Buffer.Binded sm sb nm '[Obj.List 256 MyPixel ""]) _md ->
	let	bufferViewInfo' :: Vk.BufferView.CreateInfo 'Nothing MyPixel ""
			'(sm, sb, nm, '[Obj.List 256 MyPixel ""])
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
				. HPList.Singleton $ U3 bv }
	Vk.DscSet.updateDs dvc (
		U5 (writeDscSet @w1 @w2 @w3 dscSet ba bb bc) :** U5 wds' :**
		HPList.Nil )
		HPList.Nil
	f dscSet ma mb mc

data MyPixel = MyPixel Float Float Float Float deriving Show

type instance Vk.BufferView.FormatOf MyPixel = 'Vk.TEnum.FormatR32g32b32a32Sfloat

instance Storable MyPixel where
	sizeOf _ = 4 * sizeOf @Float undefined
	alignment _ = alignment @Float undefined
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
	Vk.DscSet.allocateInfoSetLayouts = U2 lyt :** HPList.Nil }

writeDscSet ::
	forall w1 w2 w3 slbts sb1 sb2 sb3 sm1 sm2 sm3 nm1 nm2 nm3 objs1 objs2 objs3 sds . (
	Show (HPList.PL Obj.Length objs1),
	Show (HPList.PL Obj.Length objs2),
	Show (HPList.PL Obj.Length objs3),
	Obj.OffsetRange (Obj.List 256 w1 "") objs1 0,
	Obj.OffsetRange (Obj.List 256 w2 "") objs2 0,
	Obj.OffsetRange (Obj.List 256 w3 "") objs3 0 ) =>
	Vk.DscSet.D sds slbts ->
	Vk.Buffer.Binded sm1 sb1 nm1 objs1 -> Vk.Buffer.Binded sm2 sb2 nm2 objs2 ->
	Vk.Buffer.Binded sm3 sb3 nm3 objs3 ->
	Vk.DscSet.Write 'Nothing sds slbts ('Vk.DscSet.WriteSourcesArgBuffer '[
		'(sm1, sb1, nm1, Obj.List 256 w1 "", 0), '(sm2, sb2, nm2, Obj.List 256 w2 "", 0),
		'(sm3, sb3, nm3, Obj.List 256 w3 "", 0) ]) 0
writeDscSet ds ba bb bc = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = ds,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		U5 (bufferInfoList @w1 ba) :** U5 (bufferInfoList @w2 bb) :**
		U5 (bufferInfoList @w3 bc) :** HPList.Nil }

bufferInfoList :: forall t {sb} {sm} {nm} {objs} . (
	Show (HPList.PL Obj.Length objs),
	Obj.OffsetRange (Obj.List 256 t "") objs 0 ) =>
	Vk.Buffer.Binded sm sb nm objs ->
	Vk.Dsc.BufferInfo sm sb nm (Obj.List 256 t "") 0
bufferInfoList = Vk.Dsc.BufferInfo

storageBufferNew4 :: (Storable w1, Storable w2, Storable w3, Storable w4) =>
	Vk.Dvc.D sd -> Vk.Phd.P ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> V.Vector w4 -> (
		forall sb1 sm1 sb2 sm2 sb3 sm3 sb4 sm4 .
		Vk.Buffer.Binded sm1 sb1 nm1 '[Obj.List 256 w1 ""] ->
		Vk.Mm.M sm1 '[ '(sb1, 'Vk.Mm.BufferArg nm1 '[Obj.List 256 w1 ""])] ->
		Vk.Buffer.Binded sm2 sb2 nm2 '[Obj.List 256 w2 ""] ->
		Vk.Mm.M sm2 '[ '(sb2, 'Vk.Mm.BufferArg nm2 '[Obj.List 256 w2 ""])] ->
		Vk.Buffer.Binded sm3 sb3 nm3 '[Obj.List 256 w3 ""] ->
		Vk.Mm.M sm3 '[ '(sb3, 'Vk.Mm.BufferArg nm3 '[Obj.List 256 w3 ""])] ->
		Vk.Buffer.Binded sm4 sb4 nm4 '[Obj.List 256 w4 ""] ->
		Vk.Mm.M sm4 '[ '(sb4, 'Vk.Mm.BufferArg nm4 '[Obj.List 256 w4 ""])] ->
		IO a ) -> IO a
storageBufferNew4 dvc phdvc x y z w f = storageBufferNews
	dvc phdvc (x :** y :** z :** w :** HPList.Nil) $ addArg4 f

addArg4 :: (forall sb1 sm1 sb2 sm2 sb3 sm3 sb4 sm4 .
	Vk.Buffer.Binded sm1 sb1 nm1 '[Obj.List 256 w1 ""] ->
	Vk.Mm.M sm1 '[ '(sb1, 'Vk.Mm.BufferArg nm1 '[Obj.List 256 w1 ""])] ->
	Vk.Buffer.Binded sm2 sb2 nm2 '[Obj.List 256 w2 ""] ->
	Vk.Mm.M sm2 '[ '(sb2, 'Vk.Mm.BufferArg nm2 '[Obj.List 256 w2 ""])] ->
	Vk.Buffer.Binded sm3 sb3 nm3 '[Obj.List 256 w3 ""] ->
	Vk.Mm.M sm3 '[ '(sb3, 'Vk.Mm.BufferArg nm3 '[Obj.List 256 w3 ""])] ->
	Vk.Buffer.Binded sm4 sb4 nm4 '[Obj.List 256 w4 ""] ->
	Vk.Mm.M sm4 '[ '(sb4, 'Vk.Mm.BufferArg nm4 '[Obj.List 256 w4 ""])] ->
	r) -> Arg nm1 w1 (Arg nm2 w2 (Arg nm3 w3 (Arg nm4 w4 r)))
addArg4 f = Arg \b1 m1 ->
	Arg \b2 m2 -> Arg \b3 m3 -> Arg \b4 m4 -> f b1 m1 b2 m2 b3 m3 b4 m4

storageBufferNew3 :: (Storable w1, Storable w2, Storable w3) =>
	Vk.Dvc.D sd -> Vk.Phd.P ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (
		forall sb1 sm1 sb2 sm2 sb3 sm3 .
		Vk.Buffer.Binded sm1 sb1 nm1 '[Obj.List 256 w1 ""] ->
		Vk.Mm.M sm1 '[ '(sb1, 'Vk.Mm.BufferArg nm1 '[Obj.List 256 w1 ""])] ->
		Vk.Buffer.Binded sm2 sb2 nm2 '[Obj.List 256 w2 ""] ->
		Vk.Mm.M sm2 '[ '(sb2, 'Vk.Mm.BufferArg nm2 '[Obj.List 256 w2 ""])] ->
		Vk.Buffer.Binded sm3 sb3 nm3 '[Obj.List 256 w3 ""] ->
		Vk.Mm.M sm3 '[ '(sb3, 'Vk.Mm.BufferArg nm3 '[Obj.List 256 w3 ""])] -> IO a ) -> IO a
storageBufferNew3 dvc phdvc x y z f =
	storageBufferNews dvc phdvc (x :** y :** z :** HPList.Nil) $ addArg3 f

addArg3 :: (forall sb1 sm1 sb2 sm2 sb3 sm3 .
	Vk.Buffer.Binded sm1 sb1 nm1 '[Obj.List 256 w1 ""] ->
	Vk.Mm.M sm1 '[ '(sb1, 'Vk.Mm.BufferArg nm1 '[Obj.List 256 w1 ""])] ->
	Vk.Buffer.Binded sm2 sb2 nm2 '[Obj.List 256 w2 ""] ->
	Vk.Mm.M sm2 '[ '(sb2, 'Vk.Mm.BufferArg nm2 '[Obj.List 256 w2 ""])] ->
	Vk.Buffer.Binded sm3 sb3 nm3 '[Obj.List 256 w3 ""] ->
	Vk.Mm.M sm3 '[ '(sb3, 'Vk.Mm.BufferArg nm3 '[Obj.List 256 w3 ""])] -> r) ->
	Arg nm1 w1 (Arg nm2 w2 (Arg nm3 w3 r))
addArg3 f = Arg \b1 m1 -> Arg \b2 m2 -> Arg \b3 m3 -> f b1 m1 b2 m2 b3 m3

class StorageBufferNews f a where
	type Vectors f :: [Type]
	storageBufferNews :: Vk.Dvc.D sd -> Vk.Phd.P ->
		HPList.PL V.Vector (Vectors f) -> f -> IO a

data Arg nm w f = Arg (forall sb sm .
	Vk.Buffer.Binded sm sb nm '[Obj.List 256 w ""] ->
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[Obj.List 256 w ""])] -> f)

instance StorageBufferNews (IO a) a where
	type Vectors (IO a) = '[]
	storageBufferNews _dvc _phdvc HPList.Nil f = f

instance (Storable w, StorageBufferNews f a) =>
	StorageBufferNews (Arg nm w f) a where
	type Vectors (Arg nm w f) = w ': Vectors f
	storageBufferNews dvc phdvc (vs :** vss) (Arg f) =
		storageBufferNew dvc phdvc vs \buf mem ->
		storageBufferNews @f @a dvc phdvc vss (f buf mem)

storageBufferNew :: forall sd nm w a . Storable w =>
	Vk.Dvc.D sd -> Vk.Phd.P -> V.Vector w -> (
		forall sb sm .
		Vk.Buffer.Binded sm sb nm '[Obj.List 256 w ""]  ->
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[Obj.List 256 w ""])] -> IO a ) -> IO a
storageBufferNew dvc phdvc xs f =
	Vk.Buffer.create dvc (bufferInfo xs) nil \buffer -> do
		memoryInfo <- getMemoryInfo phdvc dvc buffer
		Vk.Mm.allocateBind dvc (U2 (Vk.Mm.Buffer buffer) :** HPList.Nil) memoryInfo
			nil \(U2 (Vk.Mm.BufferBinded binded) :** HPList.Nil) memory -> do
			Vk.Mm.write @nm @(Obj.List 256 w "") @0 dvc memory def xs
			f binded memory

bufferInfo :: Storable w => V.Vector w -> Vk.Buffer.CreateInfo 'Nothing '[Obj.List 256 w ""]
bufferInfo xs = Vk.Buffer.CreateInfo {
	Vk.Buffer.createInfoNext = TMaybe.N,
	Vk.Buffer.createInfoFlags = def,
	Vk.Buffer.createInfoLengths =
		Obj.LengthList (fromIntegral $ V.length xs) :** HPList.Nil,
	Vk.Buffer.createInfoUsage =
		Vk.Buffer.UsageStorageBufferBit .|.
		Vk.Buffer.UsageStorageTexelBufferBit,
	Vk.Buffer.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Buffer.createInfoQueueFamilyIndices = [] }

getMemoryInfo :: Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Buffer.B sb nm objs ->
	IO (Vk.Mm.AllocateInfo 'Nothing)
getMemoryInfo phdvc dvc buffer = do
	requirements <- Vk.Buffer.getMemoryRequirements dvc buffer
	memTypeIdx <- findMemoryTypeIndex phdvc requirements (
		Vk.Mm.PropertyHostVisibleBit .|.
		Vk.Mm.PropertyHostCoherentBit )
	pure Vk.Mm.AllocateInfo {
		Vk.Mm.allocateInfoNext = TMaybe.N,
		Vk.Mm.allocateInfoMemoryTypeIndex = memTypeIdx }

findMemoryTypeIndex ::
	Vk.Phd.P -> Vk.Mm.M.Requirements -> Vk.Mm.PropertyFlags ->
	IO Vk.Mm.M.TypeIndex
findMemoryTypeIndex physicalDevice requirements memoryProp = do
	memoryProperties <- Vk.Phd.getMemoryProperties physicalDevice
	let	reqTypes = Vk.Mm.M.requirementsMemoryTypeBits requirements
		memPropTypes = (fst <$>)
			. filter (checkBits memoryProp
				. Vk.Mm.M.mTypePropertyFlags . snd)
			$ Vk.Phd.memoryPropertiesMemoryTypes memoryProperties
	case filter (`Vk.Mm.M.elemTypeIndex` reqTypes) memPropTypes of
		[] -> error "No available memory types"
		i : _ -> pure i

checkBits :: Bits bs => bs -> bs -> Bool
checkBits bs0 = (== bs0) . (.&. bs0)

head' :: [a] -> a
head' = \case [] -> error "empty list"; x : _ -> x

[glslComputeShader|

#version 460
layout(local_size_x = 1, local_size_y = 1) in;
layout(binding = 0) buffer Data {
	float val[];
} data[3];

layout(constant_id = 0) const uint sc = 2;
layout(constant_id = 1) const uint sc2 = 3;

layout(binding = 1, rgba32f) uniform imageBuffer storageTexelBuffer;

void
main()
{
	int index = int(gl_GlobalInvocationID.x);
	vec4 some = imageLoad(storageTexelBuffer, index);
//	data[2].val[index] = (data[0].val[index] + data[1].val[index]) * sc * sc2;
	data[0].val[index] = some.x;
	data[1].val[index] = some.y;
	data[2].val[index] = some.z;
}

|]
