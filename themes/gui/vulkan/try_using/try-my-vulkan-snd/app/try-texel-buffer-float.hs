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
import Gpu.Vulkan.Object.Base qualified as BObj
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
import qualified Gpu.Vulkan.DescriptorPool as Vk.DscPl
import qualified Gpu.Vulkan.ShaderModule as Vk.ShaderMod
import qualified "try-gpu-vulkan" Gpu.Vulkan.Pipeline as Vk.Ppl
import qualified Gpu.Vulkan.PipelineLayout as Vk.Ppl.Lyt
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShaderSt
import qualified Gpu.Vulkan.Pipeline.Compute as Vk.Ppl.Cmpt
import qualified Gpu.Vulkan.DescriptorSet as Vk.DscSt
import qualified Gpu.Vulkan.CommandBuffer as Vk.CmdBuf
import qualified Gpu.Vulkan.Cmd as Vk.Cmd

import qualified Gpu.Vulkan.Buffer as Vk.Bffr
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscStLyt

import qualified Gpu.Vulkan.BufferView as Vk.BffrVw
import qualified Gpu.Vulkan.PushConstant as Vk.PushConstant

import Gpu.Vulkan.TypeEnum qualified as Vk.TEnum

import Data.Tuple.ToolsYj

main :: IO ()
main = withDvc \pd d q cpl mgcx ->
	let	da = V.replicate mgcx 0
		db = V.replicate mgcx 0
		dc = V.replicate mgcx 0
		dd = V.fromList . take mgcx $ cycle [
			Pixel 123 321 111 333, Pixel 555 444 333 222,
			Pixel 999 888 777 666 ] in
	Vk.DscStLyt.create d dslinfo nil \dsl ->
	createBffr4Mm4 pd d dsl da db dc dd \dss
		(ma :: Mm sm1 sb1 nm1 W1) (mb :: Mm sm2 sb2 nm2 W2)
		(mc :: Mm sm3 sb3 nm3 W3) -> do
	calc @nm1 @nm2 @nm3 @W1 @W2 @W3
		@(MmBffrArg sb1 nm1 W1) @(MmBffrArg sb2 nm2 W2)
		@(MmBffrArg sb3 nm3 W3) d q cpl dsl dss mgcx
	mapTup3M_ (print . take 20)
		. appTup3 (unW1 <$>) (unW2 <$>) (unW3 <$>) =<< (,,)
			<$> Vk.Mm.read @nm1 @(OList W1) @0 @[W1] d ma zeroBits
			<*> Vk.Mm.read @nm2 @(OList W2) @0 @[W2] d mb zeroBits
			<*> Vk.Mm.read @nm3 @(OList W3) @0 @[W3] d mc zeroBits
	where
	dslinfo :: Vk.DscStLyt.CreateInfo 'Nothing (DscStLytArg W1 W2 W3)
	dslinfo = Vk.DscStLyt.CreateInfo {
		Vk.DscStLyt.createInfoNext = TMaybe.N,
		Vk.DscStLyt.createInfoFlags = zeroBits,
		Vk.DscStLyt.createInfoBindings = bdg0 :** bdg1 :** HPList.Nil }
	bdg0 = Vk.DscStLyt.BindingBuffer {
		Vk.DscStLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeStorageBuffer,
		Vk.DscStLyt.bindingBufferStageFlags = Vk.ShaderStageComputeBit }
	bdg1 = Vk.DscStLyt.BindingBufferView {
		Vk.DscStLyt.bindingBufferViewDescriptorType =
			Vk.Dsc.TypeStorageTexelBuffer,
		Vk.DscStLyt.bindingBufferViewStageFlags =
			Vk.ShaderStageComputeBit }

newtype W1 = W1 { unW1 :: Float } deriving (Show, Storable, Num)
newtype W2 = W2 { unW2 :: Float } deriving (Show, Storable, Num)
newtype W3 = W3 { unW3 :: Float } deriving (Show, Storable, Num)

type DscStLytArg w1 w2 w3 = '[
	'Vk.DscStLyt.Buffer '[OList w1, OList w2, OList w3],
	'Vk.DscStLyt.BufferView '[ '("", Pixel)] ]
type Mm sm sb nm w = Vk.Mm.M sm (MmBffrArg sb nm w)
type MmBffrArg sb nm w = '[ '(sb, 'Vk.Mm.BufferArg nm '[OList w])]
type OList t = Obj.List 256 t ""

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

-- PREPARE BUFFERS AND MEMORIES

createBffr4Mm4 :: forall nm1 nm2 nm3 w1 w2 w3 sd sdsl bts a . (
	Storable w1, Storable w2, Storable w3,
	Vk.DscSt.BindingAndArrayElemBufferView bts '[ '("", Pixel)] 0,
	Vk.DscSt.BindingAndArrayElemBuffer
		bts '[OList w1, OList w2, OList w3] 0,
	Vk.DscSt.UpdateDynamicLength bts '[OList w1, OList w2, OList w3],
	Default (HPList.PL2 BObj.Length
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts)) ) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.DscStLyt.D sdsl bts ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> V.Vector Pixel -> (
		forall sds sm1 sm2 sm3 sb1 sb2 sb3 .
		Vk.DscSt.D sds '(sdsl, bts) ->
		Mm sm1 sb1 nm1 w1 -> Mm sm2 sb2 nm2 w2 -> Mm sm3 sb3 nm3 w3 ->
		IO a ) -> IO a
createBffr4Mm4 pd dv dsl da db dc dd a =
	Vk.DscPl.create dv dscPlInfo nil \dpl ->
	Vk.DscSt.allocateDs dv (dscStInfo dpl dsl) \(HPList.Singleton dss) ->
	bffr4Mm4 pd dv da db dc dd \ba bb bc bd ma mb mc _md ->
	Vk.BffrVw.create dv (bffrVwInfo bd) nil \bv ->
	Vk.DscSt.updateDs dv (
		U5 (writeDscStBffr3 @w1 @w2 @w3 dss ba bb bc) :**
		U5 (writeDscStBffrVw dss bv) :** HPList.Nil ) HPList.Nil >>
	a dss ma mb mc

bffrVwInfo :: Vk.Bffr.Binded sm sb nm '[OList Pixel] ->
	Vk.BffrVw.CreateInfo 'Nothing Pixel "" '(sm, sb, nm, '[OList Pixel])
bffrVwInfo bd = Vk.BffrVw.CreateInfo {
	Vk.BffrVw.createInfoNext = TMaybe.N,
	Vk.BffrVw.createInfoFlags = zeroBits,
	Vk.BffrVw.createInfoBuffer = U4 bd }

dscPlInfo :: Vk.DscPl.CreateInfo 'Nothing
dscPlInfo = Vk.DscPl.CreateInfo {
	Vk.DscPl.createInfoNext = TMaybe.N,
	Vk.DscPl.createInfoFlags = Vk.DscPl.CreateFreeDescriptorSetBit,
	Vk.DscPl.createInfoMaxSets = 1,
	Vk.DscPl.createInfoPoolSizes = [plszsb, plszstb] }
	where
	plszsb = Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeStorageBuffer,
		Vk.DscPl.sizeDescriptorCount = 3 }
	plszstb = Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeStorageTexelBuffer,
		Vk.DscPl.sizeDescriptorCount = 1 }

dscStInfo :: Vk.DscPl.P sp -> Vk.DscStLyt.D sl bts ->
	Vk.DscSt.AllocateInfo 'Nothing sp '[ '(sl, bts)]
dscStInfo dpl dsl = Vk.DscSt.AllocateInfo {
	Vk.DscSt.allocateInfoNext = TMaybe.N,
	Vk.DscSt.allocateInfoDescriptorPool = dpl,
	Vk.DscSt.allocateInfoSetLayouts = HPList.Singleton $ U2 dsl }

-- CALC

calc :: forall nm1 nm2 nm3 w1 w2 w3 objss1 objss2 objss3
		slbts sl bts sd sds scpl . (
	Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[], '[]],
	slbts ~ '(sl, bts),
	Show (HPList.PL
		(HPList.PL BObj.Length)
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics (TIndex.I1_2 slbts))),
	Storable w1, Storable w2, Storable w3,
	Vk.Mm.OffsetSize nm1 (Obj.List 256 w1 "") objss1 0,
	Vk.Mm.OffsetSize nm2 (Obj.List 256 w2 "") objss2 0,
	Vk.Mm.OffsetSize nm3 (Obj.List 256 w3 "") objss3 0,
	InfixIndex '[slbts] '[ '(sl, bts)]) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C scpl ->
	Vk.DscStLyt.D sl bts ->
	Vk.DscSt.D sds slbts -> Word32 -> IO ()
calc dvc queue cmdPool dsl dscSet dsz =
	Vk.Ppl.Lyt.create dvc (pplLayoutInfo dsl) nil \pplLyt ->
	Vk.Ppl.Cmpt.createCs
		dvc Nothing
		(U4 (computePipelineInfo pplLyt) :** HPList.Nil)
		nil \(ppl :** HPList.Nil) ->
	Vk.CmdBuf.allocate dvc (commandBufferInfo cmdPool) \(cmdBuf :*. HPList.Nil) ->
	run @nm1 @nm2 @nm3 @w1 @w2 @w3 @objss1 @objss2 @objss3 queue cmdBuf ppl pplLyt dscSet dsz

pplLayoutInfo :: Vk.DscStLyt.D sl bts ->
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
	Vk.Ppl.Cmpt.createInfoFlags = zeroBits,
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
	Vk.DscStLyt.BindingTypeListBufferOnlyDynamics (TIndex.I1_2 slbts) ~ '[ '[], '[]],
	sbtss ~ '[slbts],
	Show (HPList.PL
		(HPList.PL BObj.Length)
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics (TIndex.I1_2 slbts))),
	Storable w1, Storable w2, Storable w3,
	Vk.Mm.OffsetSize nm1 (Obj.List 256 w1 "") objss1 0,
	Vk.Mm.OffsetSize nm2 (Obj.List 256 w2 "") objss2 0,
	Vk.Mm.OffsetSize nm3 (Obj.List 256 w3 "") objss3 0,
	InfixIndex '[slbts] sbtss ) =>
	Vk.Q.Q -> Vk.CmdBuf.C sc ->
	Vk.Ppl.Cmpt.C sg '(sl, sbtss, '[]) ->
	Vk.Ppl.Lyt.P sl sbtss '[] -> Vk.DscSt.D sds slbts -> Word32 -> IO ()
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

data Pixel = Pixel Float Float Float Float deriving Show

type instance Vk.BffrVw.FormatOf Pixel = 'Vk.TEnum.FormatR32g32b32a32Sfloat

instance Storable Pixel where
	sizeOf _ = 4 * sizeOf @Float undefined
	alignment _ = alignment @Float undefined
	peek p = do
		[r, g, b, a] <- peekArray 4 $ castPtr p
		pure $ Pixel r g b a
	poke p (Pixel r g b a) = pokeArray (castPtr p) [r, g, b, a]

writeDscStBffr3 ::
	forall w1 w2 w3 slbts sb1 sb2 sb3 sm1 sm2 sm3 nm1 nm2 nm3 objs1 objs2 objs3 sds . (
	Show (HPList.PL Obj.Length objs1),
	Show (HPList.PL Obj.Length objs2),
	Show (HPList.PL Obj.Length objs3),
	Obj.OffsetRange (Obj.List 256 w1 "") objs1 0,
	Obj.OffsetRange (Obj.List 256 w2 "") objs2 0,
	Obj.OffsetRange (Obj.List 256 w3 "") objs3 0 ) =>
	Vk.DscSt.D sds slbts ->
	Vk.Bffr.Binded sm1 sb1 nm1 objs1 -> Vk.Bffr.Binded sm2 sb2 nm2 objs2 ->
	Vk.Bffr.Binded sm3 sb3 nm3 objs3 ->
	Vk.DscSt.Write 'Nothing sds slbts ('Vk.DscSt.WriteSourcesArgBuffer '[
		'(sm1, sb1, nm1, Obj.List 256 w1 "", 0), '(sm2, sb2, nm2, Obj.List 256 w2 "", 0),
		'(sm3, sb3, nm3, Obj.List 256 w3 "", 0) ]) 0
writeDscStBffr3 ds ba bb bc = Vk.DscSt.Write {
	Vk.DscSt.writeNext = TMaybe.N,
	Vk.DscSt.writeDstSet = ds,
	Vk.DscSt.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSt.writeSources = Vk.DscSt.BufferInfos $
		U5 (bufferInfoList @w1 ba) :** U5 (bufferInfoList @w2 bb) :**
		U5 (bufferInfoList @w3 bc) :** HPList.Nil }

writeDscStBffrVw :: Vk.DscSt.D sds sdslbts -> Vk.BffrVw.B sbv bvnm fmt ->
	Vk.DscSt.Write _ _ _ _ 0
writeDscStBffrVw dss bv = Vk.DscSt.Write {
	Vk.DscSt.writeNext = TMaybe.N,
	Vk.DscSt.writeDstSet = dss,
	Vk.DscSt.writeDescriptorType = Vk.Dsc.TypeStorageTexelBuffer,
	Vk.DscSt.writeSources =
		Vk.DscSt.TexelBufferViews . HPList.Singleton $ U3 bv }

bufferInfoList :: forall t {sb} {sm} {nm} {objs} . (
	Show (HPList.PL Obj.Length objs),
	Obj.OffsetRange (Obj.List 256 t "") objs 0 ) =>
	Vk.Bffr.Binded sm sb nm objs ->
	Vk.Dsc.BufferInfo sm sb nm (Obj.List 256 t "") 0
bufferInfoList = Vk.Dsc.BufferInfo

bffr4Mm4 :: (Storable w1, Storable w2, Storable w3, Storable w4) =>
	Vk.Phd.P ->
	Vk.Dvc.D sd ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> V.Vector w4 -> (
		forall sb1 sm1 sb2 sm2 sb3 sm3 sb4 sm4 .
		Vk.Bffr.Binded sm1 sb1 nm1 '[Obj.List 256 w1 ""] ->
		Vk.Bffr.Binded sm2 sb2 nm2 '[Obj.List 256 w2 ""] ->
		Vk.Bffr.Binded sm3 sb3 nm3 '[Obj.List 256 w3 ""] ->
		Vk.Bffr.Binded sm4 sb4 nm4 '[Obj.List 256 w4 ""] ->
		Vk.Mm.M sm1 '[ '(sb1, 'Vk.Mm.BufferArg nm1 '[Obj.List 256 w1 ""])] ->
		Vk.Mm.M sm2 '[ '(sb2, 'Vk.Mm.BufferArg nm2 '[Obj.List 256 w2 ""])] ->
		Vk.Mm.M sm3 '[ '(sb3, 'Vk.Mm.BufferArg nm3 '[Obj.List 256 w3 ""])] ->
		Vk.Mm.M sm4 '[ '(sb4, 'Vk.Mm.BufferArg nm4 '[Obj.List 256 w4 ""])] ->
		IO a ) -> IO a
bffr4Mm4 pd dvc x y z w f = storageBufferNews
	dvc pd (x :** y :** z :** w :** HPList.Nil) $ addArg4 f

addArg4 :: (forall sb1 sm1 sb2 sm2 sb3 sm3 sb4 sm4 .
	Vk.Bffr.Binded sm1 sb1 nm1 '[Obj.List 256 w1 ""] ->
	Vk.Bffr.Binded sm2 sb2 nm2 '[Obj.List 256 w2 ""] ->
	Vk.Bffr.Binded sm3 sb3 nm3 '[Obj.List 256 w3 ""] ->
	Vk.Bffr.Binded sm4 sb4 nm4 '[Obj.List 256 w4 ""] ->
	Vk.Mm.M sm1 '[ '(sb1, 'Vk.Mm.BufferArg nm1 '[Obj.List 256 w1 ""])] ->
	Vk.Mm.M sm2 '[ '(sb2, 'Vk.Mm.BufferArg nm2 '[Obj.List 256 w2 ""])] ->
	Vk.Mm.M sm3 '[ '(sb3, 'Vk.Mm.BufferArg nm3 '[Obj.List 256 w3 ""])] ->
	Vk.Mm.M sm4 '[ '(sb4, 'Vk.Mm.BufferArg nm4 '[Obj.List 256 w4 ""])] ->
	r) -> Arg nm1 w1 (Arg nm2 w2 (Arg nm3 w3 (Arg nm4 w4 r)))
addArg4 f = Arg \b1 m1 ->
	Arg \b2 m2 -> Arg \b3 m3 -> Arg \b4 m4 -> f b1 b2 b3 b4 m1 m2 m3 m4

storageBufferNew3 :: (Storable w1, Storable w2, Storable w3) =>
	Vk.Dvc.D sd -> Vk.Phd.P ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (
		forall sb1 sm1 sb2 sm2 sb3 sm3 .
		Vk.Bffr.Binded sm1 sb1 nm1 '[Obj.List 256 w1 ""] ->
		Vk.Mm.M sm1 '[ '(sb1, 'Vk.Mm.BufferArg nm1 '[Obj.List 256 w1 ""])] ->
		Vk.Bffr.Binded sm2 sb2 nm2 '[Obj.List 256 w2 ""] ->
		Vk.Mm.M sm2 '[ '(sb2, 'Vk.Mm.BufferArg nm2 '[Obj.List 256 w2 ""])] ->
		Vk.Bffr.Binded sm3 sb3 nm3 '[Obj.List 256 w3 ""] ->
		Vk.Mm.M sm3 '[ '(sb3, 'Vk.Mm.BufferArg nm3 '[Obj.List 256 w3 ""])] -> IO a ) -> IO a
storageBufferNew3 dvc pd x y z f =
	storageBufferNews dvc pd (x :** y :** z :** HPList.Nil) $ addArg3 f

addArg3 :: (forall sb1 sm1 sb2 sm2 sb3 sm3 .
	Vk.Bffr.Binded sm1 sb1 nm1 '[Obj.List 256 w1 ""] ->
	Vk.Mm.M sm1 '[ '(sb1, 'Vk.Mm.BufferArg nm1 '[Obj.List 256 w1 ""])] ->
	Vk.Bffr.Binded sm2 sb2 nm2 '[Obj.List 256 w2 ""] ->
	Vk.Mm.M sm2 '[ '(sb2, 'Vk.Mm.BufferArg nm2 '[Obj.List 256 w2 ""])] ->
	Vk.Bffr.Binded sm3 sb3 nm3 '[Obj.List 256 w3 ""] ->
	Vk.Mm.M sm3 '[ '(sb3, 'Vk.Mm.BufferArg nm3 '[Obj.List 256 w3 ""])] -> r) ->
	Arg nm1 w1 (Arg nm2 w2 (Arg nm3 w3 r))
addArg3 f = Arg \b1 m1 -> Arg \b2 m2 -> Arg \b3 m3 -> f b1 m1 b2 m2 b3 m3

class StorageBufferNews f a where
	type Vectors f :: [Type]
	storageBufferNews :: Vk.Dvc.D sd -> Vk.Phd.P ->
		HPList.PL V.Vector (Vectors f) -> f -> IO a

data Arg nm w f = Arg (forall sb sm .
	Vk.Bffr.Binded sm sb nm '[Obj.List 256 w ""] ->
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[Obj.List 256 w ""])] -> f)

instance StorageBufferNews (IO a) a where
	type Vectors (IO a) = '[]
	storageBufferNews _dvc _pd HPList.Nil f = f

instance (Storable w, StorageBufferNews f a) =>
	StorageBufferNews (Arg nm w f) a where
	type Vectors (Arg nm w f) = w ': Vectors f
	storageBufferNews dvc pd (vs :** vss) (Arg f) =
		storageBufferNew dvc pd vs \buf mem ->
		storageBufferNews @f @a dvc pd vss (f buf mem)

storageBufferNew :: forall sd nm w a . Storable w =>
	Vk.Dvc.D sd -> Vk.Phd.P -> V.Vector w -> (
		forall sb sm .
		Vk.Bffr.Binded sm sb nm '[Obj.List 256 w ""]  ->
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[Obj.List 256 w ""])] -> IO a ) -> IO a
storageBufferNew dvc pd xs f =
	Vk.Bffr.create dvc (bufferInfo xs) nil \buffer -> do
		memoryInfo <- getMemoryInfo pd dvc buffer
		Vk.Mm.allocateBind dvc (U2 (Vk.Mm.Buffer buffer) :** HPList.Nil) memoryInfo
			nil \(U2 (Vk.Mm.BufferBinded binded) :** HPList.Nil) memory -> do
			Vk.Mm.write @nm @(Obj.List 256 w "") @0 dvc memory def xs
			f binded memory

bufferInfo :: Storable w => V.Vector w -> Vk.Bffr.CreateInfo 'Nothing '[Obj.List 256 w ""]
bufferInfo xs = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N,
	Vk.Bffr.createInfoFlags = def,
	Vk.Bffr.createInfoLengths =
		Obj.LengthList (fromIntegral $ V.length xs) :** HPList.Nil,
	Vk.Bffr.createInfoUsage =
		Vk.Bffr.UsageStorageBufferBit .|.
		Vk.Bffr.UsageStorageTexelBufferBit,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

getMemoryInfo :: Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Bffr.B sb nm objs ->
	IO (Vk.Mm.AllocateInfo 'Nothing)
getMemoryInfo pd dvc buffer = do
	requirements <- Vk.Bffr.getMemoryRequirements dvc buffer
	memTypeIdx <- findMemoryTypeIndex pd requirements (
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
