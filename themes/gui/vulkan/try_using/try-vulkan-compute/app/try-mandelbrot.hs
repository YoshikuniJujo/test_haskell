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

import Gpu.Vulkan.Object qualified as Obj
import Gpu.Vulkan.Object.Base qualified as KObj
import Data.Default
import Data.Bits
import Data.Bits.ToolsYj
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import qualified Data.HeteroParList as HPList
import Data.HeteroParList (pattern(:*), pattern (:*.))
import Data.Word

import Language.SpirV.Shaderc.TH
import Language.SpirV.ShaderKind

import qualified Gpu.Vulkan as Vk
import qualified Gpu.Vulkan.Instance as Vk.Inst
import qualified Gpu.Vulkan.PhysicalDevice as Vk.Phd
import qualified Gpu.Vulkan.Queue as Vk.Q
import qualified Gpu.Vulkan.QueueFamily as Vk.QFm
import qualified Gpu.Vulkan.Device as Vk.Dvc
import qualified Gpu.Vulkan.CommandPool as Vk.CmdPl
import qualified Gpu.Vulkan.Memory as Vk.Mm
import qualified Gpu.Vulkan.Descriptor as Vk.Dsc
import qualified Gpu.Vulkan.DescriptorPool as Vk.DscPl
import qualified Gpu.Vulkan.ShaderModule as Vk.ShaderMod
import qualified Gpu.Vulkan.Pipeline as Vk.Ppl
import qualified Gpu.Vulkan.PipelineLayout as Vk.PplLyt
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShdrSt
import qualified Gpu.Vulkan.Pipeline.Compute as Vk.Ppl.Cmpt
import qualified Gpu.Vulkan.DescriptorSet as Vk.DscSt
import qualified Gpu.Vulkan.CommandBuffer as Vk.CBffr
import qualified Gpu.Vulkan.Cmd as Vk.Cmd

import qualified Gpu.Vulkan.Buffer as Vk.Bffr
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscStLyt

import qualified Gpu.Vulkan.PushConstant as Vk.PushConstant

import Data.Vector.Storable qualified as V

import Gpu.Vulkan.TypeEnum qualified as Vk.T

import Foreign.Storable.Generic qualified as GStr

import Gpu.Vulkan.Fence qualified as Vk.Fnc
import Data.Complex

import Mandelbrot.Draw
import Tools

---------------------------------------------------------------------------

-- MAIN
-- PREPARE MEMORIES
-- CALC
-- COMPUTE PIPELINE INFO

---------------------------------------------------------------------------

type PictureSize = GStr.W (Word32, Word32)

-- MAIN

main :: IO ()
main = draw "autogen/mandelbrot.png" render

render :: Size -> Complex Float -> Complex Float -> IO (V.Vector Word8)
render (w, h) lt rb = (fromIntegral `V.map`) <$> withDvc \pd dv q cb ->
	Vk.DscStLyt.create dv dslinfo nil \dsl ->
	prepareMm (fromIntegral w) (fromIntegral h) pd dv dsl \ds m ->
	calc w h lt rb dv q cb dsl ds >>
	Vk.Mm.read @"" @Word32List @0 dv m zeroBits
	where
	dslinfo :: Vk.DscStLyt.CreateInfo
		'Nothing '[ 'Vk.DscStLyt.Buffer '[Word32List]]
	dslinfo = Vk.DscStLyt.CreateInfo {
		Vk.DscStLyt.createInfoNext = TMaybe.N,
		Vk.DscStLyt.createInfoFlags = zeroBits,
		Vk.DscStLyt.createInfoBindings = HPList.Singleton
			Vk.DscStLyt.BindingBuffer {
				Vk.DscStLyt.bindingBufferDescriptorType =
					Vk.Dsc.TypeStorageBuffer,
				Vk.DscStLyt.bindingBufferStageFlags =
					Vk.ShaderStageComputeBit } }

type Word32List = Obj.List 256 Word32 ""

withDvc :: (forall sd scb .
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CBffr.C scb-> IO a) -> IO a
withDvc a = Vk.Inst.create iinfo nil \ist -> do
	pd <- head' <$> Vk.Phd.enumerate ist
	qfi <- fst . head' . filter (
			checkBits Vk.Q.ComputeBit .
			Vk.QFm.propertiesQueueFlags . snd )
		<$> Vk.Phd.getQueueFamilyProperties pd
	Vk.Dvc.create pd (dinfo qfi) nil \dv ->
		Vk.Dvc.getQueue dv qfi 0 >>= \q ->
		Vk.CmdPl.create dv (cpinfo qfi) nil \cp ->
		Vk.CBffr.allocateCs dv (cbinfo cp) \(cb :*. HPList.Nil) ->
		a pd dv q cb
	where
	iinfo :: Vk.Inst.CreateInfo 'Nothing 'Nothing
	iinfo = def {
		Vk.Inst.createInfoEnabledLayerNames =
			[Vk.layerKhronosValidation] }
	dinfo qfi = Vk.Dvc.CreateInfo {
		Vk.Dvc.createInfoNext = TMaybe.N,
		Vk.Dvc.createInfoFlags = zeroBits,
		Vk.Dvc.createInfoQueueCreateInfos = HPList.Singleton $ qinfo qfi,
		Vk.Dvc.createInfoEnabledLayerNames =
			[Vk.layerKhronosValidation],
		Vk.Dvc.createInfoEnabledExtensionNames = [],
		Vk.Dvc.createInfoEnabledFeatures = Nothing }
	qinfo qfi = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = zeroBits,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qfi,
		Vk.Dvc.queueCreateInfoQueuePriorities = [0] }
	cpinfo qfi = Vk.CmdPl.CreateInfo {
		Vk.CmdPl.createInfoNext = TMaybe.N,
		Vk.CmdPl.createInfoFlags = Vk.CmdPl.CreateResetCommandBufferBit,
		Vk.CmdPl.createInfoQueueFamilyIndex = qfi }
	cbinfo :: Vk.CmdPl.C s -> Vk.CBffr.AllocateInfo 'Nothing s '[ '()]
	cbinfo cp = Vk.CBffr.AllocateInfo {
		Vk.CBffr.allocateInfoNext = TMaybe.N,
		Vk.CBffr.allocateInfoCommandPool = cp,
		Vk.CBffr.allocateInfoLevel = Vk.CBffr.LevelPrimary }

-- PREPARE MEMORIES

prepareMm :: forall sd sl bts a . (
	Default (HPList.PL2 KObj.Length
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts)),
	Vk.DscSt.BindingAndArrayElemBuffer bts '[Word32List] 0,
	Vk.DscSt.UpdateDynamicLength bts '[Word32List] ) =>
	Vk.Dvc.Size -> Vk.Dvc.Size ->
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.DscStLyt.D sl bts -> (forall sds sm sb .
		Vk.DscSt.D sds '(sl, bts) ->
		Vk.Mm.M sm '[ '( sb, 'Vk.Mm.BufferArg "" '[Word32List])] ->
		IO a) -> IO a
prepareMm w h pd dv dsl a =
	Vk.DscPl.create dv dpinfo nil \dp ->
	Vk.DscSt.allocateDs dv (dsinfo dp) \(HPList.Singleton ds) ->
	bffrNew w h pd dv \b m ->
	Vk.DscSt.updateDs dv
		(HPList.Singleton . U5 $ writeDscSt ds b) HPList.Nil >> a ds m
	where
	dpinfo = Vk.DscPl.CreateInfo {
		Vk.DscPl.createInfoNext = TMaybe.N,
		Vk.DscPl.createInfoFlags = Vk.DscPl.CreateFreeDescriptorSetBit,
		Vk.DscPl.createInfoMaxSets = 1,
		Vk.DscPl.createInfoPoolSizes = (: []) Vk.DscPl.Size {
			Vk.DscPl.sizeType = Vk.Dsc.TypeStorageBuffer,
			Vk.DscPl.sizeDescriptorCount = 1 } }
	dsinfo :: Vk.DscPl.P sp ->
		Vk.DscSt.AllocateInfo 'Nothing sp '[ '(sl, bts)]
	dsinfo pl = Vk.DscSt.AllocateInfo {
		Vk.DscSt.allocateInfoNext = TMaybe.N,
		Vk.DscSt.allocateInfoDescriptorPool = pl,
		Vk.DscSt.allocateInfoSetLayouts = HPList.Singleton $ U2 dsl }

writeDscSt :: forall sds slbts sm sb os .
	(Obj.OffsetRange Word32List os 0, Show (HPList.PL Obj.Length os)) =>
	Vk.DscSt.D sds slbts -> Vk.Bffr.Binded sm sb "" os ->
	Vk.DscSt.Write 'Nothing sds slbts
		('Vk.DscSt.WriteSourcesArgBuffer
			'[ '(sm, sb, "", Word32List, 0)]) 0
writeDscSt ds b = Vk.DscSt.Write {
	Vk.DscSt.writeNext = TMaybe.N, Vk.DscSt.writeDstSet = ds,
	Vk.DscSt.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSt.writeSources = Vk.DscSt.BufferInfos
		. HPList.Singleton . U5 $ Vk.Dsc.BufferInfo b }

bffrNew :: forall sd nm a . Vk.Dvc.Size -> Vk.Dvc.Size ->
	Vk.Phd.P -> Vk.Dvc.D sd -> (forall sb sm .
		Vk.Bffr.Binded sm sb nm '[Word32List]  ->
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[Word32List])] ->
		IO a) -> IO a
bffrNew w h pd dv a =
	Vk.Bffr.create dv binfo nil \b -> getMmInfo pd dv b >>= \mi ->
	Vk.Mm.allocateBind dv
		(HPList.Singleton . U2 $ Vk.Mm.Buffer b) mi nil
		\(HPList.Singleton (U2 (Vk.Mm.BufferBinded bnd))) m -> a bnd m
	where binfo = Vk.Bffr.CreateInfo {
		Vk.Bffr.createInfoNext = TMaybe.N,
		Vk.Bffr.createInfoFlags = zeroBits,
		Vk.Bffr.createInfoLengths =
			HPList.Singleton . Obj.LengthList $ w * h,
		Vk.Bffr.createInfoUsage = Vk.Bffr.UsageStorageBufferBit,
		Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Bffr.createInfoQueueFamilyIndices = [] }

getMmInfo :: Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Bffr.B sb nm os -> IO (Vk.Mm.AllocateInfo 'Nothing)
getMmInfo pd dv b = do
	rqs <- Vk.Bffr.getMemoryRequirements dv b
	mti <- findMmTpIdx pd rqs
		$ Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit
	pure Vk.Mm.AllocateInfo {
		Vk.Mm.allocateInfoNext = TMaybe.N,
		Vk.Mm.allocateInfoMemoryTypeIndex = mti }

findMmTpIdx :: Vk.Phd.P ->
	Vk.Mm.Requirements -> Vk.Mm.PropertyFlags -> IO Vk.Mm.TypeIndex
findMmTpIdx pd rqs wt = Vk.Phd.getMemoryProperties pd >>= \prs ->
	let	rqts = Vk.Mm.requirementsMemoryTypeBits rqs
		prpts = (fst <$>)
			. filter (checkBits wt . Vk.Mm.mTypePropertyFlags . snd)
			$ Vk.Phd.memoryPropertiesMemoryTypes prs in
	case filter (`Vk.Mm.elemTypeIndex` rqts) prpts of
		[] -> error "No available memory types"
		i : _ -> pure i

-- CALC

calc :: forall sd scb sds sl bts .
	(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[]]) =>
	Word32 -> Word32 -> Complex Float -> Complex Float ->
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CBffr.C scb ->
	Vk.DscStLyt.D sl bts -> Vk.DscSt.D sds '(sl, bts) -> IO ()
calc w h lt rb dv q cb dsl ds =
	Vk.PplLyt.create dv (pplLytInfo dsl) nil \pl ->
	Vk.Ppl.Cmpt.createCs dv Nothing (HPList.Singleton . U4 $ pplInfo pl)
		nil \(HPList.Singleton ppl) ->
	run w h lt rb dv q ds cb pl ppl

pplLytInfo :: Vk.DscStLyt.D sl bts -> Vk.PplLyt.CreateInfo 'Nothing '[ '(sl, bts)]
	('Vk.PushConstant.Layout
		'[PictureSize, Complex Float, Complex Float]
		'[ 'Vk.PushConstant.Range
			'[ 'Vk.T.ShaderStageComputeBit]
			'[PictureSize, Complex Float, Complex Float]])
pplLytInfo dsl = Vk.PplLyt.CreateInfo {
	Vk.PplLyt.createInfoNext = TMaybe.N,
	Vk.PplLyt.createInfoFlags = zeroBits,
	Vk.PplLyt.createInfoSetLayouts = HPList.Singleton $ U2 dsl }

run :: forall sd sds sl slbts scb sppl .
	(Vk.Cmd.LayoutArgListOnlyDynamics '[slbts] ~ '[ '[ '[]]]) =>
	Word32 -> Word32 -> Complex Float -> Complex Float ->
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.DscSt.D sds slbts -> Vk.CBffr.C scb ->
	Vk.PplLyt.P sl '[slbts] '[PictureSize, Complex Float, Complex Float] ->
	Vk.Ppl.Cmpt.C sppl
		'(sl, '[slbts], '[PictureSize, Complex Float, Complex Float]) ->
	IO ()
run w h lt rb dv q ds cb pl ppl = do
	Vk.CBffr.begin @'Nothing @'Nothing cb def $
		Vk.Cmd.bindPipelineCompute
			cb Vk.Ppl.BindPointCompute ppl \ccb ->
		Vk.Cmd.pushConstantsCompute @'[ 'Vk.T.ShaderStageComputeBit]
			ccb pl (GStr.W (w, h) :* lt :* rb :* HPList.Nil) >>
		Vk.Cmd.bindDescriptorSetsCompute
			ccb pl (HPList.Singleton $ U2 ds) def >>
		Vk.Cmd.dispatch ccb (w `div` 32) (h `div` 32) 1
	Vk.Fnc.create dv Vk.Fnc.CreateInfo {
		Vk.Fnc.createInfoNext = TMaybe.N,
		Vk.Fnc.createInfoFlags = zeroBits } nil \fnc ->
		Vk.Q.submit q (HPList.Singleton $ U4 sinfo) (Just fnc) >>
		Vk.Fnc.waitForFs dv (HPList.Singleton fnc) True Nothing
	where
	sinfo :: Vk.SubmitInfo 'Nothing '[] '[scb] '[]
	sinfo = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Nil }

-- COMPUTE PIPELINE INFO

pplInfo :: Vk.PplLyt.P sl sbtss '[PictureSize, Complex Float, Complex Float] ->
	Vk.Ppl.Cmpt.CreateInfo 'Nothing
		'( 'Nothing, 'Nothing, 'GlslComputeShader, 'Nothing, '[])
		'(sl, sbtss, '[PictureSize, Complex Float, Complex Float]) sbph
pplInfo pl = Vk.Ppl.Cmpt.CreateInfo {
	Vk.Ppl.Cmpt.createInfoNext = TMaybe.N,
	Vk.Ppl.Cmpt.createInfoFlags = zeroBits,
	Vk.Ppl.Cmpt.createInfoStage = U5 shdrStInfo,
	Vk.Ppl.Cmpt.createInfoLayout = U3 pl,
	Vk.Ppl.Cmpt.createInfoBasePipelineHandleOrIndex = Nothing }

shdrStInfo :: Vk.Ppl.ShdrSt.CreateInfo
	'Nothing 'Nothing 'GlslComputeShader 'Nothing '[]
shdrStInfo = Vk.Ppl.ShdrSt.CreateInfo {
	Vk.Ppl.ShdrSt.createInfoNext = TMaybe.N,
	Vk.Ppl.ShdrSt.createInfoFlags = zeroBits,
	Vk.Ppl.ShdrSt.createInfoStage = Vk.ShaderStageComputeBit,
	Vk.Ppl.ShdrSt.createInfoModule = (minfo, nil),
	Vk.Ppl.ShdrSt.createInfoName = "main",
	Vk.Ppl.ShdrSt.createInfoSpecializationInfo = HPList.Nil }
	where minfo = Vk.ShaderMod.CreateInfo {
		Vk.ShaderMod.createInfoNext = TMaybe.N,
		Vk.ShaderMod.createInfoFlags = zeroBits,
		Vk.ShaderMod.createInfoCode = glslComputeShaderMain }

[glslComputeShader|

#version 460

layout(local_size_x = 32, local_size_y = 32) in;

layout(binding = 0) buffer Image { uint image[]; };
layout(push_constant) uniform Range { uvec2 sz; vec2 lt; vec2 rb; } range;

#define cx_add(a, b) vec2(a.x+b.x, a.y+b.y)
#define cx_mul(a, b) vec2(a.x*b.x-a.y*b.y, a.x*b.y+a.y*b.x)
#define cx_magnitude(a) length(a)

int
escape_time(vec2 c, int lm)
{
	vec2 z = vec2(0, 0);

	for (int i = 0; i <= lm; i++) {
		z = cx_add(cx_mul(z, z), c);
		if (cx_magnitude(z) > 2) return i; }

	return -1;
}

vec2
pixel_to_point(uint pxw, uint pxh, vec2 lt, vec2 rb, uint x, uint y)
{
	float pntw = rb.x - lt.x; float pnth = lt.y - rb.y;
	return vec2(
		lt.x + float(x) * pntw / float(pxw),
		lt.y - float(y) * pnth / float(pxh) );
}

void
render(uint w, uint h, vec2 lt, vec2 rb)
{
	uint x = gl_GlobalInvocationID.x; uint y = gl_GlobalInvocationID.y;
	uint i = x + y * w;

	vec2 point = pixel_to_point(w, h, lt, rb, x, y);
	int t = escape_time(point, 255);
	if (t < 0) image[i] = 0; else image[i] = 255 - t;
}

void
main()
{
	render(range.sz.x, range.sz.y, range.lt, range.rb);
}

|]
