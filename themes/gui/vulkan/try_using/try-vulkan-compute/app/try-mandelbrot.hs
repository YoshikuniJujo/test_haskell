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

import qualified Gpu.Vulkan.Memory as Vk.Mem

import Gpu.Vulkan.Object qualified as Obj
import Gpu.Vulkan.Object.Base qualified as KObj
import Data.Default
import Data.Bits
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import qualified Data.HeteroParList as HL
import Data.HeteroParList (pattern(:*), pattern (:*.), pattern (:**))
import Data.Word

import Language.SpirV.Shaderc.TH
import Language.SpirV.ShaderKind

import qualified Gpu.Vulkan as Vk
import qualified Gpu.Vulkan.Instance as Vk.Inst
import qualified Gpu.Vulkan.PhysicalDevice as Vk.Phd
import qualified Gpu.Vulkan.Queue as Vk.Queue
import qualified Gpu.Vulkan.QueueFamily as Vk.QFm
import qualified Gpu.Vulkan.Device as Vk.Dv
import qualified Gpu.Vulkan.CommandPool as Vk.CmdPool
import qualified Gpu.Vulkan.Memory as Vk.Mm
import qualified Gpu.Vulkan.Memory as Vk.Mm.M
import qualified Gpu.Vulkan.Descriptor as Vk.Dsc
import qualified Gpu.Vulkan.DescriptorPool as Vk.DscPool
import qualified Gpu.Vulkan.ShaderModule as Vk.ShaderMod
import qualified "try-gpu-vulkan" Gpu.Vulkan.Pipeline as Vk.Ppl
import qualified Gpu.Vulkan.PipelineLayout as Vk.Ppl.Lyt
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShaderSt
import qualified Gpu.Vulkan.Pipeline.Compute as Vk.Ppl.Cmpt
import qualified Gpu.Vulkan.DescriptorSet as Vk.DS
import qualified Gpu.Vulkan.CommandBuffer as Vk.CBffr
import qualified Gpu.Vulkan.Cmd as Vk.Cmd

import qualified Gpu.Vulkan.Buffer as Vk.Bffr
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DSLyt

import qualified Gpu.Vulkan.PushConstant as Vk.PushConstant

import Codec.Picture
import Data.Vector.Storable qualified as V

import Gpu.Vulkan.TypeEnum qualified as Vk.T

import Foreign.Storable.Generic qualified as GStr
import System.Environment

import Data.List qualified as L
import Text.Read

import Gpu.Vulkan.Fence qualified as Vk.Fence
import Data.Complex

---------------------------------------------------------------------------

-- MAIN
-- PREPARE MEMORIES
-- CALC
-- COMPUTE PIPELINE INFO

---------------------------------------------------------------------------

type PictureSize = GStr.W (Word32, Word32)

-- MAIN

bffSize :: Integral n => n -> n -> n
bffSize w h = w * h

writeResult :: Word32 -> Word32 -> [Word32] -> IO ()
writeResult w h = writePng @Pixel8 "autogen/mandelbrot.png"
	. Image (fromIntegral w) (fromIntegral h)
	. V.fromList . map fromIntegral

main :: IO ()
main = do
	as <- getArgs
	case readArgs as of
		Just ((w, h), ul, lr) -> withDevice \pd qfi dv -> writeResult w h =<<
			Vk.DSLyt.create dv dscSetLayoutInfo nil \dslyt ->
			prepareMems (fromIntegral w) (fromIntegral h) pd dv dslyt \dscs m ->
			calc w h ul lr qfi dv dslyt dscs (bffSize w h) >>
			Vk.Mm.read @"" @Word32List @0 @[Word32] dv m zeroBits
		Nothing -> error "bad command line arguments"

readArgs :: [String] -> Maybe ((Word32, Word32), Complex Float, Complex Float)
readArgs = \case
	[sz, ul, lr] -> (,,)
		<$> readPair sz 'x'
		<*> (uncurry (:+) <$> readPair ul ',')
		<*> (uncurry (:+) <$> readPair lr ',')
	_ -> Nothing

readPair :: Read a => String -> Char -> Maybe (a, a)
readPair s c = case L.findIndex (== c) s of
	Nothing -> Nothing
	Just i -> (,) <$> readMaybe (take i s) <*> readMaybe (tail $ drop i s)

type Word32List = Obj.List 256 Word32 ""

withDevice :: (forall s . Vk.Phd.P -> Vk.QFm.Index -> Vk.Dv.D s -> IO a) -> IO a
withDevice f = Vk.Inst.create instInfo nil \inst -> do
	pd <- head <$> Vk.Phd.enumerate inst
	qfi <- fst . head . filter (
			checkBits Vk.Queue.ComputeBit .
			Vk.QFm.propertiesQueueFlags . snd )
		<$> Vk.Phd.getQueueFamilyProperties pd
	Vk.Dv.create pd (dvcInfo qfi) nil $ f pd qfi

instInfo :: Vk.Inst.CreateInfo 'Nothing 'Nothing
instInfo = def {
	Vk.Inst.createInfoEnabledLayerNames = [Vk.layerKhronosValidation] }
	
dvcInfo :: Vk.QFm.Index -> Vk.Dv.CreateInfo 'Nothing '[ 'Nothing]
dvcInfo qfi = Vk.Dv.CreateInfo {
	Vk.Dv.createInfoNext = TMaybe.N, Vk.Dv.createInfoFlags = zeroBits,
	Vk.Dv.createInfoQueueCreateInfos = HL.Singleton qinfo,
	Vk.Dv.createInfoEnabledLayerNames = [Vk.layerKhronosValidation],
	Vk.Dv.createInfoEnabledExtensionNames = [],
	Vk.Dv.createInfoEnabledFeatures = Nothing }
	where qinfo = Vk.Dv.QueueCreateInfo {
		Vk.Dv.queueCreateInfoNext = TMaybe.N,
		Vk.Dv.queueCreateInfoFlags = zeroBits,
		Vk.Dv.queueCreateInfoQueueFamilyIndex = qfi,
		Vk.Dv.queueCreateInfoQueuePriorities = [0] }

dscSetLayoutInfo :: Vk.DSLyt.CreateInfo 'Nothing '[ 'Vk.DSLyt.Buffer '[Word32List]]
dscSetLayoutInfo = Vk.DSLyt.CreateInfo {
	Vk.DSLyt.createInfoNext = TMaybe.N, Vk.DSLyt.createInfoFlags = zeroBits,
	Vk.DSLyt.createInfoBindings = HL.Singleton bdg }
	where bdg = Vk.DSLyt.BindingBuffer {
		Vk.DSLyt.bindingBufferDescriptorType = Vk.Dsc.TypeStorageBuffer,
		Vk.DSLyt.bindingBufferStageFlags = Vk.ShaderStageComputeBit }

checkBits :: Bits bs => bs -> bs -> Bool
checkBits bs0 = (== bs0) . (.&. bs0)

-- PREPARE MEMORIES

prepareMems :: (
	Default (HL.PL (HL.PL KObj.Length)
		(Vk.DSLyt.BindingTypeListBufferOnlyDynamics bts)),
	Vk.DS.BindingAndArrayElemBuffer bts '[Word32List] 0,
	Vk.DS.UpdateDynamicLength bts '[Word32List] ) =>
	Vk.Dv.Size -> Vk.Dv.Size ->
	Vk.Phd.P -> Vk.Dv.D sd -> Vk.DSLyt.D sl bts ->
	(forall sds sm sb .
		Vk.DS.D sds '(sl, bts) ->
		Vk.Mm.M sm '[ '( sb, 'Vk.Mm.BufferArg "" '[Word32List])] ->
		IO a) -> IO a
prepareMems w h pd dv dslyt f =
	Vk.DscPool.create dv dscPoolInfo nil \dp ->
	Vk.DS.allocateDs dv (dscSetInfo dp dslyt) \(HL.Singleton ds) ->
	storageBufferNew w h pd dv \b m ->
	Vk.DS.updateDs dv (HL.Singleton . U5 $ writeDscSet ds b) HL.Nil >>
	f ds m

dscPoolInfo :: Vk.DscPool.CreateInfo 'Nothing
dscPoolInfo = Vk.DscPool.CreateInfo {
	Vk.DscPool.createInfoNext = TMaybe.N,
	Vk.DscPool.createInfoFlags = Vk.DscPool.CreateFreeDescriptorSetBit,
	Vk.DscPool.createInfoMaxSets = 1,
	Vk.DscPool.createInfoPoolSizes = [sz] }
	where sz = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeStorageBuffer,
		Vk.DscPool.sizeDescriptorCount = 10 }

dscSetInfo :: Vk.DscPool.P sp -> Vk.DSLyt.D sl bts ->
	Vk.DS.AllocateInfo 'Nothing sp '[ '(sl, bts)]
dscSetInfo pl lyt = Vk.DS.AllocateInfo {
	Vk.DS.allocateInfoNext = TMaybe.N,
	Vk.DS.allocateInfoDescriptorPool = pl,
	Vk.DS.allocateInfoSetLayouts = HL.Singleton $ U2 lyt }

storageBufferNew :: forall sd nm a . Vk.Dv.Size -> Vk.Dv.Size -> Vk.Phd.P -> Vk.Dv.D sd -> (forall sb sm .
	Vk.Bffr.Binded sm sb nm '[Word32List]  ->
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[Word32List])] -> IO a) -> IO a
storageBufferNew w h pd dv f =
	Vk.Bffr.create dv (bufferInfo w h) nil \bf ->
	getMemoryInfo pd dv bf >>= \mmi ->
	Vk.Mm.allocateBind dv
		(HL.Singleton . U2 $ Vk.Mm.Buffer bf) mmi nil
		\(HL.Singleton (U2 (Vk.Mm.BufferBinded bnd))) mm ->
	f bnd mm

bufferInfo :: Vk.Dv.Size -> Vk.Dv.Size -> Vk.Bffr.CreateInfo 'Nothing '[Word32List]
bufferInfo w h = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N,
	Vk.Bffr.createInfoFlags = zeroBits,
	Vk.Bffr.createInfoLengths = HL.Singleton . Obj.LengthList $ bffSize w h,
	Vk.Bffr.createInfoUsage = Vk.Bffr.UsageStorageBufferBit,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

getMemoryInfo :: Vk.Phd.P -> Vk.Dv.D sd -> Vk.Bffr.B sb nm objs ->
	IO (Vk.Mem.AllocateInfo 'Nothing)
getMemoryInfo pd dv bff = do
	rqs <- Vk.Bffr.getMemoryRequirements dv bff
	mti <- findMemoryTypeIndex pd rqs
		$ Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit
	pure Vk.Mem.AllocateInfo {
		Vk.Mem.allocateInfoNext = TMaybe.N,
		Vk.Mem.allocateInfoMemoryTypeIndex = mti }

findMemoryTypeIndex :: Vk.Phd.P ->
	Vk.Mm.M.Requirements -> Vk.Mm.PropertyFlags -> IO Vk.Mm.M.TypeIndex
findMemoryTypeIndex pd rqs prp0 = Vk.Phd.getMemoryProperties pd >>= \prps ->
	let	rqts = Vk.Mm.M.requirementsMemoryTypeBits rqs
		prpts = (fst <$>)
			. filter (checkBits prp0
				. Vk.Mm.mTypePropertyFlags . snd)
			$ Vk.Phd.memoryPropertiesMemoryTypes prps in
	case filter (`Vk.Mm.M.elemTypeIndex` rqts) prpts of
		[] -> error "No available memory types"
		i : _ -> pure i

writeDscSet :: forall s slbts sb sm os . (
	Show (HL.PL Obj.Length os),
	Obj.OffsetRange' (Obj.List 256 Word32 "") os 0
	) =>
	Vk.DS.D s slbts -> Vk.Bffr.Binded sm sb "" os ->
	Vk.DS.Write 'Nothing s slbts
		('Vk.DS.WriteSourcesArgBuffer '[ '(sm, sb, "", Word32List, 0)]) 0
writeDscSet ds ba = Vk.DS.Write {
	Vk.DS.writeNext = TMaybe.N,
	Vk.DS.writeDstSet = ds,
	Vk.DS.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DS.writeSources =
		Vk.DS.BufferInfos . HL.Singleton . U5 $ Vk.Dsc.BufferInfo ba }

-- CALC

calc :: forall slbts sl bts sd s . (
	slbts ~ '(sl, bts),
	Vk.DSLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[]] ) =>
	Word32 -> Word32 -> Complex Float -> Complex Float ->
	Vk.QFm.Index -> Vk.Dv.D sd -> Vk.DSLyt.D sl bts ->
	Vk.DS.D s slbts -> Word32 -> IO ()
calc w h ul lr qfi dv dslyt ds sz =
	Vk.Ppl.Lyt.create dv (pplLayoutInfo dslyt) nil \plyt ->
	Vk.Ppl.Cmpt.createCs dv Nothing
		(HL.Singleton . U4 $ pplInfo plyt) nil \(pl :** HL.Nil) ->
	Vk.CmdPool.create dv (commandPoolInfo qfi) nil \cp ->
	Vk.CBffr.allocate dv (commandBufferInfo cp) \(cb :*. HL.Nil) ->
	run w h ul lr qfi dv ds cb plyt pl sz

pplLayoutInfo :: Vk.DSLyt.D sl bts ->
	Vk.Ppl.Lyt.CreateInfo 'Nothing '[ '(sl, bts)]
		('Vk.PushConstant.Layout
			'[PictureSize, Complex Float, Complex Float]
			'[	'Vk.PushConstant.Range
					'[ 'Vk.T.ShaderStageComputeBit] '[PictureSize, Complex Float, Complex Float]
				])
pplLayoutInfo dsl = Vk.Ppl.Lyt.CreateInfo {
	Vk.Ppl.Lyt.createInfoNext = TMaybe.N,
	Vk.Ppl.Lyt.createInfoFlags = zeroBits,
	Vk.Ppl.Lyt.createInfoSetLayouts = HL.Singleton $ U2 dsl }

commandPoolInfo :: Vk.QFm.Index -> Vk.CmdPool.CreateInfo 'Nothing
commandPoolInfo qfi = Vk.CmdPool.CreateInfo {
	Vk.CmdPool.createInfoNext = TMaybe.N,
	Vk.CmdPool.createInfoFlags = Vk.CmdPool.CreateResetCommandBufferBit,
	Vk.CmdPool.createInfoQueueFamilyIndex = qfi }

commandBufferInfo :: Vk.CmdPool.C s -> Vk.CBffr.AllocateInfo 'Nothing s '[ '()]
commandBufferInfo cmdPool = Vk.CBffr.AllocateInfo {
	Vk.CBffr.allocateInfoNext = TMaybe.N,
	Vk.CBffr.allocateInfoCommandPool = cmdPool,
	Vk.CBffr.allocateInfoLevel = Vk.CBffr.LevelPrimary }

run :: forall slbts sd sc sg sl s . (
	Vk.Cmd.LayoutArgListOnlyDynamics '[slbts] ~ '[ '[ '[]]] ) =>
	Word32 -> Word32 -> Complex Float -> Complex Float ->
	Vk.QFm.Index -> Vk.Dv.D sd -> Vk.DS.D s slbts -> Vk.CBffr.C sc ->
	Vk.Ppl.Lyt.P sl '[slbts] '[PictureSize, Complex Float, Complex Float] ->
	Vk.Ppl.Cmpt.C sg '(sl, '[slbts], '[PictureSize, Complex Float, Complex Float]) -> Word32 -> IO ()
run w h ul lr qfi dv ds cb lyt pl sz = Vk.Dv.getQueue dv qfi 0 >>= \q -> do
	Vk.CBffr.begin @'Nothing @'Nothing cb def $
		Vk.Cmd.bindPipelineCompute cb Vk.Ppl.BindPointCompute pl \ccb ->
		Vk.Cmd.pushConstantsCompute @'[ 'Vk.T.ShaderStageComputeBit]
			ccb lyt (GStr.W (w, h) :* ul :* lr :* HL.Nil) >>
		Vk.Cmd.bindDescriptorSetsCompute
			ccb lyt (HL.Singleton $ U2 ds) def >>
		Vk.Cmd.dispatch ccb (w `div` 8) (h `div` 8) 1
	Vk.Fence.create dv Vk.Fence.CreateInfo {
		Vk.Fence.createInfoNext = TMaybe.N,
		Vk.Fence.createInfoFlags = zeroBits } nil  \fnc ->
		Vk.Queue.submit q (HL.Singleton $ U4 sinfo) (Just fnc) >>
		 Vk.Queue.waitIdle q >>
		Vk.Fence.waitForFs dv (HL.Singleton fnc) True Nothing
	where
	sinfo :: Vk.SubmitInfo 'Nothing '[] '[sc] '[]
	sinfo = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HL.Nil,
		Vk.submitInfoCommandBuffers = HL.Singleton cb,
		Vk.submitInfoSignalSemaphores = HL.Nil }

-- COMPUTE PIPELINE INFO

pplInfo :: Vk.Ppl.Lyt.P sl sbtss '[PictureSize, Complex Float, Complex Float] ->
	Vk.Ppl.Cmpt.CreateInfo 'Nothing
		'( 'Nothing, 'Nothing, 'GlslComputeShader, 'Nothing, '[])
		'(sl, sbtss, '[PictureSize, Complex Float, Complex Float]) sbph
pplInfo pl = Vk.Ppl.Cmpt.CreateInfo {
	Vk.Ppl.Cmpt.createInfoNext = TMaybe.N,
	Vk.Ppl.Cmpt.createInfoFlags = zeroBits,
	Vk.Ppl.Cmpt.createInfoStage = U5 shaderStInfo,
	Vk.Ppl.Cmpt.createInfoLayout = U3 pl,
	Vk.Ppl.Cmpt.createInfoBasePipelineHandleOrIndex = Nothing }

shaderStInfo :: Vk.Ppl.ShaderSt.CreateInfo 'Nothing 'Nothing 'GlslComputeShader 'Nothing '[]
shaderStInfo = Vk.Ppl.ShaderSt.CreateInfo {
	Vk.Ppl.ShaderSt.createInfoNext = TMaybe.N,
	Vk.Ppl.ShaderSt.createInfoFlags = zeroBits,
	Vk.Ppl.ShaderSt.createInfoStage = Vk.ShaderStageComputeBit,
	Vk.Ppl.ShaderSt.createInfoModule = (shdrMdInfo, nil),
	Vk.Ppl.ShaderSt.createInfoName = "main",
	Vk.Ppl.ShaderSt.createInfoSpecializationInfo = Nothing }
	where shdrMdInfo = Vk.ShaderMod.CreateInfo {
		Vk.ShaderMod.createInfoNext = TMaybe.N,
		Vk.ShaderMod.createInfoFlags = zeroBits,
		Vk.ShaderMod.createInfoCode = glslComputeShaderMain }

[glslComputeShader|

#version 460

layout(push_constant) uniform Foo {
	uvec2 sz; vec2 ul; vec2 lr;
	} foo;

layout(binding = 0) buffer Data { uint val[]; };

layout(local_size_x = 8, local_size_y = 8) in;

#define cx_add(a, b) vec2(a.x+b.x, a.y+b.y)
#define cx_mul(a, b) vec2(a.x*b.x-a.y*b.y, a.x*b.y+a.y*b.x)
#define cx_magnitude(a) length(a)

int
escape_time(vec2 c, int lm)
{
	vec2 z = vec2(0, 0);

	for (int i = 0; i <= lm; i++) {
		z = cx_add(cx_mul(z, z), c);
		if (cx_magnitude(z) > 2) return i;
	}

	return -1;
}

vec2
pixel_to_point(
	uint w, uint h, uint x, uint y, vec2 upper_left, vec2 lower_right )
{
	float width = lower_right.x - upper_left.x;
	float height = upper_left.y - lower_right.y;

	return vec2(
		upper_left.x + float(x) * width / float(w),
		upper_left.y - float(y) * height / float(h) );
}

void
render(uint w, uint h, vec2 upper_left, vec2 lower_right)
{
	uint row = gl_GlobalInvocationID.y;
	uint column = gl_GlobalInvocationID.x;
	vec2 point = pixel_to_point(w, h, column, row, upper_left, lower_right);
	int t = escape_time(point, 255);
	if (t < 0) val[row * w + column] = 0;
	else val[row * w + column] = 255 - t;
}

void
main()
{
	render(foo.sz.x, foo.sz.y, foo.ul, foo.lr);
}

|]
