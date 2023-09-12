{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Gpu.Vulkan.Object.Base qualified as KObj
import Gpu.Vulkan.Object qualified as VObj
import Control.Arrow
import Data.Default
import Data.Bits
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.Index qualified as TIndex
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.List
import Data.HeteroParList qualified as HL
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Word

import qualified Data.Vector.Storable as V

import Language.SpirV.Shaderc.TH
import Language.SpirV.ShaderKind
import Gpu.Vulkan.Misc

import qualified Gpu.Vulkan as Vk
import qualified Gpu.Vulkan.Enum as Vk
import qualified Gpu.Vulkan.Instance as Vk.Ist
import qualified Gpu.Vulkan.PhysicalDevice as Vk.Phd
import qualified Gpu.Vulkan.Queue as Vk.Queue
import qualified Gpu.Vulkan.Queue.Enum as Vk.Queue
import qualified Gpu.Vulkan.QueueFamily as Vk.QF
import qualified Gpu.Vulkan.Device as Vk.Dv
import qualified Gpu.Vulkan.CommandPool as Vk.CommandPool
import qualified Gpu.Vulkan.CommandPool.Enum as Vk.CommandPool
import qualified Gpu.Vulkan.Buffer.Enum as Vk.Bff
import qualified Gpu.Vulkan.Memory as Vk.Mm
import qualified Gpu.Vulkan.Memory.Enum as Vk.Mm
import qualified Gpu.Vulkan.Descriptor.Enum as Vk.Dsc
import qualified Gpu.Vulkan.DescriptorPool as Vk.DP
import qualified Gpu.Vulkan.DescriptorPool.Enum as Vk.DP
import qualified Gpu.Vulkan.ShaderModule as Vk.ShaderMod
import qualified Gpu.Vulkan.Pipeline.Enum as Vk.Ppl
import qualified Gpu.Vulkan.PipelineLayout as Vk.Ppl.Lyt
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShaderSt
import qualified Gpu.Vulkan.Pipeline.Compute as Vk.Ppl.Cmpt
import qualified Gpu.Vulkan.DescriptorSet as Vk.DS
import qualified Gpu.Vulkan.CommandBuffer as Vk.CmdBuf
import qualified Gpu.Vulkan.CommandBuffer.Enum as Vk.CmdBuf
import qualified Gpu.Vulkan.Cmd as Vk.Cmd

import qualified Gpu.Vulkan.Buffer as Vk.Bff
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DSLyt

import qualified Gpu.Vulkan.BufferView as Vk.BffVw
import qualified Gpu.Vulkan.PushConstant as Vk.PC

import Gpu.Vulkan.TypeEnum qualified as Vk.T

import Codec.Picture qualified as P
import System.Environment
import System.FilePath

main :: IO ()
main = do
	inf : _ <- getArgs
	let outf = uncurry (++) . first (++ "_nega") $ splitExtension inf
	writePixels outf =<< makeNega =<< readPixels inf

makeNega :: Pixels -> IO Pixels
makeNega (sz@(fromIntegral -> w, fromIntegral -> h), v) =
	device \phd qf dv -> dscSetLayout dv \dslyt ->
	buffer phd dv dslyt v \ds (m :: Memory sm sb nm) ->
	pipeline dv qf dslyt \cb ppl plyt ->
	(sz ,) <$> run @nm dv qf cb ppl plyt ds m w h

type Memory sm sb nm = Vk.Mm.M sm '[ '( sb, 'Vk.Mm.BufferArg nm '[PixelList])]
type PixelList = VObj.List 256 Pixel ""

device :: (forall sd . Vk.Phd.P -> Vk.QF.Index -> Vk.Dv.D sd -> IO a) -> IO a
device f = Vk.Ist.create @_ @'Nothing instInfo nil' \ist -> do
	phd <- head <$> Vk.Phd.enumerate ist
	qf <- findQueueFamily phd Vk.Queue.ComputeBit
	Vk.Dv.create phd (dvcInfo qf) nil' \dv -> f phd qf dv
	where
	instInfo :: Vk.Ist.CreateInfo 'Nothing 'Nothing
	instInfo = def {
		Vk.Ist.createInfoEnabledLayerNames =
			[Vk.layerKhronosValidation] }
	dvcInfo qf = Vk.Dv.CreateInfo {
		Vk.Dv.createInfoNext = TMaybe.N,
		Vk.Dv.createInfoFlags = zeroBits,
		Vk.Dv.createInfoQueueCreateInfos = HL.Singleton $ queueInfo qf,
		Vk.Dv.createInfoEnabledLayerNames = [Vk.layerKhronosValidation],
		Vk.Dv.createInfoEnabledExtensionNames = [],
		Vk.Dv.createInfoEnabledFeatures = Nothing }
	queueInfo qf = Vk.Dv.QueueCreateInfo {
		Vk.Dv.queueCreateInfoNext = TMaybe.N,
		Vk.Dv.queueCreateInfoFlags = zeroBits,
		Vk.Dv.queueCreateInfoQueueFamilyIndex = qf,
		Vk.Dv.queueCreateInfoQueuePriorities = [0] }

findQueueFamily :: Vk.Phd.P -> Vk.Queue.FlagBits -> IO Vk.QF.Index
findQueueFamily phd qf = fst . head
	. filter ((/= zeroBits) . (.&. qf) . Vk.QF.propertiesQueueFlags . snd)
	<$> Vk.Phd.getQueueFamilyProperties phd

dscSetLayout :: Vk.Dv.D sd -> (forall s .
	Vk.DSLyt.D s '[ 'Vk.DSLyt.BufferView '[ '("", Pixel)]] -> IO a) -> IO a
dscSetLayout dv = Vk.DSLyt.create dv dscSetLayoutInfo TPMaybe.N
	where
	dscSetLayoutInfo = Vk.DSLyt.CreateInfo {
		Vk.DSLyt.createInfoNext = TMaybe.N,
		Vk.DSLyt.createInfoFlags = zeroBits,
		Vk.DSLyt.createInfoBindings = bdng :** HL.Nil }
	bdng = Vk.DSLyt.BindingBufferView {
		Vk.DSLyt.bindingBufferViewDescriptorType =
			Vk.Dsc.TypeStorageTexelBuffer,
		Vk.DSLyt.bindingBufferViewStageFlags =
			Vk.ShaderStageComputeBit }

buffer :: forall bts sd sl nm a . (
	Default (HL.PL (HL.PL KObj.Length)
		(Vk.DSLyt.BindingTypeListBufferOnlyDynamics bts)),
	Vk.DS.BindingAndArrayElemBufferView bts '[ '("", Pixel)] 0 ) =>
	Vk.Phd.P -> Vk.Dv.D sd -> Vk.DSLyt.D sl bts -> V.Vector Pixel -> (
		forall sds sm sb .
		Vk.DS.D sds '(sl, bts) -> Memory sm sb nm -> IO a ) -> IO a
buffer phd dv lyt v f =
	Vk.DP.create dv poolInfo nil' \pl ->
	Vk.DS.allocateDs dv (setInfo pl)
		\((ds :: Vk.DS.D sds '(sl, bts)) :** HL.Nil) ->
	bufferNew dv phd v \(bd :: Vk.Bff.Binded sm sb nm '[PixelList]) m ->
	Vk.BffVw.create dv (bvInfo bd) nil' \(bv :: Vk.BffVw.B sbv "" Pixel) ->
	Vk.DS.updateDs dv (U5 (write ds bv) :** HL.Nil) HL.Nil >> f ds m
	where
	poolInfo = Vk.DP.CreateInfo {
		Vk.DP.createInfoNext = TMaybe.N,
		Vk.DP.createInfoFlags = Vk.DP.CreateFreeDescriptorSetBit,
		Vk.DP.createInfoMaxSets = 1,
		Vk.DP.createInfoPoolSizes = [poolSize] }
	poolSize = Vk.DP.Size {
		Vk.DP.sizeType = Vk.Dsc.TypeStorageTexelBuffer,
		Vk.DP.sizeDescriptorCount = 1 }
	setInfo :: Vk.DP.P sp -> Vk.DS.AllocateInfo 'Nothing sp '[ '(sl, bts)]
	setInfo pl = Vk.DS.AllocateInfo {
		Vk.DS.allocateInfoNext = TMaybe.N,
		Vk.DS.allocateInfoDescriptorPool = pl,
		Vk.DS.allocateInfoSetLayouts = U2 lyt :** HL.Nil }
	bvInfo bd = Vk.BffVw.CreateInfo {
		Vk.BffVw.createInfoNext = TMaybe.N,
		Vk.BffVw.createInfoFlags = zeroBits,
		Vk.BffVw.createInfoBuffer = U4 bd }
	write :: Vk.DS.D sds '(sl, bts) -> Vk.BffVw.B sb "" Pixel ->
		Vk.DS.Write 'Nothing sds '(sl, bts)
			(Vk.DS.WriteSourcesArgBufferView '[ '(sb, "", Pixel)]) 0
	write ds bv = Vk.DS.Write {
		Vk.DS.writeNext = TMaybe.N,
		Vk.DS.writeDstSet = ds,
		Vk.DS.writeDescriptorType = Vk.Dsc.TypeStorageTexelBuffer,
		Vk.DS.writeSources =
			Vk.DS.TexelBufferViews . HL.Singleton $ U3 bv }

bufferNew :: forall sd nm a .
	Vk.Dv.D sd -> Vk.Phd.P -> V.Vector Pixel -> (forall sb sm .
		Vk.Bff.Binded sm sb nm '[PixelList] -> Memory sm sb nm ->
		IO a) -> IO a
bufferNew dv phd v f =
	Vk.Bff.create dv bufferInfo nil' \bffr ->
	memoryInfo bffr >>= \mi ->
	Vk.Mm.allocateBind dv (U2 (Vk.Mm.Buffer bffr) :** HL.Nil) mi nil'
		\(U2 (Vk.Mm.BufferBinded bnd) :** HL.Nil) m ->
	Vk.Mm.write @nm @PixelList dv m def v >> f bnd m
	where
	bufferInfo = Vk.Bff.CreateInfo {
		Vk.Bff.createInfoNext = TMaybe.N,
		Vk.Bff.createInfoFlags = def,
		Vk.Bff.createInfoLengths =
			VObj.LengthList (fromIntegral $ V.length v) :** HL.Nil,
		Vk.Bff.createInfoUsage =
			Vk.Bff.UsageStorageBufferBit .|.
			Vk.Bff.UsageStorageTexelBufferBit,
		Vk.Bff.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Bff.createInfoQueueFamilyIndices = [] }
	memoryInfo :: Vk.Bff.B sb nm objs -> IO (Vk.Mm.AllocateInfo 'Nothing)
	memoryInfo b = do
		rq <- Vk.Bff.getMemoryRequirements dv b
		mt <- findMemoryTypeIndex phd rq (
			Vk.Mm.PropertyHostVisibleBit .|.
			Vk.Mm.PropertyHostCoherentBit )
		pure Vk.Mm.AllocateInfo {
			Vk.Mm.allocateInfoNext = TMaybe.N,
			Vk.Mm.allocateInfoMemoryTypeIndex = mt }

findMemoryTypeIndex ::
	Vk.Phd.P -> Vk.Mm.Requirements -> Vk.Mm.PropertyFlags ->
	IO Vk.Mm.TypeIndex
findMemoryTypeIndex phd rq prp0 = Vk.Phd.getMemoryProperties phd >>= \prps -> do
	let	rqts = Vk.Mm.requirementsMemoryTypeBits rq
		mtps = (fst <$>)
			. filter (check prp0 . Vk.Mm.mTypePropertyFlags . snd)
			$ Vk.Phd.memoryPropertiesMemoryTypes prps
	case filter (`Vk.Mm.elemTypeIndex` rqts) mtps of
		[] -> error "No available memory types"; i : _ -> pure i
	where check = (.) <$> (==) <*> (.&.)

pipeline :: forall sd sl bts a .
	Vk.Dv.D sd -> Vk.QF.Index -> Vk.DSLyt.D sl bts -> (forall scb s1 s .
		Vk.CmdBuf.C scb ->
		Vk.Ppl.Cmpt.C s1 '(s, '[ '(sl, bts)], '[Word32]) ->
		Vk.Ppl.Lyt.P s '[ '(sl, bts)] '[Word32] -> IO a) -> IO a
pipeline dv qf dslyt f =
	Vk.Ppl.Lyt.create dv plytInfo nil' \plyt ->
	Vk.Ppl.Cmpt.createCs dv Nothing (U4 (pplInfo plyt) :** HL.Nil) nil'
		\(ppl :** HL.Nil) ->
	Vk.CommandPool.create dv cpoolInfo nil' \cp ->
	Vk.CmdBuf.allocate dv (cbInfo cp) \(cb :*. HL.Nil) -> f cb ppl plyt
	where
	plytInfo :: Vk.Ppl.Lyt.CreateInfo 'Nothing '[ '(sl, bts)]
		('Vk.PC.Layout '[Word32] '[ 'Vk.PC.Range
			'[ 'Vk.T.ShaderStageComputeBit] '[Word32]])
	plytInfo = Vk.Ppl.Lyt.CreateInfo {
		Vk.Ppl.Lyt.createInfoNext = TMaybe.N,
		Vk.Ppl.Lyt.createInfoFlags = zeroBits,
		Vk.Ppl.Lyt.createInfoSetLayouts = U2 dslyt :** HL.Nil }
	pplInfo pl = Vk.Ppl.Cmpt.CreateInfo {
		Vk.Ppl.Cmpt.createInfoNext = TMaybe.N,
		Vk.Ppl.Cmpt.createInfoFlags = def,
		Vk.Ppl.Cmpt.createInfoStage = U5 shaderInfo,
		Vk.Ppl.Cmpt.createInfoLayout = U3 pl,
		Vk.Ppl.Cmpt.createInfoBasePipelineHandleOrIndex = Nothing }
	shaderInfo :: Vk.Ppl.ShaderSt.CreateInfo
		'Nothing 'Nothing 'GlslComputeShader 'Nothing '[Word32, Word32]
	shaderInfo = Vk.Ppl.ShaderSt.CreateInfo {
		Vk.Ppl.ShaderSt.createInfoNext = TMaybe.N,
		Vk.Ppl.ShaderSt.createInfoFlags = zeroBits,
		Vk.Ppl.ShaderSt.createInfoStage = Vk.ShaderStageComputeBit,
		Vk.Ppl.ShaderSt.createInfoModule = (shaderModInfo, nil'),
		Vk.Ppl.ShaderSt.createInfoName = "main",
		Vk.Ppl.ShaderSt.createInfoSpecializationInfo =
			Just $ HL.Id 3 :** HL.Id 10 :** HL.Nil }
	shaderModInfo = Vk.ShaderMod.CreateInfo {
		Vk.ShaderMod.createInfoNext = TMaybe.N,
		Vk.ShaderMod.createInfoFlags = zeroBits,
		Vk.ShaderMod.createInfoCode = glslComputeShaderMain }
	cpoolInfo = Vk.CommandPool.CreateInfo {
		Vk.CommandPool.createInfoNext = TMaybe.N,
		Vk.CommandPool.createInfoFlags =
			Vk.CommandPool.CreateResetCommandBufferBit,
		Vk.CommandPool.createInfoQueueFamilyIndex = qf }
	cbInfo :: Vk.CommandPool.C s -> Vk.CmdBuf.AllocateInfo 'Nothing s '[ '()]
	cbInfo cp = Vk.CmdBuf.AllocateInfo {
		Vk.CmdBuf.allocateInfoNext = TMaybe.N,
		Vk.CmdBuf.allocateInfoCommandPool = cp,
		Vk.CmdBuf.allocateInfoLevel = Vk.CmdBuf.LevelPrimary }

run :: forall nm4 w4 objss4 slbts sbtss sd sc sg sl sm4 sds . (
	Vk.DSLyt.BindingTypeListBufferOnlyDynamics (TIndex.I1_2 slbts) ~ '[ '[]],
	sbtss ~ '[slbts],
	Storable w4,
	Vk.Mm.OffsetSize nm4 (VObj.List 256 w4 "") objss4,
	InfixIndex '[slbts] sbtss ) =>
	Vk.Dv.D sd -> Vk.QF.Index -> Vk.CmdBuf.C sc ->
	Vk.Ppl.Cmpt.C sg '(sl, sbtss, '[Word32]) ->
	Vk.Ppl.Lyt.P sl sbtss '[Word32] -> Vk.DS.D sds slbts ->
	Vk.Mm.M sm4 objss4 -> Word32 -> Word32 ->
	IO (V.Vector w4)
run dv qf cb ppl plyt ds m w h = Vk.Dv.getQueue dv qf 0 >>= \q -> do
	Vk.CmdBuf.begin @'Nothing @'Nothing cb def $
		Vk.Cmd.bindPipelineCompute cb Vk.Ppl.BindPointCompute ppl \ccb -> do
			Vk.Cmd.bindDescriptorSetsCompute ccb plyt
				(U2 ds :** HL.Nil)
				(HL.Singleton
					$ HL.Nil :** HL.Nil)
			Vk.Cmd.pushConstantsCompute @'[ 'Vk.T.ShaderStageComputeBit ]
				ccb plyt (HL.Singleton (HL.Id (w :: Word32)))
			Vk.Cmd.dispatch ccb w h 1
	Vk.Queue.submit q (U4 submitInfo :** HL.Nil) Nothing
	Vk.Queue.waitIdle q
	Vk.Mm.read @nm4 @(VObj.List 256 w4 "") @(V.Vector w4) dv m def
	where	submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = TMaybe.N,
			Vk.submitInfoWaitSemaphoreDstStageMasks = HL.Nil,
			Vk.submitInfoCommandBuffers = cb :** HL.Nil,
			Vk.submitInfoSignalSemaphores = HL.Nil }

type Pixels = ((Int, Int), V.Vector Pixel)

readPixels :: FilePath -> IO Pixels
readPixels fp = (<$> P.readPng fp) \case
	Right (P.ImageRGBA8 img) -> ((P.imageWidth &&& P.imageHeight) &&&
			V.unsafeCast . P.imageData) img
	_ -> error "readPixels: can't get pixels"

writePixels :: FilePath -> Pixels -> IO ()
writePixels fp ((w, h), pxs) =
	P.writePng @P.PixelRGBA8 fp . P.Image w h $ V.unsafeCast pxs

data Pixel = Pixel Word8 Word8 Word8 Word8 deriving Show

type instance Vk.BffVw.FormatOf Pixel = 'Vk.T.FormatR8g8b8a8Uint

instance Storable Pixel where
	sizeOf _ = 4 * sizeOf @Word8 undefined
	alignment _ = alignment @Word8 undefined
	peek p = peekArray 4 (castPtr p) >>= \case
		[r, g, b, a] -> pure (Pixel r g b a); _ -> error "never occur"
	poke p (Pixel r g b a) = pokeArray (castPtr p) [r, g, b, a]

[glslComputeShader|

#version 460
layout(local_size_x = 1, local_size_y = 1) in;

layout(binding = 0, rgba8ui) uniform uimageBuffer storageTexelBuffer;
layout(push_constant) uniform PushConstant { uint width; } pushConstant;

void
main()
{
	int index = int(gl_GlobalInvocationID.x +
		gl_GlobalInvocationID.y * pushConstant.width);
	uvec4 px = imageLoad(storageTexelBuffer, index);
	px.r = 255 - px.r; px.g = 255 - px.g; px.b = 255 - px.b;
	imageStore(storageTexelBuffer, index, px);
}

|]
