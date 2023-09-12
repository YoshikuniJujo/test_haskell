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
import qualified "try-gpu-vulkan" Gpu.Vulkan.Enum as Vk
import qualified Gpu.Vulkan.Instance as Vk.Ist
import qualified Gpu.Vulkan.PhysicalDevice as Vk.Phd
import qualified Gpu.Vulkan.Queue as Vk.Queue
import qualified Gpu.Vulkan.Queue.Enum as Vk.Queue
import qualified Gpu.Vulkan.QueueFamily as Vk.QFam
import qualified Gpu.Vulkan.Device as Vk.Dv
import qualified Gpu.Vulkan.CommandPool as Vk.CommandPool
import qualified "try-gpu-vulkan" Gpu.Vulkan.CommandPool.Enum as Vk.CommandPool
import qualified "try-gpu-vulkan" Gpu.Vulkan.Buffer.Enum as Vk.Buffer
import qualified Gpu.Vulkan.Memory as Vk.Mem
import qualified Gpu.Vulkan.Memory.Enum as Vk.Mem
import qualified Gpu.Vulkan.Memory as Vk.Mem.M
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

import Codec.Picture
import System.Environment
import System.FilePath

main :: IO ()
main = do
	inf : _ <- getArgs
	let outf = uncurry (++) . first (++ "_nega") $ splitExtension inf
	writePixels outf =<< makeNega =<< readPixels inf

makeNega :: Pixels -> IO Pixels
makeNega pxs = device \phd qf dvc ->
	Vk.DscSetLyt.create dvc dscSetLayoutInfo TPMaybe.N \dslyt ->
	pure pxs >>= \(sz@(fromIntegral -> w, fromIntegral -> h), dd') ->
	prepareMems phd dvc dslyt dd' \dscSet
		(md :: MemoryList sm4 sb4 nm4 MyPixel) ->
	calc' @nm4 dvc qf dslyt dscSet md w h >>= \r4 ->
	pure (sz, r4)

device :: (forall sd . Vk.Phd.P -> Vk.QFam.Index -> Vk.Dv.D sd -> IO a) -> IO a
device f = Vk.Ist.create @_ @'Nothing instInfo nil' \ist -> do
	phd <- head <$> Vk.Phd.enumerate ist
	qf <- findQueueFamily phd Vk.Queue.ComputeBit
	Vk.Dv.create phd (dvcInfo qf) nil' \dvc -> f phd qf dvc
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

findQueueFamily :: Vk.Phd.P -> Vk.Queue.FlagBits -> IO Vk.QFam.Index
findQueueFamily phd qf = fst . head
	. filter ((/= zeroBits) . (.&. qf) . Vk.QFam.propertiesQueueFlags . snd)
	<$> Vk.Phd.getQueueFamilyProperties phd

dscSetLayoutInfo :: Vk.DscSetLyt.CreateInfo 'Nothing '[
	'Vk.DscSetLyt.BufferView '[ '("", MyPixel)] ]
dscSetLayoutInfo = Vk.DscSetLyt.CreateInfo {
	Vk.DscSetLyt.createInfoNext = TMaybe.N,
	Vk.DscSetLyt.createInfoFlags = zeroBits,
	Vk.DscSetLyt.createInfoBindings = binding1 :** HL.Nil }

type MemoryList sm sb nm w =
	Vk.Mem.M sm '[ '( sb, 'Vk.Mem.BufferArg nm '[VObj.List 256 w ""])]

calc' :: forall nm4 w4 objss4 sm4 slbts sl bts sd sds . (
	Vk.DscSetLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[]],
	slbts ~ '(sl, bts),
	Storable w4,
	Vk.Mem.OffsetSize nm4 (VObj.List 256 w4 "") objss4,
	InfixIndex '[slbts] '[ '(sl, bts)]) =>
	Vk.Dv.D sd -> Vk.QFam.Index -> Vk.DscSetLyt.D sl bts ->
	Vk.DscSet.D sds slbts ->
	Vk.Mem.M sm4 objss4 -> Word32 -> Word32 -> IO (V.Vector w4)
calc' dvc qf dscSetLyt dscSet md w h =
	Vk.Ppl.Lyt.create dvc (pplLayoutInfo dscSetLyt) nil' \pplLyt ->
	Vk.Ppl.Cmpt.createCs
		dvc Nothing
		(U4 (computePipelineInfo pplLyt) :** HL.Nil)
		nil' \(ppl :** HL.Nil) ->
	Vk.CommandPool.create dvc (commandPoolInfo qf) nil' \cmdPool ->
	Vk.CmdBuf.allocate dvc (commandBufferInfo cmdPool) \(cmdBuf :*. HL.Nil) ->
		run @nm4 dvc qf cmdBuf ppl pplLyt dscSet md w h

pplLayoutInfo :: Vk.DscSetLyt.D sl bts ->
	Vk.Ppl.Lyt.CreateInfo 'Nothing '[ '(sl, bts)]
		('Vk.PushConstant.Layout
			'[Word32]
			'[ 'Vk.PushConstant.Range
				'[ 'Vk.TEnum.ShaderStageComputeBit] '[Word32]])
pplLayoutInfo dsl = Vk.Ppl.Lyt.CreateInfo {
	Vk.Ppl.Lyt.createInfoNext = TMaybe.N,
	Vk.Ppl.Lyt.createInfoFlags = zeroBits,
	Vk.Ppl.Lyt.createInfoSetLayouts = U2 dsl :** HL.Nil }

computePipelineInfo :: Vk.Ppl.Lyt.P sl sbtss '[Word32] ->
	Vk.Ppl.Cmpt.CreateInfo 'Nothing
		'( 'Nothing, 'Nothing, 'GlslComputeShader, 'Nothing, '[Word32, Word32])
		'(sl, sbtss, '[Word32]) sbph
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
	Vk.Ppl.ShaderSt.createInfoModule = (shaderModInfo, nil'),
	Vk.Ppl.ShaderSt.createInfoName = "main",
	Vk.Ppl.ShaderSt.createInfoSpecializationInfo = Just $ HL.Id 3 :** HL.Id 10 :** HL.Nil }
	where shaderModInfo = Vk.ShaderMod.CreateInfo {
		Vk.ShaderMod.createInfoNext = TMaybe.N,
		Vk.ShaderMod.createInfoFlags = zeroBits,
		Vk.ShaderMod.createInfoCode = glslComputeShaderMain }

commandPoolInfo :: Vk.QFam.Index -> Vk.CommandPool.CreateInfo 'Nothing
commandPoolInfo qf = Vk.CommandPool.CreateInfo {
	Vk.CommandPool.createInfoNext = TMaybe.N,
	Vk.CommandPool.createInfoFlags =
		Vk.CommandPool.CreateResetCommandBufferBit,
	Vk.CommandPool.createInfoQueueFamilyIndex = qf }

commandBufferInfo :: Vk.CommandPool.C s -> Vk.CmdBuf.AllocateInfo 'Nothing s '[ '()]
commandBufferInfo cmdPool = Vk.CmdBuf.AllocateInfo {
	Vk.CmdBuf.allocateInfoNext = TMaybe.N,
	Vk.CmdBuf.allocateInfoCommandPool = cmdPool,
	Vk.CmdBuf.allocateInfoLevel = Vk.CmdBuf.LevelPrimary }

run :: forall nm4 w4 objss4 slbts sbtss sd sc sg sl sm4 sds . (
	Vk.DscSetLyt.BindingTypeListBufferOnlyDynamics (TIndex.I1_2 slbts) ~ '[ '[]],
	sbtss ~ '[slbts],
	Storable w4,
	Vk.Mem.OffsetSize nm4 (VObj.List 256 w4 "") objss4,
	InfixIndex '[slbts] sbtss ) =>
	Vk.Dv.D sd -> Vk.QFam.Index -> Vk.CmdBuf.C sc ->
	Vk.Ppl.Cmpt.C sg '(sl, sbtss, '[Word32]) ->
	Vk.Ppl.Lyt.P sl sbtss '[Word32] -> Vk.DscSet.D sds slbts ->
	Vk.Mem.M sm4 objss4 -> Word32 -> Word32 ->
	IO (V.Vector w4)
run dvc qf cmdBuf ppl pplLyt dscSet memD w h = do
	queue <- Vk.Dv.getQueue dvc qf 0
	Vk.CmdBuf.begin @'Nothing @'Nothing cmdBuf def $
		Vk.Cmd.bindPipelineCompute cmdBuf Vk.Ppl.BindPointCompute ppl \ccb -> do
			Vk.Cmd.bindDescriptorSetsCompute ccb pplLyt
				(U2 dscSet :** HL.Nil)
				(HL.Singleton
					$ HL.Nil :** HL.Nil)
			Vk.Cmd.pushConstantsCompute @'[ 'Vk.TEnum.ShaderStageComputeBit ]
				ccb pplLyt (HL.Singleton (HL.Id (w :: Word32)))
			Vk.Cmd.dispatch ccb w h 1
	Vk.Queue.submit queue (U4 submitInfo :** HL.Nil) Nothing
	Vk.Queue.waitIdle queue
	Vk.Mem.read @nm4 @(VObj.List 256 w4 "") @(V.Vector w4) dvc memD def
	where	submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = TMaybe.N,
			Vk.submitInfoWaitSemaphoreDstStageMasks = HL.Nil,
			Vk.submitInfoCommandBuffers = cmdBuf :** HL.Nil,
			Vk.submitInfoSignalSemaphores = HL.Nil }

binding1 :: Vk.DscSetLyt.Binding ('Vk.DscSetLyt.BufferView '[ '("", MyPixel) ])
binding1 = Vk.DscSetLyt.BindingBufferView {
	Vk.DscSetLyt.bindingBufferViewDescriptorType =
		Vk.Dsc.TypeStorageTexelBuffer,
	Vk.DscSetLyt.bindingBufferViewStageFlags = Vk.ShaderStageComputeBit }

prepareMems ::
	forall bts sd sl nm4 a . (
	Default (HL.PL
		(HL.PL KObj.Length)
		(Vk.DscSetLyt.BindingTypeListBufferOnlyDynamics bts)),
	Vk.DscSet.BindingAndArrayElemBufferView bts '[ '("", MyPixel)] 0 ) =>
	Vk.Phd.P -> Vk.Dv.D sd -> Vk.DscSetLyt.D sl bts ->
	V.Vector MyPixel -> (
		forall sds sm4 sb4 .
		Vk.DscSet.D sds '(sl, bts) ->
		Vk.Mem.M sm4 '[ '(sb4, 'Vk.Mem.BufferArg nm4 '[VObj.List 256 MyPixel ""])]  ->
		IO a) -> IO a
prepareMems phd dvc dscSetLyt dd f =
	Vk.DscPool.create dvc dscPoolInfo nil' \dscPool ->
	Vk.DscSet.allocateDs dvc (dscSetInfo dscPool dscSetLyt)
		\((dscSet :: Vk.DscSet.D s '(sl, bts)) :** HL.Nil) ->
	storageBufferNew dvc phd dd
		\(bd :: Vk.Buffer.Binded sm sb nm '[VObj.List 256 MyPixel ""]) md ->
	let	bufferViewInfo' :: Vk.BufferView.CreateInfo 'Nothing MyPixel ""
			'(sm, sb, nm, '[VObj.List 256 MyPixel ""])
		bufferViewInfo' = Vk.BufferView.CreateInfo {
			Vk.BufferView.createInfoNext = TMaybe.N,
			Vk.BufferView.createInfoFlags = zeroBits,
			Vk.BufferView.createInfoBuffer = U4 bd } in
	Vk.BufferView.create dvc bufferViewInfo' nil' \(bv :: Vk.BufferView.B s3 "" MyPixel) -> do
	let	wds' :: Vk.DscSet.Write 'Nothing s '(sl, bts)
			(Vk.DscSet.WriteSourcesArgBufferView '[ '(s3, "", MyPixel)]) 0
		wds' = Vk.DscSet.Write {
			Vk.DscSet.writeNext = TMaybe.N,
			Vk.DscSet.writeDstSet = dscSet,
			Vk.DscSet.writeDescriptorType =
				Vk.Dsc.TypeStorageTexelBuffer,
			Vk.DscSet.writeSources = Vk.DscSet.TexelBufferViews
				. HL.Singleton $ U3 bv }
	Vk.DscSet.updateDs dvc (U5 wds' :** HL.Nil) HL.Nil
	f dscSet md

type Pixels = ((Int, Int), V.Vector MyPixel)

readPixels :: FilePath -> IO Pixels
readPixels fp = do
	Right (ImageRGBA8 girl) <- readPng fp
	pure $ ((imageWidth &&& imageHeight) &&& V.unsafeCast . imageData) girl

writePixels :: FilePath -> Pixels -> IO ()
writePixels fp ((w, h), pxs) =
	writePng @PixelRGBA8 fp . Image w h $ V.unsafeCast pxs

data MyPixel = MyPixel Word8 Word8 Word8 Word8 deriving Show

type instance Vk.BufferView.FormatOf MyPixel = 'Vk.TEnum.FormatR8g8b8a8Uint

instance Storable MyPixel where
	sizeOf _ = 4 * sizeOf @Word8 undefined
	alignment _ = alignment @Word8 undefined
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
	Vk.DscSet.allocateInfoSetLayouts = U2 lyt :** HL.Nil }

storageBufferNew :: forall sd nm w a . Storable w =>
	Vk.Dv.D sd -> Vk.Phd.P -> V.Vector w -> (
		forall sb sm .
		Vk.Buffer.Binded sm sb nm '[VObj.List 256 w ""]  ->
		Vk.Mem.M sm '[ '(sb, 'Vk.Mem.BufferArg nm '[VObj.List 256 w ""])] -> IO a ) -> IO a
storageBufferNew dvc phd xs f =
	Vk.Buffer.create dvc (bufferInfo xs) nil' \buffer -> do
		memoryInfo <- getMemoryInfo phd dvc buffer
		Vk.Mem.allocateBind dvc (U2 (Vk.Mem.Buffer buffer) :** HL.Nil) memoryInfo
			nil' \(U2 (Vk.Mem.BufferBinded binded) :** HL.Nil) memory -> do
			Vk.Mem.write @nm @(VObj.List 256 w "") dvc memory def xs
			f binded memory

bufferInfo :: Storable w => V.Vector w -> Vk.Buffer.CreateInfo 'Nothing '[VObj.List 256 w ""]
bufferInfo xs = Vk.Buffer.CreateInfo {
	Vk.Buffer.createInfoNext = TMaybe.N,
	Vk.Buffer.createInfoFlags = def,
	Vk.Buffer.createInfoLengths =
		VObj.LengthList (fromIntegral $ V.length xs) :** HL.Nil,
	Vk.Buffer.createInfoUsage =
		Vk.Buffer.UsageStorageBufferBit .|.
		Vk.Buffer.UsageStorageTexelBufferBit,
	Vk.Buffer.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Buffer.createInfoQueueFamilyIndices = [] }

getMemoryInfo :: Vk.Phd.P -> Vk.Dv.D sd -> Vk.Buffer.B sb nm objs ->
	IO (Vk.Mem.AllocateInfo 'Nothing)
getMemoryInfo phd dvc buffer = do
	requirements <- Vk.Buffer.getMemoryRequirements dvc buffer
	memTypeIdx <- findMemoryTypeIndex phd requirements (
		Vk.Mem.PropertyHostVisibleBit .|.
		Vk.Mem.PropertyHostCoherentBit )
	pure Vk.Mem.AllocateInfo {
		Vk.Mem.allocateInfoNext = TMaybe.N,
		Vk.Mem.allocateInfoMemoryTypeIndex = memTypeIdx }

findMemoryTypeIndex ::
	Vk.Phd.P -> Vk.Mem.M.Requirements -> Vk.Mem.PropertyFlags ->
	IO Vk.Mem.M.TypeIndex
findMemoryTypeIndex physicalDevice requirements memoryProp = do
	memoryProperties <- Vk.Phd.getMemoryProperties physicalDevice
	let	reqTypes = Vk.Mem.M.requirementsMemoryTypeBits requirements
		memPropTypes = (fst <$>)
			. filter (checkBits memoryProp
				. Vk.Mem.M.mTypePropertyFlags . snd)
			$ Vk.Phd.memoryPropertiesMemoryTypes memoryProperties
	case filter (`Vk.Mem.M.elemTypeIndex` reqTypes) memPropTypes of
		[] -> error "No available memory types"
		i : _ -> pure i

checkBits :: Bits bs => bs -> bs -> Bool
checkBits bs0 = (== bs0) . (.&. bs0)

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
