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

import GHC.Types
import Foreign.Storable
import Data.Kind.Object
import Data.MonoTraversable
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
import qualified Gpu.Vulkan.TypeEnum as Vk.T
import qualified Gpu.Vulkan.Core as Vk.C
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

import qualified Gpu.Vulkan.Image as Vk.Image
import qualified Gpu.Vulkan.Image.Enum as Vk.Image
import qualified Gpu.Vulkan.Sample.Enum as Vk.Sample

import Sample.GetOpt
import Sample.Image
import Codec.Picture.Tools

import qualified Gpu.Vulkan.Memory as Vk.Mem
import qualified Gpu.Vulkan.Memory.Kind as Vk.Mem.K
import qualified Gpu.Vulkan.PushConstant as Vk.PushConstant

main :: IO ()
main = getOptions >>= maybe (pure ()) \(Opts opt ifp tlng_) -> do
	(r1, r2, r3) <- calc opt ifp (tilingToTiling tlng_) datA datB datC
	print . take 20 $ unW1 <$> r1
	print . take 20 $ unW2 <$> r2
	print . take 20 $ unW3 <$> r3

tilingToTiling :: Tiling -> Vk.Image.Tiling
tilingToTiling Optimal = Vk.Image.TilingOptimal
tilingToTiling Linear = Vk.Image.TilingLinear

newtype W1 = W1 { unW1 :: Word32 } deriving (Show, Storable)
newtype W2 = W2 { unW2 :: Word32 } deriving (Show, Storable)
newtype W3 = W3 { unW3 :: Word32 } deriving (Show, Storable)

dataSize :: Integral n => n
dataSize = 1000000

datA :: V.Vector W1; datA = V.replicate dataSize $ W1 3
datB :: V.Vector W2; datB = V.fromList $ W2 <$> [1 .. dataSize]
datC :: V.Vector W3; datC = V.replicate dataSize $ W3 0

calc :: forall w1 w2 w3 . (
	Show w1, Show w2, Show w3,
	Storable w1, Storable w2, Storable w3,
	Offset ('List 256 w2 "") (ListBuffer1 w1 w2 w3),
	Offset ('List 256 w3 "") (ListBuffer1 w1 w2 w3),
	Vk.Mem.OffsetSizeObject ('List 256 w2 "") '[ 'List 256 w1 "", 'List 256 w2 "", 'List 256 w3 ""],
	Vk.Mem.OffsetSizeObject ('List 256 w3 "") '[ 'List 256 w1 "", 'List 256 w2 "", 'List 256 w3 ""]
	) =>
	BufMem -> FilePath -> Vk.Image.Tiling -> V.Vector w1 -> V.Vector w2 -> V.Vector w3 ->
	IO ([w1], [w2], [w3])
calc opt ifp tlng da_ db_ dc_ = withDevice \phdvc qfam dvc maxX ->
	Vk.DscSetLyt.create dvc (dscSetLayoutInfo @w1 @w2 @w3) nil nil \dslyt ->
	let	n = fromIntegral maxX
		da = V.take n da_; db = V.take n db_; dc = V.take n dc_ in
	case opt of
		Buffer3Memory3 ->
			prepareMems33 phdvc dvc dslyt da db dc \dsst
				(ma :: Vk.Mem.M sm1 '[ '( sb1, 'Vk.Mem.K.Buffer nm1 '[ 'List 256 w1 ""])])
				(mb :: Vk.Mem.M sm2 '[ '( sb2, 'Vk.Mem.K.Buffer nm2 '[ 'List 256 w2 ""])])
				(mc :: Vk.Mem.M sm3 '[ '( sb3, 'Vk.Mem.K.Buffer nm3 '[ 'List 256 w3 ""])]) ->
			calc' dvc qfam dslyt dsst maxX (readMemories @nm1 @nm2 @nm3) ma mb mc
		Buffer3Memory1 ->
			prepareMems31 phdvc dvc dslyt da db dc \dsst m ->
			calc' dvc qfam dslyt dsst maxX (readMemories @"buffer1" @"buffer2" @"buffer3") m m m
		Buffer1Memory1 ->
			prepareMems11 ifp tlng phdvc dvc dslyt da db dc \dsst m2 ->
			calc' dvc qfam dslyt dsst maxX (readMemories' @"hello" @"hello" @"hello") m2 m2 m2

calc' :: Vk.Cmd.SetPos '[slbts] '[ '(sl, bts)] =>
	Vk.Dvc.D sd -> Vk.QFam.Index -> Vk.DscSetLyt.L sl bts ->
	Vk.DscSet.S sd sp slbts -> Word32 ->
	(Vk.Dvc.D sd -> m1 -> m2 -> m3 -> IO ([w1], [w2], [w3])) ->
	m1 -> m2 -> m3 -> IO ([w1], [w2], [w3])
calc' dvc qfam dscSetLyt dscSet dsz rm ma mb mc =
	Vk.Ppl.Lyt.createNew dvc (pplLayoutInfo dscSetLyt) nil nil \pplLyt ->
	Vk.Ppl.Cmpt.createCsNew dvc Nothing
		(Singleton . V4 $ cmptPipelineInfo pplLyt)
		nil nil \(Singleton (Vk.Ppl.Cmpt.Pipeline ppl)) ->
	Vk.CommandPool.create dvc (commandPoolInfo qfam) nil nil \cmdPool ->
	Vk.CmdBuf.allocateNew dvc (commandBufferInfo cmdPool) \(Singleton cmdBuf) ->
		run dvc qfam cmdBuf ppl pplLyt dscSet dsz rm ma mb mc

type ListBuffer1 w1 w2 w3 = '[ 'List 256 w1 "", 'List 256 w2 "", 'List 256 w3 ""]
type ListBuffer3Memory3 w1 w2 w3 = '[ '[ 'List 256 w1 ""], '[ 'List 256 w2 ""], '[ 'List 256 w3 ""]]

run :: forall w1 w2 w3 slbts sbtss sd sc vs sg sl sp m1 m2 m3 .
	Vk.Cmd.SetPos '[slbts] sbtss =>
	Vk.Dvc.D sd -> Vk.QFam.Index -> Vk.CmdBuf.C sc vs -> Vk.Ppl.Cmpt.C sg ->
	Vk.Ppl.Lyt.L sl sbtss '[] -> Vk.DscSet.S sd sp slbts -> Word32 -> (
		Vk.Dvc.D sd -> m1 -> m2 -> m3 -> IO ([w1], [w2], [w3]) ) ->
	m1 -> m2 -> m3 -> IO ([w1], [w2], [w3])
run dvc qfam cmdBuf ppl pplLyt dscSet dsz rm memA memB memC = do
	queue <- Vk.Dvc.getQueue dvc qfam 0
	Vk.CmdBuf.begin @() @() cmdBuf def do
		Vk.Cmd.bindPipelineCompute cmdBuf Vk.Ppl.BindPointCompute ppl
		Vk.Cmd.bindDescriptorSetsNew cmdBuf Vk.Ppl.BindPointCompute pplLyt
			(Vk.Cmd.DescriptorSet dscSet :...: HVNil) []
		Vk.Cmd.dispatch cmdBuf dsz 1 1
	Vk.Queue.submit queue (V4 submitInfo :...: HVNil) Nothing
	Vk.Queue.waitIdle queue
	rm dvc memA memB memC
	where	submitInfo :: Vk.SubmitInfo () _ _ _
		submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = Nothing,
			Vk.submitInfoWaitSemaphoreDstStageMasks = HVNil,
			Vk.submitInfoCommandBuffers = V2 cmdBuf :...: HVNil,
			Vk.submitInfoSignalSemaphores = HVNil }

readMemories :: forall (nm1 :: Symbol) (nm2 :: Symbol) (nm3 :: Symbol)
	sd sm1 sm2 sm3 objss1 objss2 objss3 w1 w2 w3 . (
--	Vk.Dvc.Mem.Buffer.OffsetSize ('List 256 w1 "") objss1,
--	Vk.Dvc.Mem.Buffer.OffsetSize ('List 256 w2 "") objss2,
--	Vk.Dvc.Mem.Buffer.OffsetSize ('List 256 w3 "") objss3,
	Vk.Mem.OffsetSize' nm1 ('List 256 w1 "") objss1,
	Vk.Mem.OffsetSize' nm2 ('List 256 w2 "") objss2,
	Vk.Mem.OffsetSize' nm3 ('List 256 w3 "") objss3,
	Storable w1, Storable w2, Storable w3
	) =>
	Vk.Dvc.D sd ->
	Vk.Mem.M sm1 objss1 ->
	Vk.Mem.M sm2 objss2 ->
	Vk.Mem.M sm3 objss3 -> IO ([w1], [w2], [w3])
readMemories dvc memA memB memC =
	(,,)	<$> Vk.Mem.read @nm1 @('List 256 w1 "") @[w1] dvc memA def
		<*> Vk.Mem.read @nm2 @('List 256 w2 "") @[w2] dvc memB def
		<*> Vk.Mem.read @nm3 @('List 256 w3 "") @[w3] dvc memC def

readMemories' :: forall nm1 nm2 nm3 sd sm1 sm2 sm3 objss1 objss2 objss3 w1 w2 w3 . (
	Vk.Mem.OffsetSize' nm1 ('List 256 w1 "") objss1,
	Vk.Mem.OffsetSize' nm2 ('List 256 w2 "") objss2,
	Vk.Mem.OffsetSize' nm3 ('List 256 w3 "") objss3,
	Storable w1, Storable w2, Storable w3
	) =>
	Vk.Dvc.D sd ->
	Vk.Mem.M sm1 objss1 ->
	Vk.Mem.M sm2 objss2 ->
	Vk.Mem.M sm3 objss3 -> IO ([w1], [w2], [w3])
readMemories' dvc memA memB memC =
	(,,)	<$> Vk.Mem.read @nm1 @('List 256 w1 "") @[w1] dvc memA def
		<*> Vk.Mem.read @nm2 @('List 256 w2 "") @[w2] dvc memB def
		<*> Vk.Mem.read @nm3 @('List 256 w3 "") @[w3] dvc memC def

withDevice ::
	(forall sd . Vk.PhDvc.P -> Vk.QFam.Index -> Vk.Dvc.D sd -> Word32 -> IO a) -> IO a
withDevice f = Vk.Inst.create @() @() instInfo nil nil \inst -> do
	phdvc <- head <$> Vk.PhDvc.enumerate inst
	limits <- Vk.PhDvc.propertiesLimits <$> Vk.PhDvc.getProperties phdvc
	let	maxGroupCountX :. _ =
			Vk.PhDvc.limitsMaxComputeWorkGroupCount limits
	putStrLn $ "maxGroupCountX: " ++ show maxGroupCountX
	qfam <- findQueueFamily phdvc Vk.Queue.ComputeBit
	Vk.Dvc.create @() @'[()] phdvc (dvcInfo qfam) nil nil $ \dvc -> f phdvc qfam dvc maxGroupCountX
	where
	dvcInfo qfam = Vk.Dvc.CreateInfo {
		Vk.Dvc.createInfoNext = Nothing,
		Vk.Dvc.createInfoFlags = def,
		Vk.Dvc.createInfoQueueCreateInfos = Singleton $ queueInfo qfam,
		Vk.Dvc.createInfoEnabledLayerNames =
			[Vk.Khr.validationLayerName],
		Vk.Dvc.createInfoEnabledExtensionNames = [],
		Vk.Dvc.createInfoEnabledFeatures = Nothing }
	queueInfo qfam = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = Nothing,
		Vk.Dvc.queueCreateInfoFlags = def,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qfam,
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

prepareMems33 ::
	forall bts w1 w2 w3 sd sl nm1 nm2 nm3 a . (
	Storable w1, Storable w2, Storable w3
	) =>
	Vk.DscSet.BindingAndArrayElem bts '[ 'List 256 w1 "", 'List 256 w2 "", 'List 256 w3 ""] =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.DscSetLyt.L sl bts ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (forall s sm1 sm2 sm3 sb1 sb2 sb3 .
		Vk.DscSet.S sd s '(sl, bts) ->
		Vk.Mem.M sm1 '[ '(sb1, 'Vk.Mem.K.Buffer nm1 '[ 'List 256 w1 ""])] ->
		Vk.Mem.M sm2 '[ '(sb2, 'Vk.Mem.K.Buffer nm2 '[ 'List 256 w2 ""])] ->
		Vk.Mem.M sm3 '[ '(sb3, 'Vk.Mem.K.Buffer nm3 '[ 'List 256 w3 ""])] -> IO a) -> IO a
prepareMems33 phdvc dvc dscSetLyt da db dc f =
	Vk.DscPool.create dvc dscPoolInfo nil nil \dscPool ->
	Vk.DscSet.allocateSs dvc (dscSetInfo dscPool dscSetLyt)
		>>= \(dscSet :...: HVNil) ->
	storageBufferNew3' dvc phdvc da db dc \ba ma bb mb bc mc ->
	Vk.DscSet.updateDs @() @() dvc (Vk.DscSet.Write_
		(writeDscSet @w1 @w2 @w3 dscSet ba bb bc) :...: HVNil) [] >>
	f dscSet ma mb mc

prepareMems31 ::
	forall bts w1 w2 w3 sd sl a . (
	Storable w1, Storable w2, Storable w3,
	Vk.DscSet.BindingAndArrayElem bts '[ 'List 256 w1 "", 'List 256 w2 "", 'List 256 w3 ""] ) =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.DscSetLyt.L sl bts ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (forall s sm sb1 sb2 sb3 .
		Vk.DscSet.S sd s '(sl, bts) ->
		Vk.Mem.M sm '[
			'(sb1, 'Vk.Mem.K.Buffer "buffer1" '[ 'List 256 w1 ""]),
			'(sb2, 'Vk.Mem.K.Buffer "buffer2" '[ 'List 256 w2 ""]),
			'(sb3, 'Vk.Mem.K.Buffer "buffer3" '[ 'List 256 w3 ""])
			] -> IO a) -> IO a
prepareMems31 phdvc dvc dscSetLyt da db dc f =
	Vk.DscPool.create dvc dscPoolInfo nil nil \dscPool ->
	Vk.DscSet.allocateSs dvc (dscSetInfo dscPool dscSetLyt)
		>>= \(dscSet :...: HVNil) ->
	storage3BufferNew dvc phdvc da db dc \ba bb bc m ->
	Vk.DscSet.updateDs @() @() dvc (Vk.DscSet.Write_
		(writeDscSet @w1 @w2 @w3 dscSet ba bb bc) :...: HVNil) [] >>
	f dscSet m

prepareMems11 :: forall w1 w2 w3 sd sl bts a nmi . (
	Show w1, Show w2, Show w3,
	Storable w1, Storable w2, Storable w3,
	Offset ('List 256 w2 "") '[ 'List 256 w1 "", 'List 256 w2 "", 'List 256 w3 "" ],
	Offset ('List 256 w3 "") '[ 'List 256 w1 "", 'List 256 w2 "", 'List 256 w3 "" ],
	Vk.DscSet.BindingAndArrayElem bts '[ 'List 256 w1 "", 'List 256 w2 "", 'List 256 w3 ""],
	Vk.Mem.OffsetSizeObject ('List 256 w2 "") '[ 'List 256 w1 "", 'List 256 w2 "", 'List 256 w3 ""],
	Vk.Mem.OffsetSizeObject ('List 256 w3 "") '[ 'List 256 w1 "", 'List 256 w2 "", 'List 256 w3 ""]
	) =>
	FilePath -> Vk.Image.Tiling ->
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.DscSetLyt.L sl bts ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (forall s sm' si sb .
		Vk.DscSet.S sd s '(sl, bts) ->
		Vk.Mem.M sm' '[
			'(si, 'Vk.Mem.K.Image nmi 'Vk.T.FormatR8g8b8a8Srgb),
			'(sb, 'Vk.Mem.K.Buffer "hello" '[ 'List 256 w1 "", 'List 256 w2 "", 'List 256 w3 ""]) ] ->
		IO a) -> IO a
prepareMems11 ifp tlng phdvc dvc dscSetLyt da db dc f =
	readRgba8 ifp >>= \img_ ->
	let	wdt = fromIntegral $ imageWidth img_
		hgt = fromIntegral $ imageHeight img_
		imgBody = ImageRgba8 $ imageData img_
		in
	print wdt >> print hgt >> print (olength imgBody) >>
	Vk.Image.createNew @() @() @() dvc (imageInfo wdt hgt tlng) nil nil \(img :: Vk.Image.INew simg nm fmt) ->
	storage1BufferNewNoBind dvc da db dc \(buf :: Vk.Buffer.B sb "hello" objs) ->
--	storage1BufferNew dvc phdvc da db dc \(buf' :: Vk.Buffer.B sb' nm' objs) bnd' m' ->
	let	imgbuf = V2 (Vk.Mem.Image img) :...:
			V2 (Vk.Mem.Buffer buf) :...:
			HVNil in
	Vk.Mem.getMemoryRequirementsList dvc imgbuf >>= \reqs ->
	print reqs >>
	Vk.PhDvc.getMemoryProperties phdvc >>= \mprops ->
	print mprops >>
	let	memTypeIdx =
			findMemoryTypeIndex reqs memoryPropertyBits mprops
		memInfo :: Vk.Dvc.Mem.Buffer.AllocateInfo ()
		memInfo = Vk.Dvc.Mem.Buffer.AllocateInfo {
			Vk.Dvc.Mem.Buffer.allocateInfoNext = Nothing,
			Vk.Dvc.Mem.Buffer.allocateInfoMemoryTypeIndex =
				memTypeIdx } in
	print memInfo >>
	Vk.Mem.allocateBind dvc imgbuf memInfo nil nil \(
		V2 (Vk.Mem.ImageBinded _imgb) :...:
		V2 (Vk.Mem.BufferBinded bufb) :...: HVNil) mib ->
	(print =<< Vk.Mem.offsetSize
		@"hello" @('List 256 w1 "") dvc mib 0) >>
	(print =<< Vk.Mem.offsetSize
		@"hello" @('List 256 w2 "") dvc mib 0) >>
	(print =<< Vk.Mem.offsetSize
		@"hello" @('List 256 w3 "") dvc mib 0) >>
	Vk.Mem.write @"hello" @('List 256 w1 "") dvc mib def da >>
	Vk.Mem.write @"hello" @('List 256 w2 "") dvc mib def db >>
	Vk.Mem.write @"hello" @('List 256 w3 "") dvc mib def dc >>
	(print @[w1] . take 10 =<< Vk.Mem.read @"hello" @('List 256 w1 "") dvc mib def) >>
	(print @[w2] . take 10 =<< Vk.Mem.read @"hello" @('List 256 w2 "") dvc mib def) >>
	(print @[w3] . take 10 =<< Vk.Mem.read @"hello" @('List 256 w3 "") dvc mib def) >>
	Vk.DscPool.create dvc dscPoolInfo nil nil \dscPool ->
	Vk.DscSet.allocateSs dvc (dscSetInfo dscPool dscSetLyt)
		>>= \(dscSet :...: HVNil) ->
	Vk.DscSet.updateDs @() @() dvc (Vk.DscSet.Write_
		(writeDscSet' @w1 @w2 @w3 dscSet bufb) :...: HVNil) [] >>
	f dscSet mib

imageInfo ::
	Word32 -> Word32 -> Vk.Image.Tiling -> Vk.Image.CreateInfoNew n 'Vk.T.FormatR8g8b8a8Srgb
imageInfo wdt hgt tlng = Vk.Image.CreateInfoNew {
	Vk.Image.createInfoNextNew = Nothing,
	Vk.Image.createInfoImageTypeNew = Vk.Image.Type2d,
	Vk.Image.createInfoExtentNew = Vk.C.Extent3d {
		Vk.C.extent3dWidth = wdt,
		Vk.C.extent3dHeight = hgt,
		Vk.C.extent3dDepth = 1 },
	Vk.Image.createInfoMipLevelsNew = 1,
	Vk.Image.createInfoArrayLayersNew = 1,
	Vk.Image.createInfoTilingNew = tlng,
	Vk.Image.createInfoInitialLayoutNew = Vk.Image.LayoutUndefined,
	Vk.Image.createInfoUsageNew = Vk.Image.UsageSampledBit,
	Vk.Image.createInfoSharingModeNew = Vk.SharingModeExclusive,
	Vk.Image.createInfoSamplesNew = Vk.Sample.Count1Bit,
	Vk.Image.createInfoFlagsNew = zeroBits,
	Vk.Image.createInfoQueueFamilyIndicesNew = [] }

storageBufferNew3' :: (Storable w1, Storable w2, Storable w3) =>
	Vk.Dvc.D sd -> Vk.PhDvc.P ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (
		forall sb1 sm1 sb2 sm2 sb3 sm3 .
		Vk.Buffer.Binded sb1 sm1 nm1 '[ 'List 256 w1 ""] ->
		Vk.Mem.M sm1 '[ '(sb1, 'Vk.Mem.K.Buffer nm1 '[ 'List 256 w1 ""])] ->
		Vk.Buffer.Binded sb2 sm2 nm2 '[ 'List 256 w2 ""] ->
		Vk.Mem.M sm2 '[ '(sb2, 'Vk.Mem.K.Buffer nm2 '[ 'List 256 w2 ""])] ->
		Vk.Buffer.Binded sb3 sm3 nm3 '[ 'List 256 w3 ""] ->
		Vk.Mem.M sm3 '[ '(sb3, 'Vk.Mem.K.Buffer nm3 '[ 'List 256 w3 ""])] -> IO a) -> IO a
storageBufferNew3' dvc phdvc x y z f =
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

storageBufferNew :: forall sd w a nm . Storable w =>
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

storage3BufferNew :: forall sd w1 w2 w3 a . (
	Storable w1, Storable w2, Storable w3
	) =>
	Vk.Dvc.D sd -> Vk.PhDvc.P ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (
		forall sb1 sb2 sb3 sm .
		Vk.Buffer.Binded sb1 sm "buffer1" '[ 'List 256 w1 ""] ->
		Vk.Buffer.Binded sb2 sm "buffer2" '[ 'List 256 w2 ""] ->
		Vk.Buffer.Binded sb3 sm "buffer3" '[ 'List 256 w3 ""] ->
		Vk.Mem.M sm '[
			'(sb1, 'Vk.Mem.K.Buffer "buffer1" '[ 'List 256 w1 ""]),
			'(sb2, 'Vk.Mem.K.Buffer "buffer2" '[ 'List 256 w2 ""]),
			'(sb3, 'Vk.Mem.K.Buffer "buffer3" '[ 'List 256 w3 ""]) ] -> IO a
		) -> IO a
storage3BufferNew dvc phdvc xs ys zs f =
	Vk.Buffer.create dvc (bufferInfo xs) nil nil \buf1 -> do
		memInfo1 <- getMemoryInfo phdvc dvc buf1
		Vk.Buffer.create dvc (bufferInfo ys) nil nil \buf2 -> do
			memInfo2 <- getMemoryInfo phdvc dvc buf2
			Vk.Buffer.create dvc (bufferInfo zs) nil nil \buf3 -> do
				memInfo3 <- getMemoryInfo phdvc dvc buf3
				if (memInfo1 == memInfo2 && memInfo2 == memInfo3) then
					Vk.Mem.allocateBind dvc (
						V2 (Vk.Mem.Buffer buf1) :...:
						V2 (Vk.Mem.Buffer buf2) :...:
						V2 (Vk.Mem.Buffer buf3) :...: HVNil
						) memInfo1 nil nil
						\(	V2 (Vk.Mem.BufferBinded bnd1) :...:
							V2 (Vk.Mem.BufferBinded bnd2) :...:
							V2 (Vk.Mem.BufferBinded bnd3) :...: HVNil ) mem -> do
						Vk.Mem.write @"buffer1" @('List 256 w1 "") dvc mem def xs
						Vk.Mem.write @"buffer2" @('List 256 w2 "") dvc mem def ys
						Vk.Mem.write @"buffer3" @('List 256 w3 "") dvc mem def zs
						f bnd1 bnd2 bnd3 mem
					else error "bad"

bufferInfo :: Storable w => V.Vector w -> Vk.Buffer.CreateInfo () '[ 'List 256 w ""]
bufferInfo xs = Vk.Buffer.CreateInfo {
	Vk.Buffer.createInfoNext = Nothing,
	Vk.Buffer.createInfoFlags = def,
	Vk.Buffer.createInfoLengths =
		ObjectLengthList (V.length xs) :...: HVNil,
	Vk.Buffer.createInfoUsage = Vk.Buffer.UsageStorageBufferBit,
	Vk.Buffer.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Buffer.createInfoQueueFamilyIndices = [] }

storage1BufferNewNoBind :: forall sd nm w1 w2 w3 a . (
	Storable w1, Storable w2, Storable w3 ) =>
	Vk.Dvc.D sd ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (
		forall sb .
		Vk.Buffer.B sb nm '[ 'List 256 w1 "", 'List 256 w2 "", 'List 256 w3 ""] -> IO a) -> IO a
storage1BufferNewNoBind dvc xs ys zs f =
	Vk.Buffer.create dvc (bufferInfo' xs ys zs) nil nil f

bufferInfo' :: (
	Storable w1, Storable w2, Storable w3 ) =>
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 ->
	Vk.Buffer.CreateInfo () '[ 'List 256 w1 "", 'List 256 w2 "", 'List 256 w3 ""]
bufferInfo' xs ys zs = Vk.Buffer.CreateInfo {
	Vk.Buffer.createInfoNext = Nothing,
	Vk.Buffer.createInfoFlags = def,
	Vk.Buffer.createInfoLengths =
		ObjectLengthList (V.length xs) :...:
		ObjectLengthList (V.length ys) :...:
		ObjectLengthList (V.length zs) :...: HVNil,
	Vk.Buffer.createInfoUsage = Vk.Buffer.UsageStorageBufferBit,
	Vk.Buffer.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Buffer.createInfoQueueFamilyIndices = [] }

pplLayoutInfo :: Vk.DscSetLyt.L sl bts ->
	Vk.Ppl.Lyt.CreateInfoNew () '[ '(sl, bts)]
		('Vk.PushConstant.PushConstantLayout '[] '[])
pplLayoutInfo dsl = Vk.Ppl.Lyt.CreateInfoNew {
	Vk.Ppl.Lyt.createInfoNextNew = Nothing,
	Vk.Ppl.Lyt.createInfoFlagsNew = def,
	Vk.Ppl.Lyt.createInfoSetLayoutsNew = V2 dsl :...: HVNil }

cmptPipelineInfo :: Vk.Ppl.Lyt.L sl sbtss '[] ->
	Vk.Ppl.Cmpt.CreateInfoNew ()
		'((), (), 'GlslComputeShader, (), (), ())
		'(sl, sbtss, '[]) sbph
cmptPipelineInfo pl = Vk.Ppl.Cmpt.CreateInfoNew {
	Vk.Ppl.Cmpt.createInfoNextNew = Nothing,
	Vk.Ppl.Cmpt.createInfoFlagsNew = def,
	Vk.Ppl.Cmpt.createInfoStageNew = V6 shaderStageInfo,
	Vk.Ppl.Cmpt.createInfoLayoutNew = V3 pl,
	Vk.Ppl.Cmpt.createInfoBasePipelineHandleNew = Nothing,
	Vk.Ppl.Cmpt.createInfoBasePipelineIndexNew = Nothing }

commandPoolInfo :: Vk.QFam.Index -> Vk.CommandPool.CreateInfo ()
commandPoolInfo qfam = Vk.CommandPool.CreateInfo {
	Vk.CommandPool.createInfoNext = Nothing,
	Vk.CommandPool.createInfoFlags =
		Vk.CommandPool.CreateResetCommandBufferBit,
	Vk.CommandPool.createInfoQueueFamilyIndex = qfam }

dscSetInfo :: Vk.DscPool.P sp -> Vk.DscSetLyt.L sl bts ->
	Vk.DscSet.AllocateInfo () sp '[ '(sl, bts)]
dscSetInfo pl lyt = Vk.DscSet.AllocateInfo {
	Vk.DscSet.allocateInfoNext = Nothing,
	Vk.DscSet.allocateInfoDescriptorPool = pl,
	Vk.DscSet.allocateInfoSetLayouts = Vk.DscSet.Layout lyt :...: HVNil }

commandBufferInfo :: Vk.CommandPool.C s -> Vk.CmdBuf.AllocateInfoNew () s vss
commandBufferInfo cmdPool = Vk.CmdBuf.AllocateInfoNew {
	Vk.CmdBuf.allocateInfoNextNew = Nothing,
	Vk.CmdBuf.allocateInfoCommandPoolNew = cmdPool,
	Vk.CmdBuf.allocateInfoLevelNew = Vk.CmdBuf.LevelPrimary }

dscPoolInfo :: Vk.DscPool.CreateInfo ()
dscPoolInfo = Vk.DscPool.CreateInfo {
	Vk.DscPool.createInfoNext = Nothing,
	Vk.DscPool.createInfoFlags = Vk.DscPool.CreateFreeDescriptorSetBit,
	Vk.DscPool.createInfoMaxSets = 1,
	Vk.DscPool.createInfoPoolSizes = [poolSize] }
	where poolSize = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeStorageBuffer,
		Vk.DscPool.sizeDescriptorCount = 10 }

getMemoryInfo :: Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Buffer.B sb nm objs ->
	IO (Vk.Dvc.Mem.Buffer.AllocateInfo ())
getMemoryInfo phdvc dvc buffer = do
	memTypeIdx <- findMemoryTypeIndex
		<$> ((: []) <$> Vk.Buffer.getMemoryRequirements dvc buffer)
		<*> pure memoryPropertyBits
		<*> Vk.PhDvc.getMemoryProperties phdvc
	pure Vk.Dvc.Mem.Buffer.AllocateInfo {
		Vk.Dvc.Mem.Buffer.allocateInfoNext = Nothing,
		Vk.Dvc.Mem.Buffer.allocateInfoMemoryTypeIndex = memTypeIdx }

memoryPropertyBits :: Vk.Mem.PropertyFlagBits
memoryPropertyBits =
	Vk.Mem.PropertyHostVisibleBit .|. Vk.Mem.PropertyHostCoherentBit

findMemoryTypeIndex ::
	[Vk.Mem.M.Requirements] -> Vk.Mem.PropertyFlags ->
	Vk.PhDvc.MemoryProperties -> Vk.Mem.M.TypeIndex
findMemoryTypeIndex requirementss memoryProp memoryProperties = do
	let	reqTypess = Vk.Mem.M.requirementsMemoryTypeBits <$> requirementss
		memPropTypes = (fst <$>)
			. filter (checkBits memoryProp
				. Vk.Mem.M.mTypePropertyFlags . snd)
			$ Vk.PhDvc.memoryPropertiesMemoryTypes memoryProperties
	case filter (\x -> all (Vk.Mem.M.elemTypeIndex x) reqTypess) memPropTypes of
		[] -> error "No available memory types"
		i : _ -> i

checkBits :: Bits bs => bs -> bs -> Bool
checkBits bs0 = (== bs0) . (.&. bs0)

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

writeDscSet' :: forall w1 w2 w3 sd sp slbts sb sm nm objs .
	Vk.DscSet.S sd sp slbts ->
	Vk.Buffer.Binded sm sb nm objs ->
	Vk.DscSet.Write () sd sp slbts ('Vk.DscSet.WriteSourcesArgBuffer '[
		'(sb, sm, nm, objs, 'List 256 w1 ""), '(sb, sm, nm, objs, 'List 256 w2 ""),
		'(sb, sm, nm, objs, 'List 256 w3 "") ])
writeDscSet' ds b = Vk.DscSet.Write {
	Vk.DscSet.writeNext = Nothing,
	Vk.DscSet.writeDstSet = ds,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		bufferInfoList @w1 b :...: bufferInfoList @w2 b :...:
		bufferInfoList @w3 b :...: HVNil }

bufferInfoList :: forall t {sb} {sm} {nm} {objs} .
	Vk.Buffer.Binded sm sb nm objs ->
	Vk.Dsc.BufferInfo '(sb, sm, nm, objs, 'List 256 t "")
bufferInfoList = Vk.Dsc.BufferInfoList

shaderStageInfo :: Vk.Ppl.ShaderSt.CreateInfo () () 'GlslComputeShader () () ()
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
