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

import qualified Gpu.Vulkan.Memory as Vk.Mm

import GHC.Types
import Foreign.Storable
import Gpu.Vulkan.Object.Base qualified as BObj
import Gpu.Vulkan.Object qualified as Obj
import Data.MonoTraversable
import Data.Default
import Data.Bits
import Data.List.Length
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import qualified Data.HeteroParList as HPList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Word

import qualified Data.Vector.Storable as V

import Language.SpirV.ShaderKind
import Language.SpirV.Shaderc.TH
import Data.TypeLevel.ParMaybe (nil)

import qualified Gpu.Vulkan as Vk
import qualified Gpu.Vulkan.TypeEnum as Vk.T
import qualified Gpu.Vulkan.Instance as Vk.Inst
import qualified Gpu.Vulkan.PhysicalDevice as Vk.Phd
import qualified Gpu.Vulkan.Queue as Vk.Q
import qualified Gpu.Vulkan.QueueFamily as Vk.QFam
import qualified Gpu.Vulkan.Device as Vk.Dvc
import qualified Gpu.Vulkan.CommandPool as Vk.CmdPl
import qualified Gpu.Vulkan.Memory as Vk.Mm.M
import qualified Gpu.Vulkan.Descriptor as Vk.Dsc
import qualified Gpu.Vulkan.DescriptorPool as Vk.DscPl
import qualified Gpu.Vulkan.ShaderModule as Vk.ShaderMod
import qualified "try-gpu-vulkan" Gpu.Vulkan.Pipeline as Vk.Ppl
import qualified Gpu.Vulkan.PipelineLayout as Vk.PplLyt
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShaderSt
import qualified Gpu.Vulkan.Pipeline.Compute as Vk.Ppl.Cp
import qualified Gpu.Vulkan.DescriptorSet as Vk.DscSt
import qualified Gpu.Vulkan.CommandBuffer as Vk.CBffr
import qualified Gpu.Vulkan.Cmd as Vk.Cmd

import qualified Gpu.Vulkan.Buffer as Vk.Bffr
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscStLyt

import qualified Gpu.Vulkan.Image as Vk.Img
import qualified Gpu.Vulkan.Sample as Vk.Sample

import Sample.GetOpt
import Sample.Image

import qualified Gpu.Vulkan.PushConstant as Vk.PshCnst

import Tools (readRgba8, Image(..))

import Data.Bits.ToolsYj
import Data.Tuple.ToolsYj

main :: IO ()
main = getOptions >>= maybe (pure ()) \(Opts opt ifp tlng) -> do
	rs <- realMain opt ifp (tilingToTiling tlng) datA datB datC
	mapTup3M_ (print . take 20)
		$ appTup3 (unW1 <$>) (unW2 <$>) (unW3 <$>) rs

datA :: V.Vector W1; datA = V.replicate dataSize $ W1 3
datB :: V.Vector W2; datB = V.fromList $ W2 <$> [1 .. dataSize]
datC :: V.Vector W3; datC = V.replicate dataSize $ W3 0

newtype W1 = W1 { unW1 :: Word32 } deriving (Show, Storable)
newtype W2 = W2 { unW2 :: Word32 } deriving (Show, Storable)
newtype W3 = W3 { unW3 :: Word32 } deriving (Show, Storable)

dataSize :: Integral n => n
dataSize = 1000000

realMain :: forall w1 w2 w3 . (
	Show w1, Show w2, Show w3,
	Storable w1, Storable w2, Storable w3,
	Obj.OffsetRange (OList w2) '[OList w1, OList w2, OList w3] 0,
	Obj.OffsetRange (OList w3) '[OList w1, OList w2, OList w3] 0,
	Obj.LengthOf (OList w2) '[OList w1, OList w2, OList w3],
	Obj.LengthOf (OList w3) '[OList w1, OList w2, OList w3] ) =>
	OptBffMm -> FilePath -> Vk.Img.Tiling ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> IO ([w1], [w2], [w3])
realMain opt ifp tlng da_ db_ dc_ = withDvc \pd d q cpl n ->
	Vk.DscStLyt.create d dslinfo nil \dsl ->
	let	da = V.take n da_; db = V.take n db_; dc = V.take n dc_ in
	case opt of
		Bffr3Mm3 -> createBffr3Mm3 pd d dsl da db dc \dss
				(ma :: Mm sm1 '[BffrArg sb1 nm1 '[OList w1]])
				(mb :: Mm sm2 '[BffrArg sb2 nm2 '[OList w2]])
				(mc :: Mm sm3 '[BffrArg sb3 nm3 '[OList w3]]) ->
			calc @nm1 @nm2 @nm3 d q cpl dsl dss n ma mb mc
		Bffr3Mm1 -> createBffr3Mm1 pd d dsl da db dc \dss
				(m :: Mm sm '[
					BffrArg sb1 nm1 '[OList w1],
					BffrArg sb2 nm2 '[OList w2],
					BffrArg sb3 nm3 '[OList w3] ]) ->
			calc @nm1 @nm2 @nm3 d q cpl dsl dss n m m m
		Bffr1Mm1 -> createBffr1Mm1 ifp tlng pd d dsl da db dc \dss
				(m :: Mm sm '[
					'(sb0, Vk.Mm.ImageArg nmi
						Vk.T.FormatR8g8b8a8Srgb),
					'(sb, 'Vk.Mm.BufferArg nm
						'[OList w1, OList w2, OList w3])
					]) ->
			calc @nm @nm @nm d q cpl dsl dss n m m m
	where
	dslinfo :: Vk.DscStLyt.CreateInfo
		'Nothing '[ 'Vk.DscStLyt.Buffer '[OList w1, OList w2, OList w3]]
	dslinfo = Vk.DscStLyt.CreateInfo {
		Vk.DscStLyt.createInfoNext = TMaybe.N,
		Vk.DscStLyt.createInfoFlags = def,
		Vk.DscStLyt.createInfoBindings = HPList.Singleton bdg }
	bdg = Vk.DscStLyt.BindingBuffer {
		Vk.DscStLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeStorageBuffer,
		Vk.DscStLyt.bindingBufferStageFlags = Vk.ShaderStageComputeBit }

type Mm = Vk.Mm.M
type BffrArg sb nm ts = '(sb, Vk.Mm.BufferArg nm ts)
type OList t = Obj.List 256 t ""

withDvc :: (forall sd scpl .
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C scpl ->
	(forall c . Integral c => c) -> IO a) -> IO a
withDvc a = Vk.Inst.create iinfo nil \ist -> do
	pd <- head <$> Vk.Phd.enumerate ist
	mgcx :. _ <- Vk.Phd.limitsMaxComputeWorkGroupCount
		. Vk.Phd.propertiesLimits <$> Vk.Phd.getProperties pd
	qfi <- fst . head . filter (
			checkBits Vk.Q.ComputeBit .
			Vk.QFam.propertiesQueueFlags . snd )
		<$> Vk.Phd.getQueueFamilyProperties pd
	Vk.Dvc.create pd (dvcInfo qfi) nil \dv ->
		Vk.Dvc.getQueue dv qfi 0 >>= \q ->
		Vk.CmdPl.create dv (cpinfo qfi) nil \cpl ->
		a pd dv q cpl $ fromIntegral mgcx
	where
	iinfo :: Vk.Inst.CreateInfo 'Nothing 'Nothing
	iinfo = def {
		Vk.Inst.createInfoEnabledLayerNames =
			[Vk.layerKhronosValidation] }
	cpinfo qfi = Vk.CmdPl.CreateInfo {
		Vk.CmdPl.createInfoNext = TMaybe.N,
		Vk.CmdPl.createInfoFlags = Vk.CmdPl.CreateResetCommandBufferBit,
		Vk.CmdPl.createInfoQueueFamilyIndex = qfi }

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

calc :: forall
	nm1 nm2 nm3 w1 w2 w3 sl bts sd scpl
	sds sm1 sm2 sm3 objss1 objss2 objss3 . (
	Storable w1, Storable w2, Storable w3,
	Vk.Mm.OffsetSize nm1 (OList w1) objss1 0,
	Vk.Mm.OffsetSize nm2 (OList w2) objss2 0,
	Vk.Mm.OffsetSize nm3 (OList w3) objss3 0,
	Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[]] ) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C scpl -> Vk.DscStLyt.D sl bts ->
	Vk.DscSt.D sds '(sl, bts) -> Word32 ->
	Mm sm1 objss1 -> Mm sm2 objss2 -> Mm sm3 objss3 -> IO ([w1], [w2], [w3])
calc dv q cpl dsl dss n ma mb mc =
	Vk.PplLyt.create dv plinfo nil \pl ->
	Vk.Ppl.Cp.createCs dv Nothing
		(HPList.Singleton . U4 $ cpplinfo pl) nil
		\(cppl :** HPList.Nil) ->
	Vk.CBffr.allocate dv cbinfo \(cb :*. HPList.Nil) ->
	run @nm1 @nm2 @nm3 dv q cb cppl pl dss n ma mb mc
	where
	plinfo :: Vk.PplLyt.CreateInfo
		'Nothing '[ '(sl, bts)] ('Vk.PshCnst.Layout '[] '[])
	plinfo = Vk.PplLyt.CreateInfo {
		Vk.PplLyt.createInfoNext = TMaybe.N,
		Vk.PplLyt.createInfoFlags = zeroBits,
		Vk.PplLyt.createInfoSetLayouts = HPList.Singleton $ U2 dsl }
	cpplinfo pl = Vk.Ppl.Cp.CreateInfo {
		Vk.Ppl.Cp.createInfoNext = TMaybe.N,
		Vk.Ppl.Cp.createInfoFlags = zeroBits,
		Vk.Ppl.Cp.createInfoStage = U5 shdrStInfo,
		Vk.Ppl.Cp.createInfoLayout = U3 pl,
		Vk.Ppl.Cp.createInfoBasePipelineHandleOrIndex = Nothing }
	cbinfo :: Vk.CBffr.AllocateInfo 'Nothing scpl '[ '()]
	cbinfo = Vk.CBffr.AllocateInfo {
		Vk.CBffr.allocateInfoNext = TMaybe.N,
		Vk.CBffr.allocateInfoCommandPool = cpl,
		Vk.CBffr.allocateInfoLevel = Vk.CBffr.LevelPrimary }

shdrStInfo :: Vk.Ppl.ShaderSt.CreateInfo
	'Nothing 'Nothing 'GlslComputeShader 'Nothing '[]
shdrStInfo = Vk.Ppl.ShaderSt.CreateInfo {
	Vk.Ppl.ShaderSt.createInfoNext = TMaybe.N,
	Vk.Ppl.ShaderSt.createInfoFlags = zeroBits,
	Vk.Ppl.ShaderSt.createInfoStage = Vk.ShaderStageComputeBit,
	Vk.Ppl.ShaderSt.createInfoModule = (mdinfo, nil),
	Vk.Ppl.ShaderSt.createInfoName = "main",
	Vk.Ppl.ShaderSt.createInfoSpecializationInfo = Nothing }
	where mdinfo = Vk.ShaderMod.CreateInfo {
		Vk.ShaderMod.createInfoNext = TMaybe.N,
		Vk.ShaderMod.createInfoFlags = zeroBits,
		Vk.ShaderMod.createInfoCode = glslComputeShaderMain }

run :: forall
	nm1 nm2 nm3 w1 w2 w3 slbts sd scb scppl spl
	sds sm1 sm2 sm3 objss1 objss2 objss3 . (
	Storable w1, Storable w2, Storable w3,
	Vk.Mm.OffsetSize nm1 (Obj.List 256 w1 "") objss1 0,
	Vk.Mm.OffsetSize nm2 (Obj.List 256 w2 "") objss2 0,
	Vk.Mm.OffsetSize nm3 (Obj.List 256 w3 "") objss3 0,
	Vk.Cmd.LayoutArgListOnlyDynamics '[slbts] ~ '[ '[ '[]]] ) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CBffr.C scb ->
	Vk.Ppl.Cp.C scppl '(spl, '[slbts], '[]) ->
	Vk.PplLyt.P spl '[slbts] '[] -> Vk.DscSt.D sds slbts ->
	Word32 -> Mm sm1 objss1 -> Mm sm2 objss2 -> Mm sm3 objss3 ->
	IO ([w1], [w2], [w3])
run dv q cb cppl pl dss n ma mb mc = do
	Vk.CBffr.begin @'Nothing @'Nothing cb def $
		Vk.Cmd.bindPipelineCompute
			cb Vk.Ppl.BindPointCompute cppl \ccb ->
			Vk.Cmd.bindDescriptorSetsCompute ccb pl
				(HPList.Singleton $ U2 dss)
				(HPList.Singleton2 HPList.Nil) >>
			Vk.Cmd.dispatch ccb n 1 1
	Vk.Q.submit q (U4 sinfo :** HPList.Nil) Nothing
	Vk.Q.waitIdle q
	(,,)	<$> Vk.Mm.read @nm1 @(OList w1) @0 @[w1] dv ma def
		<*> Vk.Mm.read @nm2 @(OList w2) @0 @[w2] dv mb def
		<*> Vk.Mm.read @nm3 @(OList w3) @0 @[w3] dv mc def
	where sinfo = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Nil }

-- BUFFER 3 : MEMORY 3

createBffr3Mm3 :: forall nm1 nm2 nm3 w1 w2 w3 sd sdl bts a . (
	Storable w1, Storable w2, Storable w3,
	Vk.DscSt.BindingAndArrayElemBuffer
		bts '[OList w1, OList w2, OList w3] 0,
	Vk.DscSt.UpdateDynamicLength bts '[OList w1, OList w2, OList w3],
	Default (HPList.PL2 BObj.Length
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts)) ) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.DscStLyt.D sdl bts ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (
		forall sds sm1 sm2 sm3 sb1 sb2 sb3 .
		Vk.DscSt.D sds '(sdl, bts) ->
		Vk.Mm.M sm1 '[ '(sb1, 'Vk.Mm.BufferArg nm1 '[OList w1])] ->
		Vk.Mm.M sm2 '[ '(sb2, 'Vk.Mm.BufferArg nm2 '[OList w2])] ->
		Vk.Mm.M sm3 '[ '(sb3, 'Vk.Mm.BufferArg nm3 '[OList w3])] ->
		IO a) -> IO a
createBffr3Mm3 pd dv dsl da db dc a =
	Vk.DscPl.create dv dscPlInfo nil \dsp ->
	Vk.DscSt.allocateDs dv (dscStInfo dsp dsl) \(HPList.Singleton dss) ->
	bffr3Mm3 pd dv da db dc \ba bb bc ma mb mc ->
	Vk.DscSt.updateDs dv
		(HPList.Singleton
			. U5 $ writeDscStBffr3 @w1 @w2 @w3 dss ba bb bc)
		HPList.Nil >> a dss ma mb mc

bffr3Mm3 :: (Storable w1, Storable w2, Storable w3) =>
	Vk.Phd.P -> Vk.Dvc.D sd ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (
		forall sm1 sm2 sm3 sb1 sb2 sb3 .
		Vk.Bffr.Binded sm1 sb1 nm1 '[OList w1] ->
		Vk.Bffr.Binded sm2 sb2 nm2 '[OList w2] ->
		Vk.Bffr.Binded sm3 sb3 nm3 '[OList w3] ->
		Vk.Mm.M sm1 '[ '(sb1, 'Vk.Mm.BufferArg nm1 '[OList w1])] ->
		Vk.Mm.M sm2 '[ '(sb2, 'Vk.Mm.BufferArg nm2 '[OList w2])] ->
		Vk.Mm.M sm3 '[ '(sb3, 'Vk.Mm.BufferArg nm3 '[OList w3])] ->
		IO a ) -> IO a
bffr3Mm3 pd dv x y z a = bffrMms pd dv (x :** y :** z :** HPList.Nil) $ arg3 a

class BffrMms f a where
	type Vectors f :: [Type]
	bffrMms :: Vk.Phd.P ->
		Vk.Dvc.D sd -> HPList.PL V.Vector (Vectors f) -> f -> IO a

instance BffrMms (IO a) a where
	type Vectors (IO a) = '[]
	bffrMms _pd _dv HPList.Nil f = f

instance (Storable w, BffrMms f a) => BffrMms (Arg nm w f) a where
	type Vectors (Arg nm w f) = w ': Vectors f
	bffrMms pd dv (vs :** vss) (Arg f) =
		bffrMm pd dv vs \b m -> bffrMms @f @a pd dv vss $ f b m

data Arg nm w f = Arg (forall sm sb .
	Vk.Bffr.Binded sm sb nm '[OList w] ->
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[OList w])] -> f)

bffrMm :: forall sd nm w a . Storable w =>
	Vk.Phd.P -> Vk.Dvc.D sd -> V.Vector w -> (
		forall sm sb . Vk.Bffr.Binded sm sb nm '[OList w]  ->
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[OList w])] ->
		IO a ) -> IO a
bffrMm phdvc dvc xs f =
	Vk.Bffr.create dvc (bufferInfo xs) nil \buffer -> do
		memoryInfo <- getMemoryInfo phdvc dvc buffer
		Vk.Mm.allocateBind dvc (U2 (Vk.Mm.Buffer buffer) :** HPList.Nil) memoryInfo
			nil \(U2 (Vk.Mm.BufferBinded binded) :** HPList.Nil) memory -> do
			Vk.Mm.write @nm @(Obj.List 256 w "") @0 dvc memory def xs
			f binded memory

createBffr3Mm1 ::
	forall bts w1 w2 w3 sd sl a . (
	Default (HPList.PL2 BObj.Length
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts)),
	Storable w1, Storable w2, Storable w3,
	Vk.DscSt.BindingAndArrayElemBuffer bts '[Obj.List 256 w1 "",Obj.List 256 w2 "",Obj.List 256 w3 ""] 0,
	Vk.DscSt.UpdateDynamicLength bts '[Obj.List 256 w1 "",Obj.List 256 w2 "",Obj.List 256 w3 ""]
	) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.DscStLyt.D sl bts ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (forall sds sm sb1 sb2 sb3 .
		Vk.DscSt.D sds '(sl, bts) ->
		Vk.Mm.M sm '[
			'(sb1, 'Vk.Mm.BufferArg "buffer1" '[Obj.List 256 w1 ""]),
			'(sb2, 'Vk.Mm.BufferArg "buffer2" '[Obj.List 256 w2 ""]),
			'(sb3, 'Vk.Mm.BufferArg "buffer3" '[Obj.List 256 w3 ""])
			] -> IO a) -> IO a
createBffr3Mm1 phdvc dvc dscSetLyt da db dc f =
	Vk.DscPl.create dvc dscPlInfo nil \dscPool ->
	Vk.DscSt.allocateDs dvc (dscStInfo dscPool dscSetLyt)
		\(dscSet :** HPList.Nil) ->
	storage3BufferNew dvc phdvc da db dc \ba bb bc m ->
	Vk.DscSt.updateDs dvc (U5
		(writeDscStBffr3 @w1 @w2 @w3 dscSet ba bb bc) :** HPList.Nil) HPList.Nil >>
	f dscSet m

createBffr1Mm1 :: forall w1 w2 w3 sd sl bts a nmi . (
	Default (HPList.PL
		(HPList.PL BObj.Length)
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts)),
	Show w1, Show w2, Show w3,
	Storable w1, Storable w2, Storable w3,
	Obj.OffsetRange (Obj.List 256 w2 "") '[Obj.List 256 w1 "",Obj.List 256 w2 "",Obj.List 256 w3 "" ] 0,
	Obj.OffsetRange (Obj.List 256 w3 "") '[Obj.List 256 w1 "",Obj.List 256 w2 "",Obj.List 256 w3 "" ] 0,
	Vk.DscSt.BindingAndArrayElemBuffer bts '[Obj.List 256 w1 "",Obj.List 256 w2 "",Obj.List 256 w3 ""] 0,
	Vk.DscSt.UpdateDynamicLength bts '[Obj.List 256 w1 "",Obj.List 256 w2 "",Obj.List 256 w3 ""],
	Obj.LengthOf (Obj.List 256 w2 "") '[Obj.List 256 w1 "",Obj.List 256 w2 "",Obj.List 256 w3 ""],
	Obj.LengthOf (Obj.List 256 w3 "") '[Obj.List 256 w1 "",Obj.List 256 w2 "",Obj.List 256 w3 ""]
	) =>
	FilePath -> Vk.Img.Tiling ->
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.DscStLyt.D sl bts ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (forall sds sm' si sb .
		Vk.DscSt.D sds '(sl, bts) ->
		Vk.Mm.M sm' '[
			'(si, 'Vk.Mm.ImageArg nmi 'Vk.T.FormatR8g8b8a8Srgb),
			'(sb, 'Vk.Mm.BufferArg "hello" '[Obj.List 256 w1 "",Obj.List 256 w2 "",Obj.List 256 w3 ""]) ] ->
		IO a) -> IO a
createBffr1Mm1 ifp tlng phdvc dvc dscSetLyt da db dc f =
	readRgba8 ifp >>= \img_ ->
	let	wdt = fromIntegral $ imageWidth img_
		hgt = fromIntegral $ imageHeight img_
		imgBody = ImageRgba8 $ imageData img_
		in
	print wdt >> print hgt >> print (olength imgBody) >>
	Vk.Img.create @'Nothing dvc (imageInfo wdt hgt tlng) nil \(img :: Vk.Img.I simg nm fmt) ->
	storage1BufferNewNoBind dvc da db dc \(buf :: Vk.Bffr.B sb "hello" objs) ->
--	storage1BufferNew dvc phdvc da db dc \(buf' :: Vk.Bffr.B sb' nm' objs) bnd' m' ->
	let	imgbuf = U2 (Vk.Mm.Image img) :**
			U2 (Vk.Mm.Buffer buf) :**
			HPList.Nil in
	Vk.Mm.getRequirementsList dvc imgbuf >>= \reqs ->
	print reqs >>
	Vk.Phd.getMemoryProperties phdvc >>= \mprops ->
	print mprops >>
	let	memTypeIdx =
			findMemoryTypeIndex reqs memoryPropertyBits mprops
		memInfo :: Vk.Mm.AllocateInfo 'Nothing
		memInfo = Vk.Mm.AllocateInfo {
			Vk.Mm.allocateInfoNext = TMaybe.N,
			Vk.Mm.allocateInfoMemoryTypeIndex =
				memTypeIdx } in
	print memInfo >>
	Vk.Mm.allocateBind dvc imgbuf memInfo nil \(
		U2 (Vk.Mm.ImageBinded _imgb) :**
		U2 (Vk.Mm.BufferBinded bufb) :** HPList.Nil) mib ->
	Vk.Mm.write @"hello" @(Obj.List 256 w1 "") @0 dvc mib def da >>
	Vk.Mm.write @"hello" @(Obj.List 256 w2 "") @0 dvc mib def db >>
	Vk.Mm.write @"hello" @(Obj.List 256 w3 "") @0 dvc mib def dc >>
	(print @[w1] . take 10 =<< Vk.Mm.read @"hello" @(Obj.List 256 w1 "") @0 dvc mib def) >>
	(print @[w2] . take 10 =<< Vk.Mm.read @"hello" @(Obj.List 256 w2 "") @0 dvc mib def) >>
	(print @[w3] . take 10 =<< Vk.Mm.read @"hello" @(Obj.List 256 w3 "") @0 dvc mib def) >>
	Vk.DscPl.create dvc dscPlInfo nil \dscPool ->
	Vk.DscSt.allocateDs dvc (dscStInfo dscPool dscSetLyt)
		\(dscSet :** HPList.Nil) ->
	Vk.DscSt.updateDs dvc (HPList.Singleton
		. U5 $ writeDscSet' @w1 @w2 @w3 dscSet bufb) HPList.Nil >>
	f dscSet mib

imageInfo ::
	Word32 -> Word32 -> Vk.Img.Tiling -> Vk.Img.CreateInfo 'Nothing 'Vk.T.FormatR8g8b8a8Srgb
imageInfo wdt hgt tlng = Vk.Img.CreateInfo {
	Vk.Img.createInfoNext = TMaybe.N,
	Vk.Img.createInfoImageType = Vk.Img.Type2d,
	Vk.Img.createInfoExtent = Vk.Extent3d {
		Vk.extent3dWidth = wdt,
		Vk.extent3dHeight = hgt,
		Vk.extent3dDepth = 1 },
	Vk.Img.createInfoMipLevels = 1,
	Vk.Img.createInfoArrayLayers = 1,
	Vk.Img.createInfoTiling = tlng,
	Vk.Img.createInfoInitialLayout = Vk.Img.LayoutUndefined,
	Vk.Img.createInfoUsage = Vk.Img.UsageSampledBit,
	Vk.Img.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Img.createInfoSamples = Vk.Sample.Count1Bit,
	Vk.Img.createInfoFlags = zeroBits,
	Vk.Img.createInfoQueueFamilyIndices = [] }

arg3 :: (forall sm1 sm2 sm3 sb1 sb2 sb3 .
	Vk.Bffr.Binded sm1 sb1 nm1 '[Obj.List 256 w1 ""] ->
	Vk.Bffr.Binded sm2 sb2 nm2 '[Obj.List 256 w2 ""] ->
	Vk.Bffr.Binded sm3 sb3 nm3 '[Obj.List 256 w3 ""] ->
	Vk.Mm.M sm1 '[ '(sb1, 'Vk.Mm.BufferArg nm1 '[Obj.List 256 w1 ""])] ->
	Vk.Mm.M sm2 '[ '(sb2, 'Vk.Mm.BufferArg nm2 '[Obj.List 256 w2 ""])] ->
	Vk.Mm.M sm3 '[ '(sb3, 'Vk.Mm.BufferArg nm3 '[Obj.List 256 w3 ""])] -> r) ->
	Arg nm1 w1 (Arg nm2 w2 (Arg nm3 w3 r))
arg3 f = Arg \b1 m1 -> Arg \b2 m2 -> Arg \b3 m3 -> f b1 b2 b3 m1 m2 m3

storage3BufferNew :: forall sd w1 w2 w3 a . (
	Storable w1, Storable w2, Storable w3
	) =>
	Vk.Dvc.D sd -> Vk.Phd.P ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (
		forall sb1 sb2 sb3 sm .
		Vk.Bffr.Binded sm sb1 "buffer1" '[Obj.List 256 w1 ""] ->
		Vk.Bffr.Binded sm sb2 "buffer2" '[Obj.List 256 w2 ""] ->
		Vk.Bffr.Binded sm sb3 "buffer3" '[Obj.List 256 w3 ""] ->
		Vk.Mm.M sm '[
			'(sb1, 'Vk.Mm.BufferArg "buffer1" '[Obj.List 256 w1 ""]),
			'(sb2, 'Vk.Mm.BufferArg "buffer2" '[Obj.List 256 w2 ""]),
			'(sb3, 'Vk.Mm.BufferArg "buffer3" '[Obj.List 256 w3 ""]) ] -> IO a
		) -> IO a
storage3BufferNew dvc phdvc xs ys zs f =
	Vk.Bffr.create dvc (bufferInfo xs) nil \buf1 -> do
		memInfo1 <- getMemoryInfo phdvc dvc buf1
		Vk.Bffr.create dvc (bufferInfo ys) nil \buf2 -> do
			memInfo2 <- getMemoryInfo phdvc dvc buf2
			Vk.Bffr.create dvc (bufferInfo zs) nil \buf3 -> do
				memInfo3 <- getMemoryInfo phdvc dvc buf3
				if (memInfo1 == memInfo2 && memInfo2 == memInfo3) then
					Vk.Mm.allocateBind dvc (
						U2 (Vk.Mm.Buffer buf1) :**
						U2 (Vk.Mm.Buffer buf2) :**
						U2 (Vk.Mm.Buffer buf3) :** HPList.Nil
						) memInfo1 nil
						\(	U2 (Vk.Mm.BufferBinded bnd1) :**
							U2 (Vk.Mm.BufferBinded bnd2) :**
							U2 (Vk.Mm.BufferBinded bnd3) :** HPList.Nil ) mem -> do
						Vk.Mm.write @"buffer1" @(Obj.List 256 w1 "") @0 dvc mem def xs
						Vk.Mm.write @"buffer2" @(Obj.List 256 w2 "") @0 dvc mem def ys
						Vk.Mm.write @"buffer3" @(Obj.List 256 w3 "") @0 dvc mem def zs
						f bnd1 bnd2 bnd3 mem
					else error "bad"

bufferInfo :: Storable w => V.Vector w -> Vk.Bffr.CreateInfo 'Nothing '[Obj.List 256 w ""]
bufferInfo xs = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N,
	Vk.Bffr.createInfoFlags = def,
	Vk.Bffr.createInfoLengths =
		Obj.LengthList (fromIntegral $ V.length xs) :** HPList.Nil,
	Vk.Bffr.createInfoUsage = Vk.Bffr.UsageStorageBufferBit,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

storage1BufferNewNoBind :: forall sd nm w1 w2 w3 a . (
	Storable w1, Storable w2, Storable w3 ) =>
	Vk.Dvc.D sd ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (
		forall sb .
		Vk.Bffr.B sb nm '[Obj.List 256 w1 "",Obj.List 256 w2 "",Obj.List 256 w3 ""] -> IO a) -> IO a
storage1BufferNewNoBind dvc xs ys zs f =
	Vk.Bffr.create dvc (bufferInfo' xs ys zs) nil f

bufferInfo' :: (
	Storable w1, Storable w2, Storable w3 ) =>
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 ->
	Vk.Bffr.CreateInfo 'Nothing '[Obj.List 256 w1 "",Obj.List 256 w2 "",Obj.List 256 w3 ""]
bufferInfo' xs ys zs = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N,
	Vk.Bffr.createInfoFlags = def,
	Vk.Bffr.createInfoLengths =
		Obj.LengthList (fromIntegral $ V.length xs) :**
		Obj.LengthList (fromIntegral $ V.length ys) :**
		Obj.LengthList (fromIntegral $ V.length zs) :** HPList.Nil,
	Vk.Bffr.createInfoUsage = Vk.Bffr.UsageStorageBufferBit,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

dscStInfo :: Vk.DscPl.P sp -> Vk.DscStLyt.D sl bts ->
	Vk.DscSt.AllocateInfo 'Nothing sp '[ '(sl, bts)]
dscStInfo pl lyt = Vk.DscSt.AllocateInfo {
	Vk.DscSt.allocateInfoNext = TMaybe.N,
	Vk.DscSt.allocateInfoDescriptorPool = pl,
	Vk.DscSt.allocateInfoSetLayouts = U2 lyt :** HPList.Nil }

dscPlInfo :: Vk.DscPl.CreateInfo 'Nothing
dscPlInfo = Vk.DscPl.CreateInfo {
	Vk.DscPl.createInfoNext = TMaybe.N,
	Vk.DscPl.createInfoFlags = Vk.DscPl.CreateFreeDescriptorSetBit,
	Vk.DscPl.createInfoMaxSets = 1,
	Vk.DscPl.createInfoPoolSizes = [poolSize] }
	where poolSize = Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeStorageBuffer,
		Vk.DscPl.sizeDescriptorCount = 10 }

getMemoryInfo :: Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Bffr.B sb nm objs ->
	IO (Vk.Mm.AllocateInfo 'Nothing)
getMemoryInfo phdvc dvc buffer = do
	memTypeIdx <- findMemoryTypeIndex
		<$> ((: []) <$> Vk.Bffr.getMemoryRequirements dvc buffer)
		<*> pure memoryPropertyBits
		<*> Vk.Phd.getMemoryProperties phdvc
	pure Vk.Mm.AllocateInfo {
		Vk.Mm.allocateInfoNext = TMaybe.N,
		Vk.Mm.allocateInfoMemoryTypeIndex = memTypeIdx }

memoryPropertyBits :: Vk.Mm.PropertyFlagBits
memoryPropertyBits =
	Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit

findMemoryTypeIndex ::
	[Vk.Mm.M.Requirements] -> Vk.Mm.PropertyFlags ->
	Vk.Phd.MemoryProperties -> Vk.Mm.M.TypeIndex
findMemoryTypeIndex requirementss memoryProp memoryProperties = do
	let	reqTypess = Vk.Mm.M.requirementsMemoryTypeBits <$> requirementss
		memPropTypes = (fst <$>)
			. filter (checkBits memoryProp
				. Vk.Mm.M.mTypePropertyFlags . snd)
			$ Vk.Phd.memoryPropertiesMemoryTypes memoryProperties
	case filter (\x -> all (Vk.Mm.M.elemTypeIndex x) reqTypess) memPropTypes of
		[] -> error "No available memory types"
		i : _ -> i

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

writeDscSet' :: forall w1 w2 w3 slbts sb sm nm objs sds . (
	Show (HPList.PL Obj.Length objs),
	Obj.OffsetRange (Obj.List 256 w1 "") objs 0,
	Obj.OffsetRange (Obj.List 256 w2 "") objs 0,
	Obj.OffsetRange (Obj.List 256 w3 "") objs 0 ) =>
	Vk.DscSt.D sds slbts ->
	Vk.Bffr.Binded sm sb nm objs ->
	Vk.DscSt.Write 'Nothing sds slbts ('Vk.DscSt.WriteSourcesArgBuffer '[
		'(sm, sb, nm, Obj.List 256 w1 "", 0), '(sm, sb, nm, Obj.List 256 w2 "", 0),
		'(sm, sb, nm, Obj.List 256 w3 "", 0) ]) 0
writeDscSet' ds b = Vk.DscSt.Write {
	Vk.DscSt.writeNext = TMaybe.N,
	Vk.DscSt.writeDstSet = ds,
	Vk.DscSt.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSt.writeSources = Vk.DscSt.BufferInfos $
		U5 (bufferInfoList @w1 b) :** U5 (bufferInfoList @w2 b) :**
		U5 (bufferInfoList @w3 b) :** HPList.Nil }

bufferInfoList :: forall t {sb} {sm} {nm} {objs} . (
	Show (HPList.PL Obj.Length objs),
	Obj.OffsetRange (Obj.List 256 t "") objs 0 ) =>
	Vk.Bffr.Binded sm sb nm objs ->
	Vk.Dsc.BufferInfo sm sb nm (Obj.List 256 t "") 0
bufferInfoList = Vk.Dsc.BufferInfo

tilingToTiling :: Tiling -> Vk.Img.Tiling
tilingToTiling =
	\case Optimal -> Vk.Img.TilingOptimal; Linear -> Vk.Img.TilingLinear

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
