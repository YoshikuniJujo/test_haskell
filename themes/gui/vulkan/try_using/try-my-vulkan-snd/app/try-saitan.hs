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

import Foreign.Storable
import Data.Kind
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.Default
import Data.Bits
import Data.Bits.ToolsYj
import Data.Tuple.ToolsYj
import Data.List.Length
import Data.Vector.Storable qualified as V
import Data.HeteroParList qualified as HPList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Word
import System.Environment
import System.Console.GetOpt

import Language.SpirV.Shaderc.TH
import Language.SpirV.ShaderKind

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.Object qualified as Obj
import Gpu.Vulkan.Object.Base qualified as BObj
import Gpu.Vulkan.Instance qualified as Vk.Inst
import Gpu.Vulkan.PhysicalDevice qualified as Vk.Phd
import Gpu.Vulkan.Queue qualified as Vk.Q
import Gpu.Vulkan.QueueFamily qualified as Vk.QFam
import Gpu.Vulkan.Device qualified as Vk.Dvc
import Gpu.Vulkan.Memory qualified as Vk.Mm
import Gpu.Vulkan.Buffer qualified as Vk.Bffr
import Gpu.Vulkan.CommandPool qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer qualified as Vk.CBffr
import Gpu.Vulkan.Cmd qualified as Vk.Cmd

import Gpu.Vulkan.Pipeline qualified as Vk.Ppl
import Gpu.Vulkan.Pipeline.Compute qualified as Vk.Ppl.Cp
import Gpu.Vulkan.Pipeline.ShaderStage qualified as Vk.Ppl.ShaderSt
import Gpu.Vulkan.PipelineLayout qualified as Vk.PplLyt
import Gpu.Vulkan.PushConstant qualified as Vk.PshCnst
import Gpu.Vulkan.ShaderModule qualified as Vk.ShaderMod
import Gpu.Vulkan.Descriptor qualified as Vk.Dsc
import Gpu.Vulkan.DescriptorPool qualified as Vk.DscPl
import Gpu.Vulkan.DescriptorSet qualified as Vk.DscSt
import Gpu.Vulkan.DescriptorSetLayout qualified as Vk.DscStLyt

main :: IO ()
main = withOpts \opt -> do
	print opt
	rs <- realMain opt datA datB datC
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
	Storable w1, Storable w2, Storable w3,
	Obj.OffsetRange (OList w2) '[OList w1, OList w2, OList w3] 0,
	Obj.OffsetRange (OList w3) '[OList w1, OList w2, OList w3] 0,
	Obj.LengthOf (OList w2) '[OList w1, OList w2, OList w3],
	Obj.LengthOf (OList w3) '[OList w1, OList w2, OList w3] ) =>
	OptBffMm -> V.Vector w1 -> V.Vector w2 -> V.Vector w3 ->
	IO ([w1], [w2], [w3])
realMain opt da_ db_ dc_ = withDvc \pd d q cpl n ->
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
		Bffr1Mm1 -> createBffr1Mm1 pd d dsl da db dc \dss
				(m :: Mm sm '[ '(sb, 'Vk.Mm.BufferArg nm
					'[OList w1, OList w2, OList w3])]) ->
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
	'Nothing 'Nothing 'GlslComputeShader 'Nothing '[Word32, Word32]
shdrStInfo = Vk.Ppl.ShaderSt.CreateInfo {
	Vk.Ppl.ShaderSt.createInfoNext = TMaybe.N,
	Vk.Ppl.ShaderSt.createInfoFlags = zeroBits,
	Vk.Ppl.ShaderSt.createInfoStage = Vk.ShaderStageComputeBit,
	Vk.Ppl.ShaderSt.createInfoModule = (mdinfo, nil),
	Vk.Ppl.ShaderSt.createInfoName = "main",
	Vk.Ppl.ShaderSt.createInfoSpecializationInfo =
		Just $ HPList.Id 3 :** HPList.Id 10 :** HPList.Nil }
	where mdinfo = Vk.ShaderMod.CreateInfo {
		Vk.ShaderMod.createInfoNext = TMaybe.N,
		Vk.ShaderMod.createInfoFlags = zeroBits,
		Vk.ShaderMod.createInfoCode = glslComputeShaderMain }

run :: forall
	nm1 nm2 nm3 w1 w2 w3 sl bts sd scb scppl spl
	sds sm1 sm2 sm3 objss1 objss2 objss3 . (
	Storable w1, Storable w2, Storable w3,
	Vk.Mm.OffsetSize nm1 (Obj.List 256 w1 "") objss1 0,
	Vk.Mm.OffsetSize nm2 (Obj.List 256 w2 "") objss2 0,
	Vk.Mm.OffsetSize nm3 (Obj.List 256 w3 "") objss3 0,
--	Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[]],
	Vk.Cmd.LayoutArgListOnlyDynamics '[ '(sl, bts)] ~ '[ '[ '[]]] ) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CBffr.C scb ->
	Vk.Ppl.Cp.C scppl '(spl, '[ '(sl, bts)], '[]) ->
	Vk.PplLyt.P spl '[ '(sl, bts)] '[] -> Vk.DscSt.D sds '(sl, bts) ->
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
bffrMm pd dv xs a = Vk.Bffr.create dv (bffrInfoBffr3 xs) nil \b ->
	getMmInfo pd dv b >>= \minfo ->
	Vk.Mm.allocateBind dv (HPList.Singleton . U2 $ Vk.Mm.Buffer b) minfo nil
		\(U2 (Vk.Mm.BufferBinded bnd) :** HPList.Nil) mm ->
	Vk.Mm.write @nm @(Obj.List 256 w "") @0 dv mm def xs >> a bnd mm

arg3 :: (forall sm1 sm2 sm3 sb1 sb2 sb3 .
	Vk.Bffr.Binded sm1 sb1 nm1 '[OList w1] ->
	Vk.Bffr.Binded sm2 sb2 nm2 '[OList w2] ->
	Vk.Bffr.Binded sm3 sb3 nm3 '[OList w3] ->
	Vk.Mm.M sm1 '[ '(sb1, 'Vk.Mm.BufferArg nm1 '[OList w1])] ->
	Vk.Mm.M sm2 '[ '(sb2, 'Vk.Mm.BufferArg nm2 '[OList w2])] ->
	Vk.Mm.M sm3 '[ '(sb3, 'Vk.Mm.BufferArg nm3 '[OList w3])] -> r) ->
	Arg nm1 w1 (Arg nm2 w2 (Arg nm3 w3 r))
arg3 f = Arg \b1 m1 -> Arg \b2 m2 -> Arg \b3 m3 -> f b1 b2 b3 m1 m2 m3

-- BUFFER 3 : MEMORY 1

createBffr3Mm1 :: forall w1 w2 w3 sd sdsl bts a . (
	Storable w1, Storable w2, Storable w3,
	Vk.DscSt.BindingAndArrayElemBuffer
		bts '[OList w1, OList w2, OList w3] 0,
	Vk.DscSt.UpdateDynamicLength bts '[OList w1, OList w2, OList w3],
	Default (HPList.PL2 BObj.Length
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts)) ) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.DscStLyt.D sdsl bts ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (
		forall sds sm sb1 sb2 sb3 .
		Vk.DscSt.D sds '(sdsl, bts) ->
		Vk.Mm.M sm '[
			'(sb1, 'Vk.Mm.BufferArg "bffr1" '[OList w1]),
			'(sb2, 'Vk.Mm.BufferArg "bffr2" '[OList w2]),
			'(sb3, 'Vk.Mm.BufferArg "bffr3" '[OList w3]) ] ->
		IO a) -> IO a
createBffr3Mm1 pd dv dsl da db dc a =
	Vk.DscPl.create dv dscPlInfo nil \dsp ->
	Vk.DscSt.allocateDs dv (dscStInfo dsp dsl) \(HPList.Singleton dss) ->
	bffr3Mm1 dv pd da db dc \ba bb bc m ->
	Vk.DscSt.updateDs dv
		(HPList.Singleton
			. U5 $ writeDscStBffr3 @w1 @w2 @w3 dss ba bb bc)
		HPList.Nil >> a dss m

bffr3Mm1 :: forall sd w1 w2 w3 a . (Storable w1, Storable w2, Storable w3) =>
	Vk.Dvc.D sd -> Vk.Phd.P ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (forall sb1 sb2 sb3 sm .
		Vk.Bffr.Binded sm sb1 "bffr1" '[OList w1] ->
		Vk.Bffr.Binded sm sb2 "bffr2" '[OList w2] ->
		Vk.Bffr.Binded sm sb3 "bffr3" '[OList w3] ->
		Vk.Mm.M sm '[
			'(sb1, 'Vk.Mm.BufferArg "bffr1" '[OList w1]),
			'(sb2, 'Vk.Mm.BufferArg "bffr2" '[OList w2]),
			'(sb3, 'Vk.Mm.BufferArg "bffr3" '[OList w3]) ] ->
		IO a) -> IO a
bffr3Mm1 dv pd xs ys zs f = prepBffr3Mm1 @_ dv pd xs ys zs \b1 b2 b3 m -> do
	Vk.Mm.write @"bffr1" @(Obj.List 256 w1 "") @0 dv m def xs
	Vk.Mm.write @"bffr2" @(Obj.List 256 w2 "") @0 dv m def ys
	Vk.Mm.write @"bffr3" @(Obj.List 256 w3 "") @0 dv m def zs
	f b1 b2 b3 m

prepBffr3Mm1 :: forall sd w1 w2 w3 a .
	(Storable w1, Storable w2, Storable w3) =>
	Vk.Dvc.D sd -> Vk.Phd.P ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (forall sm sb1 sb2 sb3 .
		Vk.Bffr.Binded sm sb1 "bffr1" '[OList w1] ->
		Vk.Bffr.Binded sm sb2 "bffr2" '[OList w2] ->
		Vk.Bffr.Binded sm sb3 "bffr3" '[OList w3] ->
		Vk.Mm.M sm '[
			'(sb1, 'Vk.Mm.BufferArg "bffr1" '[OList w1]),
			'(sb2, 'Vk.Mm.BufferArg "bffr2" '[OList w2]),
			'(sb3, 'Vk.Mm.BufferArg "bffr3" '[OList w3]) ] ->
		IO a) -> IO a
prepBffr3Mm1 dv pd xs ys zs a =
	Vk.Bffr.create dv (bffrInfoBffr3 xs) nil \b1 ->
	Vk.Bffr.create dv (bffrInfoBffr3 ys) nil \b2 ->
	Vk.Bffr.create dv (bffrInfoBffr3 zs) nil \b3 -> do
	mi1 <- getMmInfo pd dv b1
	mi2 <- getMmInfo pd dv b2
	mi3 <- getMmInfo pd dv b3
	prepMm1ForBffr3 @"bffr1" @"bffr2" @"bffr3" dv b1 b2 b3 mi1 mi2 mi3 a

prepMm1ForBffr3 :: forall nm1 nm2 nm3 sb1 sb2 sb3 sd w1 w2 w3 a .
	(Storable w1, Storable w2, Storable w3) =>
	Vk.Dvc.D sd ->
	Vk.Bffr.B sb1 nm1 '[OList w1] ->
	Vk.Bffr.B sb2 nm2 '[OList w2] ->
	Vk.Bffr.B sb3 nm3 '[OList w3] ->
	Vk.Mm.AllocateInfo 'Nothing ->
	Vk.Mm.AllocateInfo 'Nothing ->
	Vk.Mm.AllocateInfo 'Nothing -> (forall sm .
		Vk.Bffr.Binded sm sb1 nm1 '[OList w1] ->
		Vk.Bffr.Binded sm sb2 nm2 '[OList w2] ->
		Vk.Bffr.Binded sm sb3 nm3 '[OList w3] ->
		Vk.Mm.M sm '[
			'(sb1, 'Vk.Mm.BufferArg nm1 '[OList w1]),
			'(sb2, 'Vk.Mm.BufferArg nm2 '[OList w2]),
			'(sb3, 'Vk.Mm.BufferArg nm3 '[OList w3]) ] ->
		IO a) -> IO a
prepMm1ForBffr3 dv b1 b2 b3 mi1 mi2 mi3 a = if (mi1 == mi2 && mi2 == mi3)
	then Vk.Mm.allocateBind dv (
		U2 (Vk.Mm.Buffer b1) :** U2 (Vk.Mm.Buffer b2) :**
		U2 (Vk.Mm.Buffer b3) :** HPList.Nil ) mi1 nil \(
			U2 (Vk.Mm.BufferBinded bnd1) :**
			U2 (Vk.Mm.BufferBinded bnd2) :**
			U2 (Vk.Mm.BufferBinded bnd3) :** HPList.Nil ) mem ->
		a bnd1 bnd2 bnd3 mem
	else error "bad"

-- BUFFER 1 : MEMORY 1

createBffr1Mm1 :: forall w1 w2 w3 sd sl bts nm a . (
	Storable w1, Storable w2, Storable w3,
	Obj.OffsetRange (OList w2) '[OList w1, OList w2, OList w3] 0,
	Obj.OffsetRange (OList w3) '[OList w1, OList w2, OList w3] 0,
	Obj.LengthOf (OList w2) '[OList w1, OList w2, OList w3],
	Obj.LengthOf (OList w3) '[OList w1, OList w2, OList w3],
	Vk.DscSt.BindingAndArrayElemBuffer
		bts '[OList w1, OList w2, OList w3] 0,
	Vk.DscSt.UpdateDynamicLength bts '[OList w1, OList w2, OList w3],
	Default (HPList.PL2 BObj.Length
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts)) ) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.DscStLyt.D sl bts ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (forall sds sm sb .
		Vk.DscSt.D sds '(sl, bts) ->
		Vk.Mm.M sm '[
			'(sb, 'Vk.Mm.BufferArg nm
				'[OList w1, OList w2, OList w3])] ->
		IO a) -> IO a
createBffr1Mm1 pd dv dsl da db dc a =
	Vk.DscPl.create dv dscPlInfo nil \dsp ->
	Vk.DscSt.allocateDs dv (dscStInfo dsp dsl) \(HPList.Singleton dss) ->
	bffr1Mm1 dv pd da db dc \b m ->
	Vk.DscSt.updateDs dv
		(HPList.Singleton . U5 $ writeDscStBffr1 @w1 @w2 @w3 dss b)
		HPList.Nil >> a dss m

bffr1Mm1 :: forall sd nm w1 w2 w3 a . (
	Storable w1, Storable w2, Storable w3,
	Obj.OffsetRange (OList w2) '[OList w1, OList w2, OList w3] 0,
	Obj.OffsetRange (OList w3) '[OList w1, OList w2, OList w3] 0,
	Obj.LengthOf (OList w2) '[OList w1, OList w2, OList w3],
	Obj.LengthOf (OList w3) '[OList w1, OList w2, OList w3] ) =>
	Vk.Dvc.D sd -> Vk.Phd.P ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[OList w1, OList w2, OList w3] ->
		Vk.Mm.M sm '[ '(sb,
			'Vk.Mm.BufferArg nm '[OList w1, OList w2, OList w3])] ->
		IO a) -> IO a
bffr1Mm1 dv pd xs ys zs a =
	Vk.Bffr.create dv (bffrInfoBffr1 xs ys zs) nil \buf ->
	getMmInfo pd dv buf >>= \mi ->
	Vk.Mm.allocateBind dv (HPList.Singleton . U2 $ Vk.Mm.Buffer buf)
		mi nil \(HPList.Singleton (U2 (Vk.Mm.BufferBinded bnd))) m -> do
		Vk.Mm.write @nm @(OList w1) @0 dv m def xs
		Vk.Mm.write @nm @(OList w2) @0 dv m def ys
		Vk.Mm.write @nm @(OList w3) @0 dv m def zs
		a bnd m

-- COMMON

dscPlInfo :: Vk.DscPl.CreateInfo 'Nothing
dscPlInfo = Vk.DscPl.CreateInfo {
	Vk.DscPl.createInfoNext = TMaybe.N,
	Vk.DscPl.createInfoFlags = Vk.DscPl.CreateFreeDescriptorSetBit,
	Vk.DscPl.createInfoMaxSets = 1,
	Vk.DscPl.createInfoPoolSizes = (: []) Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeStorageBuffer,
		Vk.DscPl.sizeDescriptorCount = 3 } }

dscStInfo :: Vk.DscPl.P sp -> Vk.DscStLyt.D sl bts ->
	Vk.DscSt.AllocateInfo 'Nothing sp '[ '(sl, bts)]
dscStInfo pl lyt = Vk.DscSt.AllocateInfo {
	Vk.DscSt.allocateInfoNext = TMaybe.N,
	Vk.DscSt.allocateInfoDescriptorPool = pl,
	Vk.DscSt.allocateInfoSetLayouts = HPList.Singleton $ U2 lyt }

bffrInfoBffr3 :: Storable w =>
	V.Vector w -> Vk.Bffr.CreateInfo 'Nothing '[OList w]
bffrInfoBffr3 xs = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N,
	Vk.Bffr.createInfoFlags = zeroBits,
	Vk.Bffr.createInfoLengths =
		HPList.Singleton . Obj.LengthList . fromIntegral $ V.length xs,
	Vk.Bffr.createInfoUsage = Vk.Bffr.UsageStorageBufferBit,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

bffrInfoBffr1 :: (Storable w1, Storable w2, Storable w3 ) =>
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 ->
	Vk.Bffr.CreateInfo 'Nothing '[OList w1, OList w2, OList w3]
bffrInfoBffr1 xs ys zs = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N,
	Vk.Bffr.createInfoFlags = zeroBits,
	Vk.Bffr.createInfoLengths =
		Obj.LengthList (fromIntegral $ V.length xs) :**
		Obj.LengthList (fromIntegral $ V.length ys) :**
		Obj.LengthList (fromIntegral $ V.length zs) :** HPList.Nil,
	Vk.Bffr.createInfoUsage = Vk.Bffr.UsageStorageBufferBit,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

getMmInfo :: Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Bffr.B sb nm objs -> IO (Vk.Mm.AllocateInfo 'Nothing)
getMmInfo pd dv b = do
	rqs <- Vk.Bffr.getMemoryRequirements dv b
	mti <- findMmTpIdx pd rqs
		(Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit)
	pure Vk.Mm.AllocateInfo {
		Vk.Mm.allocateInfoNext = TMaybe.N,
		Vk.Mm.allocateInfoMemoryTypeIndex = mti }

findMmTpIdx :: Vk.Phd.P ->
	Vk.Mm.Requirements -> Vk.Mm.PropertyFlags -> IO Vk.Mm.TypeIndex
findMmTpIdx pd rqs prs0 = do
	prs <- Vk.Phd.getMemoryProperties pd
	let	rqtps = Vk.Mm.requirementsMemoryTypeBits rqs
		prtps = (fst <$>) . filter (checkBits prs0
				. Vk.Mm.mTypePropertyFlags . snd)
			$ Vk.Phd.memoryPropertiesMemoryTypes prs
	case filter (`Vk.Mm.elemTypeIndex` rqtps) prtps of
		[] -> error "No available memory types"; i : _ -> pure i

writeDscStBffr3 :: forall w1 w2 w3
	sds slbts sm1 sm2 sm3 sb1 sb2 sb3 nm1 nm2 nm3 objs1 objs2 objs3 . (
	Show (HPList.PL Obj.Length objs1), Show (HPList.PL Obj.Length objs2),
	Show (HPList.PL Obj.Length objs3),
	Obj.OffsetRange (OList w1) objs1 0, Obj.OffsetRange (OList w2) objs2 0,
	Obj.OffsetRange (OList w3) objs3 0 ) =>
	Vk.DscSt.D sds slbts ->
	Vk.Bffr.Binded sm1 sb1 nm1 objs1 -> Vk.Bffr.Binded sm2 sb2 nm2 objs2 ->
	Vk.Bffr.Binded sm3 sb3 nm3 objs3 ->
	Vk.DscSt.Write 'Nothing sds slbts ('Vk.DscSt.WriteSourcesArgBuffer '[
		'(sm1, sb1, nm1, OList w1, 0), '(sm2, sb2, nm2, OList w2, 0),
		'(sm3, sb3, nm3, OList w3, 0) ]) 0
writeDscStBffr3 dss ba bb bc = Vk.DscSt.Write {
	Vk.DscSt.writeNext = TMaybe.N,
	Vk.DscSt.writeDstSet = dss,
	Vk.DscSt.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSt.writeSources = Vk.DscSt.BufferInfos $
		U5 (Vk.Dsc.BufferInfo ba) :** U5 (Vk.Dsc.BufferInfo bb) :**
		U5 (Vk.Dsc.BufferInfo bc) :** HPList.Nil }

writeDscStBffr1 :: forall w1 w2 w3 sds slbts sm sb nm objs . (
	Show (HPList.PL Obj.Length objs),
	Obj.OffsetRange (OList w1) objs 0, Obj.OffsetRange (OList w2) objs 0,
	Obj.OffsetRange (OList w3) objs 0 ) =>
	Vk.DscSt.D sds slbts ->
	Vk.Bffr.Binded sm sb nm objs ->
	Vk.DscSt.Write 'Nothing sds slbts ('Vk.DscSt.WriteSourcesArgBuffer '[
		'(sm, sb, nm, OList w1, 0), '(sm, sb, nm, OList w2, 0),
		'(sm, sb, nm, OList w3, 0) ]) 0
writeDscStBffr1 dss b = Vk.DscSt.Write {
	Vk.DscSt.writeNext = TMaybe.N,
	Vk.DscSt.writeDstSet = dss,
	Vk.DscSt.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSt.writeSources = Vk.DscSt.BufferInfos $
		U5 (Vk.Dsc.BufferInfo b) :** U5 (Vk.Dsc.BufferInfo b) :**
		U5 (Vk.Dsc.BufferInfo b) :** HPList.Nil }

-- WITH OPTIONS

withOpts :: (OptBffMm -> IO ()) -> IO ()
withOpts a = getArgs >>= \as ->
	let	(os, nos, ems) = getOpt RequireOrder opts as in
	case (ems, nos) of ([], []) -> a $ procOpts os; _ -> putOptErr ems nos

data OptBffMm = Bffr1Mm1 | Bffr3Mm1 | Bffr3Mm3 deriving Show

opts :: [OptDescr BffrMm]
opts = [
	Option ['b'] ["buffer"] (ReqArg
			(\case "1" -> Bffr1; "3" -> Bffr3; _ -> Nonsense)
			"Number of Buffers")
		"Set Number of Buffers",
	Option ['m'] ["memory"] (ReqArg
			(\case "1" -> Mm1; "3" -> Mm3; _ -> Nonsense)
			"Number of Memories")
		"Set Number of Memories" ]

data BffrMm = Bffr1 | Bffr3 | Mm1 | Mm3 | Nonsense deriving (Show, Eq, Ord)

procOpts :: [BffrMm] -> OptBffMm
procOpts os = case (b1, m1) of
	(False, False) -> Bffr3Mm3; (False, True) -> Bffr3Mm1;
	(True, True) -> Bffr1Mm1; _ -> Bffr3Mm3
	where b1 = Bffr1 `elem` os; m1 = Mm1 `elem` os

putOptErr :: [String] -> [String] -> IO ()
putOptErr ems nos =
	putStrLn `mapM_` ems >> putStrLn "Unsuitable args:" >>
	putStrLn `mapM_` nos

-- SHADER

[glslComputeShader|

#version 460

layout(local_size_x = 1, local_size_y = 1) in;
layout(binding = 0) buffer Data { uint val[]; } data[3];

layout(constant_id = 0) const uint sc = 2;
layout(constant_id = 1) const uint sc2 = 3;

void
main()
{
	int i = int(gl_GlobalInvocationID.x);
	data[2].val[i] = (data[0].val[i] + data[1].val[i]) * sc * sc2;
}

|]
