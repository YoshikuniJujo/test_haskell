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
import Gpu.Vulkan.Object.Base qualified as BObj
import Gpu.Vulkan.Object qualified as Obj
import Data.Default
import Data.Bits
import Data.Bits.ToolsYj
import Data.Tuple.ToolsYj
import Data.List.Length
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.List
import qualified Data.HeteroParList as HPList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Word
import System.Environment
import System.Console.GetOpt

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
import qualified Gpu.Vulkan.PipelineLayout as Vk.PplLyt
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShaderSt
import qualified Gpu.Vulkan.Pipeline.Compute as Vk.Ppl.Cp
import qualified Gpu.Vulkan.DescriptorSet as Vk.DscSet
import qualified Gpu.Vulkan.CommandBuffer as Vk.CBffr
import qualified Gpu.Vulkan.Cmd as Vk.Cmd

import qualified Gpu.Vulkan.Buffer as Vk.Buffer
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscStLyt

import qualified Gpu.Vulkan.PushConstant as Vk.PushConstant

main :: IO ()
main = withOptions \opt -> do
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
	Obj.LengthOf (OList w2) [OList w1, OList w2, OList w3],
	Obj.LengthOf (OList w3) [OList w1, OList w2, OList w3],
	Obj.OffsetRange (OList w2) [OList w1, OList w2, OList w3] 0,
	Obj.OffsetRange (OList w3) [OList w1, OList w2, OList w3] 0 ) =>
	OptBffMm -> V.Vector w1 -> V.Vector w2 -> V.Vector w3 ->
	IO ([w1], [w2], [w3])
realMain opt da db dc = withDvc \pd d q cpl n ->
	Vk.DscStLyt.create d dslinfo nil \dsl ->
	let	da' = V.take n da; db' = V.take n db; dc' = V.take n dc in
	case opt of
		Buffer3Memory3 -> prepBffr3Mm3 pd d dsl da' db' dc' \dss
				(ma :: Mm sm1 '[BffrArg sb1 nm1 '[OList w1]])
				(mb :: Mm sm2 '[BffrArg sb2 nm2 '[OList w2]])
				(mc :: Mm sm3 '[BffrArg sb3 nm3 '[OList w3]]) ->
			calc @nm1 @nm2 @nm3 d q cpl dsl dss n ma mb mc
		Buffer3Memory1 -> prepareMems' pd d dsl da' db' dc' \dss
				(m :: Mm sm '[
					BffrArg sb1 nm1 '[OList w1],
					BffrArg sb2 nm2 '[OList w2],
					BffrArg sb3 nm3 '[OList w3] ]) ->
			calc @nm1 @nm2 @nm3 d q cpl dsl dss n m m m
		Buffer1Memory1 -> prepareMems'' pd d dsl da' db' dc' \dss
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
	nm1 nm2 nm3 w1 w2 w3 slbts sl bts sd scpl
	sds sm1 sm2 sm3 objss1 objss2 objss3 . (
	Storable w1, Storable w2, Storable w3,
	Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[]],
	Show (HPList.PL2 BObj.Length
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts)),
	slbts ~ '(sl, bts), InfixIndex '[slbts] '[slbts],
	Vk.Mm.OffsetSize nm1 (OList w1) objss1 0,
	Vk.Mm.OffsetSize nm2 (OList w2) objss2 0,
	Vk.Mm.OffsetSize nm3 (OList w3) objss3 0 ) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C scpl -> Vk.DscStLyt.D sl bts ->
	Vk.DscSet.D sds slbts -> Word32 ->
	Mm sm1 objss1 -> Mm sm2 objss2 -> Mm sm3 objss3 -> IO ([w1], [w2], [w3])
calc dv q cpl dsl dss n ma mb mc =
	Vk.PplLyt.create dv plinfo nil \pl ->
	Vk.Ppl.Cp.createCs dv Nothing
		(U4 (cpplinfo pl) :** HPList.Nil) nil
		\(cppl :** HPList.Nil) ->
	Vk.CBffr.allocate dv cbinfo \(cb :*. HPList.Nil) ->
	run @nm1 @nm2 @nm3 dv q cb cppl pl dss n ma mb mc
	where
	plinfo :: Vk.PplLyt.CreateInfo
		'Nothing '[slbts] ('Vk.PushConstant.Layout '[] '[])
	plinfo = Vk.PplLyt.CreateInfo {
		Vk.PplLyt.createInfoNext = TMaybe.N,
		Vk.PplLyt.createInfoFlags = zeroBits,
		Vk.PplLyt.createInfoSetLayouts = HPList.Singleton $ U2 dsl }
	cbinfo :: Vk.CBffr.AllocateInfo 'Nothing scpl '[ '()]
	cbinfo = Vk.CBffr.AllocateInfo {
		Vk.CBffr.allocateInfoNext = TMaybe.N,
		Vk.CBffr.allocateInfoCommandPool = cpl,
		Vk.CBffr.allocateInfoLevel = Vk.CBffr.LevelPrimary }
	cpplinfo pl = Vk.Ppl.Cp.CreateInfo {
		Vk.Ppl.Cp.createInfoNext = TMaybe.N,
		Vk.Ppl.Cp.createInfoFlags = zeroBits,
		Vk.Ppl.Cp.createInfoStage = U5 shdrStInfo,
		Vk.Ppl.Cp.createInfoLayout = U3 pl,
		Vk.Ppl.Cp.createInfoBasePipelineHandleOrIndex = Nothing }

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
	nm1 nm2 nm3 w1 w2 w3 slbts sl bts sd scb scppl spl
	sds sm1 sm2 sm3 objss1 objss2 objss3 . (
	Storable w1, Storable w2, Storable w3,
	slbts ~ '(sl, bts),
	Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[]],
	Show (HPList.PL2 BObj.Length
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts)),
	InfixIndex '[slbts] '[slbts],
	Vk.Mm.OffsetSize nm1 (Obj.List 256 w1 "") objss1 0,
	Vk.Mm.OffsetSize nm2 (Obj.List 256 w2 "") objss2 0,
	Vk.Mm.OffsetSize nm3 (Obj.List 256 w3 "") objss3 0 ) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CBffr.C scb ->
	Vk.Ppl.Cp.C scppl '(spl, '[slbts], '[]) ->
	Vk.PplLyt.P spl '[slbts] '[] -> Vk.DscSet.D sds slbts -> Word32 ->
	Mm sm1 objss1 -> Mm sm2 objss2 -> Mm sm3 objss3 -> IO ([w1], [w2], [w3])
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
	(,,)	<$> Vk.Mm.read @nm1 @(Obj.List 256 w1 "") @0 @[w1] dv ma def
		<*> Vk.Mm.read @nm2 @(Obj.List 256 w2 "") @0 @[w2] dv mb def
		<*> Vk.Mm.read @nm3 @(Obj.List 256 w3 "") @0 @[w3] dv mc def
	where sinfo = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Nil }

-- BUFFER 3 : MEMORY 3

prepBffr3Mm3 :: forall bts w1 w2 w3 sd sl nm1 nm2 nm3 a . (
	Storable w1, Storable w2, Storable w3,
	Vk.DscSet.BindingAndArrayElemBuffer
		bts '[OList w1, OList w2, OList w3] 0,
	Vk.DscSet.UpdateDynamicLength bts '[OList w1, OList w2, OList w3],
	Default (HPList.PL2 BObj.Length
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts)) ) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.DscStLyt.D sl bts ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (
		forall sds sm1 sm2 sm3 sb1 sb2 sb3 .
		Vk.DscSet.D sds '(sl, bts) ->
		Vk.Mm.M sm1 '[ '( sb1, 'Vk.Mm.BufferArg nm1 '[OList w1])] ->
		Vk.Mm.M sm2 '[ '( sb2, 'Vk.Mm.BufferArg nm2 '[OList w2])] ->
		Vk.Mm.M sm3 '[ '( sb3, 'Vk.Mm.BufferArg nm3 '[OList w3])] ->
		IO a) -> IO a
prepBffr3Mm3 pd dv dsl da db dc a =
	Vk.DscPool.create dv dscPoolInfo nil \dsp ->
	Vk.DscSet.allocateDs dv (dscSetInfo dsp dsl) \(HPList.Singleton dss) ->
	storageBufferNew3' dv pd da db dc \ba ma bb mb bc mc ->
	Vk.DscSet.updateDs dv
		(HPList.Singleton . U5 $ writeDscSet @w1 @w2 @w3 dss ba bb bc)
		HPList.Nil >> a dss ma mb mc

storageBufferNew3' :: (Storable w1, Storable w2, Storable w3) =>
	Vk.Dvc.D sd -> Vk.Phd.P ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (
		forall sb1 sm1 sb2 sm2 sb3 sm3 .
		Vk.Buffer.Binded sm1 sb1 nm1 '[Obj.List 256 w1 ""] ->
		Vk.Mm.M sm1 '[ '(sb1, 'Vk.Mm.BufferArg nm1 '[Obj.List 256 w1 ""])] ->
		Vk.Buffer.Binded sm2 sb2 nm2 '[Obj.List 256 w2 ""] ->
		Vk.Mm.M sm2 '[ '(sb2, 'Vk.Mm.BufferArg nm2 '[Obj.List 256 w2 ""])] ->
		Vk.Buffer.Binded sm3 sb3 nm3 '[Obj.List 256 w3 ""] ->
		Vk.Mm.M sm3 '[ '(sb3, 'Vk.Mm.BufferArg nm3 '[Obj.List 256 w3 ""])] -> IO a ) -> IO a
storageBufferNew3' dvc phdvc x y z f =
	storageBufferNews dvc phdvc (x :** y :** z :** HPList.Nil) $ addArg3 f

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

addArg3 :: (forall sb1 sm1 sb2 sm2 sb3 sm3 .
	Vk.Buffer.Binded sm1 sb1 nm1 '[Obj.List 256 w1 ""] ->
	Vk.Mm.M sm1 '[ '(sb1, 'Vk.Mm.BufferArg nm1 '[Obj.List 256 w1 ""])] ->
	Vk.Buffer.Binded sm2 sb2 nm2 '[Obj.List 256 w2 ""] ->
	Vk.Mm.M sm2 '[ '(sb2, 'Vk.Mm.BufferArg nm2 '[Obj.List 256 w2 ""])] ->
	Vk.Buffer.Binded sm3 sb3 nm3 '[Obj.List 256 w3 ""] ->
	Vk.Mm.M sm3 '[ '(sb3, 'Vk.Mm.BufferArg nm3 '[Obj.List 256 w3 ""])] -> r) ->
	Arg nm1 w1 (Arg nm2 w2 (Arg nm3 w3 r))
addArg3 f = Arg \b1 m1 -> Arg \b2 m2 -> Arg \b3 m3 -> f b1 m1 b2 m2 b3 m3

-- BUFFER 3 : MEMORY 3

prepareMems' ::
	forall bts w1 w2 w3 sd sl a . (
	Default (HPList.PL
		(HPList.PL BObj.Length)
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts)),
	Storable w1, Storable w2, Storable w3,
	Vk.DscSet.BindingAndArrayElemBuffer bts '[Obj.List 256 w1 "",Obj.List 256 w2 "",Obj.List 256 w3 ""] 0,
	Vk.DscSet.UpdateDynamicLength bts '[Obj.List 256 w1 "",Obj.List 256 w2 "",Obj.List 256 w3 ""] ) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.DscStLyt.D sl bts ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (forall sds sm sb1 sb2 sb3 .
		Vk.DscSet.D sds '(sl, bts) ->
		Vk.Mm.M sm '[
			'(sb1, 'Vk.Mm.BufferArg "foo1" '[Obj.List 256 w1 ""]),
			'(sb2, 'Vk.Mm.BufferArg "foo2" '[Obj.List 256 w2 ""]),
			'(sb3, 'Vk.Mm.BufferArg "foo3" '[Obj.List 256 w3 ""])
			] -> IO a) -> IO a
prepareMems' phdvc dvc dscSetLyt da db dc f =
	Vk.DscPool.create dvc dscPoolInfo nil \dscPool ->
	Vk.DscSet.allocateDs dvc (dscSetInfo dscPool dscSetLyt)
		\(dscSet :** HPList.Nil) ->
	storage3BufferNew dvc phdvc da db dc \ba bb bc m ->
	Vk.DscSet.updateDs dvc (U5
		(writeDscSet @w1 @w2 @w3 dscSet ba bb bc) :** HPList.Nil) HPList.Nil >>
	f dscSet m

storage3BufferNew :: forall sd w1 w2 w3 a .
	(Storable w1, Storable w2, Storable w3) =>
	Vk.Dvc.D sd -> Vk.Phd.P ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (
		forall sb1 sb2 sb3 sm . (
		Vk.Mm.OffsetSize "foo3" (OList w3) '[
			BffrArg sb1 "foo1" '[OList w1],
			BffrArg sb2 "foo2" '[OList w2],
			BffrArg sb3 "foo3" '[OList w3] ] 0 ) =>
		Vk.Buffer.Binded sm sb1 "foo1" '[Obj.List 256 w1 ""] ->
		Vk.Buffer.Binded sm sb2 "foo2" '[Obj.List 256 w2 ""] ->
		Vk.Buffer.Binded sm sb3 "foo3" '[Obj.List 256 w3 ""] ->
		Vk.Mm.M sm '[
			'(sb1, 'Vk.Mm.BufferArg "foo1" '[Obj.List 256 w1 ""]),
			'(sb2, 'Vk.Mm.BufferArg "foo2" '[Obj.List 256 w2 ""]),
			'(sb3, 'Vk.Mm.BufferArg "foo3" '[Obj.List 256 w3 ""]) ] -> IO a
		) -> IO a
storage3BufferNew dvc phdvc xs ys zs f =
	storage3BufferNewGen @_ dvc phdvc xs ys zs \bnd1 bnd2 bnd3
			(mem :: Vk.Mm.M sm bs) -> do
		Vk.Mm.write @"foo1" @(Obj.List 256 w1 "") @0 dvc mem def xs
		Vk.Mm.write @"foo2" @(Obj.List 256 w2 "") @0 dvc mem def ys
		Vk.Mm.write @"foo3" @(Obj.List 256 w3 "") @0 @_ @_ @bs dvc mem def zs
		f bnd1 bnd2 bnd3 mem

storage3BufferNewGen :: forall sd w1 w2 w3 a . (
	Storable w1, Storable w2, Storable w3 ) =>
	Vk.Dvc.D sd -> Vk.Phd.P ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (
		forall sb1 sb2 sb3 sm . (
		Vk.Mm.OffsetSize "foo3" (OList w3) '[
			BffrArg sb1 "foo1" '[OList w1],
			BffrArg sb2 "foo2" '[OList w2],
			BffrArg sb3 "foo3" '[OList w3] ] 0 ) =>
		Vk.Buffer.Binded sm sb1 "foo1" '[Obj.List 256 w1 ""] ->
		Vk.Buffer.Binded sm sb2 "foo2" '[Obj.List 256 w2 ""] ->
		Vk.Buffer.Binded sm sb3 "foo3" '[Obj.List 256 w3 ""] ->
		Vk.Mm.M sm '[
			'(sb1, 'Vk.Mm.BufferArg "foo1" '[Obj.List 256 w1 ""]),
			'(sb2, 'Vk.Mm.BufferArg "foo2" '[Obj.List 256 w2 ""]),
			'(sb3, 'Vk.Mm.BufferArg "foo3" '[Obj.List 256 w3 ""]) ] -> IO a
		) -> IO a
storage3BufferNewGen dvc phdvc xs ys zs f =
	Vk.Buffer.create dvc (bufferInfo xs) nil \buf1 ->
	Vk.Buffer.create dvc (bufferInfo ys) nil \buf2 ->
	Vk.Buffer.create dvc (bufferInfo zs) nil \buf3 -> do
		memInfo1 <- getMemoryInfo phdvc dvc buf1
		memInfo2 <- getMemoryInfo phdvc dvc buf2
		memInfo3 <- getMemoryInfo phdvc dvc buf3
		runFoo @_ @_ @_ @"foo1" @"foo2" @"foo3" dvc buf1 buf2 buf3 memInfo1 memInfo2 memInfo3 f

runFoo :: forall sb1 sb2 sb3 nm1 nm2 nm3 sd w1 w2 w3 a . (
	Storable w1, Storable w2, Storable w3,
	Vk.Mm.OffsetSize nm3 (OList w3) [
		BffrArg sb1 nm1 '[OList w1],
		BffrArg sb2 nm2 '[OList w2],
		BffrArg sb3 nm3 '[OList w3] ] 0 ) =>
	Vk.Dvc.D sd ->
	Vk.Buffer.B sb1 nm1 '[OList w1] ->
	Vk.Buffer.B sb2 nm2 '[OList w2] ->
	Vk.Buffer.B sb3 nm3 '[OList w3] ->
	Vk.Mm.AllocateInfo 'Nothing ->
	Vk.Mm.AllocateInfo 'Nothing ->
	Vk.Mm.AllocateInfo 'Nothing -> (
		forall sm . (
		Vk.Mm.OffsetSize nm3 (OList w3) '[
			BffrArg sb1 nm1 '[OList w1],
			BffrArg sb2 nm2 '[OList w2],
			BffrArg sb3 nm3 '[OList w3] ] 0 ) =>
		Vk.Buffer.Binded sm sb1 nm1 '[Obj.List 256 w1 ""] ->
		Vk.Buffer.Binded sm sb2 nm2 '[Obj.List 256 w2 ""] ->
		Vk.Buffer.Binded sm sb3 nm3 '[Obj.List 256 w3 ""] ->
		Vk.Mm.M sm '[
			'(sb1, 'Vk.Mm.BufferArg nm1 '[Obj.List 256 w1 ""]),
			'(sb2, 'Vk.Mm.BufferArg nm2 '[Obj.List 256 w2 ""]),
			'(sb3, 'Vk.Mm.BufferArg nm3 '[Obj.List 256 w3 ""]) ] -> IO a
		) -> IO a
runFoo dvc buf1 buf2 buf3 memInfo1 memInfo2 memInfo3 f =
		if (memInfo1 == memInfo2 && memInfo2 == memInfo3) then
			Vk.Mm.allocateBind dvc (
				U2 (Vk.Mm.Buffer buf1) :**
				U2 (Vk.Mm.Buffer buf2) :**
				U2 (Vk.Mm.Buffer buf3) :** HPList.Nil
				) memInfo1 nil
				\(	U2 (Vk.Mm.BufferBinded (bnd1 :: Vk.Buffer.Binded sm sb1' nm1 '[OList w1])) :**
					U2 (Vk.Mm.BufferBinded (bnd2 :: Vk.Buffer.Binded sm sb2' nm2 '[OList w2])) :**
					U2 (Vk.Mm.BufferBinded (bnd3 :: Vk.Buffer.Binded sm sb3' nm3 '[OList w3])) :** HPList.Nil ) mem ->
				f bnd1 bnd2 bnd3 mem
			else error "bad"

-- BUFFER 1 : MEMORY 1

prepareMems'' :: forall w1 w2 w3 sd sl bts nm a . (
	Default (HPList.PL
		(HPList.PL BObj.Length)
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts)),
	Storable w1, Storable w2, Storable w3,
	Obj.OffsetRange (Obj.List 256 w2 "") '[Obj.List 256 w1 "",Obj.List 256 w2 "",Obj.List 256 w3 "" ] 0,
	Obj.OffsetRange (Obj.List 256 w3 "") '[Obj.List 256 w1 "",Obj.List 256 w2 "",Obj.List 256 w3 "" ] 0,
	Obj.LengthOf
		(Obj.List 256 w2 "") '[Obj.List 256 w1 "",Obj.List 256 w2 "",Obj.List 256 w3 ""],
	Obj.LengthOf
		(Obj.List 256 w3 "") '[Obj.List 256 w1 "",Obj.List 256 w2 "",Obj.List 256 w3 ""],
	Vk.DscSet.BindingAndArrayElemBuffer bts '[Obj.List 256 w1 "",Obj.List 256 w2 "",Obj.List 256 w3 ""] 0,
	Vk.DscSet.UpdateDynamicLength bts '[Obj.List 256 w1 "",Obj.List 256 w2 "",Obj.List 256 w3 ""] ) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.DscStLyt.D sl bts ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (forall sds sm sb .
		Vk.DscSet.D sds '(sl, bts) ->
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[Obj.List 256 w1 "",Obj.List 256 w2 "",Obj.List 256 w3 ""])] -> IO a) -> IO a
prepareMems'' phdvc dvc dscSetLyt da db dc f =
	Vk.DscPool.create dvc dscPoolInfo nil \dscPool ->
	Vk.DscSet.allocateDs dvc (dscSetInfo dscPool dscSetLyt)
		\(dscSet :** HPList.Nil) ->
	storage1BufferNew dvc phdvc da db dc \b m ->
	Vk.DscSet.updateDs dvc (U5
		(writeDscSet' @w1 @w2 @w3 dscSet b) :** HPList.Nil) HPList.Nil >>
	f dscSet m

storage1BufferNew :: forall sd nm w1 w2 w3 a . (
	Storable w1, Storable w2, Storable w3,
	Obj.OffsetRange (Obj.List 256 w2 "") '[Obj.List 256 w1 "",Obj.List 256 w2 "",Obj.List 256 w3 ""] 0,
	Obj.OffsetRange (Obj.List 256 w3 "") '[Obj.List 256 w1 "",Obj.List 256 w2 "",Obj.List 256 w3 ""] 0,
	Obj.LengthOf (Obj.List 256 w2 "") '[Obj.List 256 w1 "",Obj.List 256 w2 "",Obj.List 256 w3 ""],
	Obj.LengthOf (Obj.List 256 w3 "") '[Obj.List 256 w1 "",Obj.List 256 w2 "",Obj.List 256 w3 ""] ) =>
	Vk.Dvc.D sd -> Vk.Phd.P ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (
		forall sb sm .
		Vk.Buffer.Binded sm sb nm '[Obj.List 256 w1 "",Obj.List 256 w2 "",Obj.List 256 w3 ""] ->
		Vk.Mm.M sm
			'[ '(sb, 'Vk.Mm.BufferArg nm '[Obj.List 256 w1 "",Obj.List 256 w2 "",Obj.List 256 w3 ""])] -> IO a) -> IO a
storage1BufferNew dvc phdvc xs ys zs f =
	Vk.Buffer.create dvc (bufferInfo' xs ys zs) nil \buf -> do
		memInfo <- getMemoryInfo phdvc dvc buf
		Vk.Mm.allocateBind dvc (HPList.Singleton . U2 $ Vk.Mm.Buffer buf)
			memInfo nil \(HPList.Singleton (U2 (Vk.Mm.BufferBinded bnd))) mem -> do
			Vk.Mm.write @nm @(Obj.List 256 w1 "") @0 dvc mem def xs
			Vk.Mm.write @nm @(Obj.List 256 w2 "") @0 dvc mem def ys
			Vk.Mm.write @nm @(Obj.List 256 w3 "") @0 dvc mem def zs
			f bnd mem

-- COMMON

dscPoolInfo :: Vk.DscPool.CreateInfo 'Nothing
dscPoolInfo = Vk.DscPool.CreateInfo {
	Vk.DscPool.createInfoNext = TMaybe.N,
	Vk.DscPool.createInfoFlags = Vk.DscPool.CreateFreeDescriptorSetBit,
	Vk.DscPool.createInfoMaxSets = 1,
	Vk.DscPool.createInfoPoolSizes = [poolSize] }
	where poolSize = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeStorageBuffer,
		Vk.DscPool.sizeDescriptorCount = 10 }

dscSetInfo :: Vk.DscPool.P sp -> Vk.DscStLyt.D sl bts ->
	Vk.DscSet.AllocateInfo 'Nothing sp '[ '(sl, bts)]
dscSetInfo pl lyt = Vk.DscSet.AllocateInfo {
	Vk.DscSet.allocateInfoNext = TMaybe.N,
	Vk.DscSet.allocateInfoDescriptorPool = pl,
	Vk.DscSet.allocateInfoSetLayouts = U2 lyt :** HPList.Nil }

bufferInfo :: Storable w => V.Vector w -> Vk.Buffer.CreateInfo 'Nothing '[Obj.List 256 w ""]
bufferInfo xs = Vk.Buffer.CreateInfo {
	Vk.Buffer.createInfoNext = TMaybe.N,
	Vk.Buffer.createInfoFlags = def,
	Vk.Buffer.createInfoLengths =
		Obj.LengthList (fromIntegral $ V.length xs) :** HPList.Nil,
	Vk.Buffer.createInfoUsage = Vk.Buffer.UsageStorageBufferBit,
	Vk.Buffer.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Buffer.createInfoQueueFamilyIndices = [] }

bufferInfo' :: (
	Storable w1, Storable w2, Storable w3 ) =>
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 ->
	Vk.Buffer.CreateInfo 'Nothing '[Obj.List 256 w1 "",Obj.List 256 w2 "",Obj.List 256 w3 ""]
bufferInfo' xs ys zs = Vk.Buffer.CreateInfo {
	Vk.Buffer.createInfoNext = TMaybe.N,
	Vk.Buffer.createInfoFlags = def,
	Vk.Buffer.createInfoLengths =
		Obj.LengthList (fromIntegral $ V.length xs) :**
		Obj.LengthList (fromIntegral $ V.length ys) :**
		Obj.LengthList (fromIntegral $ V.length zs) :** HPList.Nil,
	Vk.Buffer.createInfoUsage = Vk.Buffer.UsageStorageBufferBit,
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

writeDscSet' :: forall w1 w2 w3 slbts sb sm nm objs sds . (
	Show (HPList.PL Obj.Length objs),
	Obj.OffsetRange (Obj.List 256 w1 "") objs 0,
	Obj.OffsetRange (Obj.List 256 w2 "") objs 0,
	Obj.OffsetRange (Obj.List 256 w3 "") objs 0 ) =>
	Vk.DscSet.D sds slbts ->
	Vk.Buffer.Binded sm sb nm objs ->
	Vk.DscSet.Write 'Nothing sds slbts ('Vk.DscSet.WriteSourcesArgBuffer '[
		'(sm, sb, nm, Obj.List 256 w1 "", 0), '(sm, sb, nm, Obj.List 256 w2 "", 0),
		'(sm, sb, nm, Obj.List 256 w3 "", 0) ]) 0
writeDscSet' ds b = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = ds,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		U5 (bufferInfoList @w1 b) :** U5 (bufferInfoList @w2 b) :**
		U5 (bufferInfoList @w3 b) :** HPList.Nil }

bufferInfoList :: forall t {sb} {sm} {nm} {objs} . (
	Show (HPList.PL Obj.Length objs),
	Obj.OffsetRange (Obj.List 256 t "") objs 0 ) =>
	Vk.Buffer.Binded sm sb nm objs ->
	Vk.Dsc.BufferInfo sm sb nm (Obj.List 256 t "") 0
bufferInfoList = Vk.Dsc.BufferInfo

-- WITH OPTIONS

withOptions :: (OptBffMm -> IO ()) -> IO ()
withOptions a = getArgs >>= \args ->
	let	(opts, noopts, emsgs) = getOpt RequireOrder options args in
	case (emsgs, noopts) of
		([], []) -> a $ processOptions opts
		_ -> putOptErr emsgs noopts

data OptBffMm = Buffer1Memory1 | Buffer3Memory1 | Buffer3Memory3 deriving Show

options :: [OptDescr Option]
options = [
	Option ['b'] ["buffer"] (ReqArg
			(\case "1" -> Buffer1; "3" -> Buffer3; _ -> Nonsense)
			"Number of Buffers")
		"Set Number of Buffers",
	Option ['m'] ["memory"] (ReqArg
			(\case "1" -> Memory1; "3" -> Buffer3; _ -> Nonsense)
			"Number of Memories")
		"Set Number of Memories" ]

data Option = Buffer1 | Buffer3 | Memory1 | Memory3 | Nonsense deriving (Show, Eq, Ord)

processOptions :: [Option] -> OptBffMm
processOptions opts = case (b1, m1) of
	(False, False) -> Buffer3Memory3
	(False, True) -> Buffer3Memory1
	(True, True) -> Buffer1Memory1
	_ -> Buffer3Memory3
	where b1 = Buffer1 `elem` opts; m1 = Memory1 `elem` opts

putOptErr :: [String] -> [String] -> IO ()
putOptErr emsgs noopts =
	putStrLn `mapM_` emsgs >> putStrLn "Unsuitable args:" >>
	putStrLn `mapM_` noopts

-- SHADER

[glslComputeShader|

#version 460
layout(local_size_x = 1, local_size_y = 1) in;
layout(binding = 0) buffer Data {
	uint val[];
} data[3];

layout(constant_id = 0) const uint sc = 2;
layout(constant_id = 1) const uint sc2 = 3;

void
main()
{
	int index = int(gl_GlobalInvocationID.x);
	data[2].val[index] = (data[0].val[index] + data[1].val[index]) * sc * sc2;
}

|]
