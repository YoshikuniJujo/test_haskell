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
import Gpu.Vulkan.Object.Base qualified as KObj
import Gpu.Vulkan.Object qualified as VObj
import Data.Default
import Data.Bits
import Data.Tuple.ToolsYj
import Data.List.Length
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.Index qualified as TIndex
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
import qualified Gpu.Vulkan.Memory as Vk.Mem
import qualified Gpu.Vulkan.Memory as Vk.Mem.M
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
	VObj.LengthOf (VObj.List 256 w2 "")
		'[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""],
	VObj.LengthOf (VObj.List 256 w3 "")
		'[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""],
	VObj.OffsetRange (VObj.List 256 w2 "") (ListBuffer1 w1 w2 w3) 0,
	VObj.OffsetRange (VObj.List 256 w3 "") (ListBuffer1 w1 w2 w3) 0 ) =>
	BufMem -> V.Vector w1 -> V.Vector w2 -> V.Vector w3 ->
	IO ([w1], [w2], [w3])
realMain opt da db dc = withDvc \pd d q cpl mgcx ->
	Vk.DscStLyt.create d (dscStLytInfo @w1 @w2 @w3) nil \dscSetLyt ->
	let	da' = V.take mgcx da
		db' = V.take mgcx db
		dc' = V.take mgcx dc in
	case opt of
		Buffer3Memory3 -> prepareMems pd d dscSetLyt da' db' dc' \dscSet
				(ma :: Vk.Mem.M sm1 '[ '( sb1, 'Vk.Mem.BufferArg nm1 '[VObj.List 256 w1 ""])])
				(mb :: Vk.Mem.M sm2 '[ '( sb2, 'Vk.Mem.BufferArg nm2 '[VObj.List 256 w2 ""])])
				(mc :: Vk.Mem.M sm3 '[ '( sb3, 'Vk.Mem.BufferArg nm3 '[VObj.List 256 w3 ""])]) ->
			calc @_ @_ @_ @nm1 @nm2 @nm3 d q cpl dscSetLyt dscSet mgcx ma mb mc
		Buffer3Memory1 ->
			prepareMems' pd d dscSetLyt da' db' dc' \dscSet m ->
			calc @_ @_ @_ @"buffer1" @"buffer2" @"buffer3" d q cpl dscSetLyt dscSet mgcx m m m
		Buffer1Memory1 ->
			prepareMems'' pd d dscSetLyt da' db' dc' \dscSet
				(m :: Vk.Mem.M sm '[ '(sb, 'Vk.Mem.BufferArg nm '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""])]) ->
			calc @_ @_ @_ @nm @nm @nm d q cpl dscSetLyt dscSet mgcx m m m

withDvc :: (forall sd scpl .
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C scpl ->
	(forall c . Integral c => c) -> IO a) -> IO a
withDvc a = Vk.Inst.create instInfo nil \inst -> do
	pd <- head <$> Vk.Phd.enumerate inst
	mgcx :. _ <- Vk.Phd.limitsMaxComputeWorkGroupCount
		. Vk.Phd.propertiesLimits <$> Vk.Phd.getProperties pd
	qfi <- fst . head . filter (
			checkBits Vk.Q.ComputeBit .
			Vk.QFam.propertiesQueueFlags . snd )
		<$> Vk.Phd.getQueueFamilyProperties pd
	Vk.Dvc.create pd (dvcInfo qfi) nil \dv ->
		Vk.Dvc.getQueue dv qfi 0 >>= \q ->
		Vk.CmdPl.create dv (cmdPlInfo qfi) nil \cpl ->
			a pd dv q cpl $ fromIntegral mgcx

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

calc :: forall w1 w2 w3 nm1 nm2 nm3 objss1 objss2 objss3 slbts sl bts sd scpl sm1 sm2 sm3 sds .
	(
	Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[]],
	slbts ~ '(sl, bts),
	Show (HPList.PL
		(HPList.PL KObj.Length)
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics (TIndex.I1_2 slbts))),
	Storable w1, Storable w2, Storable w3,
	Vk.Mem.OffsetSize nm1 (VObj.List 256 w1 "") objss1 0,
	Vk.Mem.OffsetSize nm2 (VObj.List 256 w2 "") objss2 0,
	Vk.Mem.OffsetSize nm3 (VObj.List 256 w3 "") objss3 0,
	InfixIndex '[slbts] '[ '(sl, bts)]) =>
	Vk.Dvc.D sd ->
	Vk.Q.Q -> Vk.CmdPl.C scpl ->
	Vk.DscStLyt.D sl bts ->
	Vk.DscSet.D sds slbts -> Word32 ->
	Vk.Mem.M sm1 objss1 -> Vk.Mem.M sm2 objss2 ->
	Vk.Mem.M sm3 objss3 -> IO ([w1], [w2], [w3])
calc dv q cpl dscSetLyt dscSet dsz ma mb mc =
	Vk.Ppl.Lyt.create dv (pplLayoutInfoNew dscSetLyt) nil \plyt ->
	Vk.Ppl.Cmpt.createCs
		dv Nothing (U4 (computePipelineInfo plyt) :** HPList.Nil)
		nil \(ppl :** HPList.Nil) ->
	Vk.CmdBuf.allocate dv (commandBufferInfo cpl) \(cmdBuf :*. HPList.Nil) ->
		run @nm1 @nm2 @nm3 dv q cmdBuf ppl plyt dscSet dsz ma mb mc

cmdPlInfo :: Vk.QFam.Index -> Vk.CmdPl.CreateInfo 'Nothing
cmdPlInfo qfi = Vk.CmdPl.CreateInfo {
	Vk.CmdPl.createInfoNext = TMaybe.N,
	Vk.CmdPl.createInfoFlags = Vk.CmdPl.CreateResetCommandBufferBit,
	Vk.CmdPl.createInfoQueueFamilyIndex = qfi }

type ListBuffer1 w1 w2 w3 = '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""]
type ListBuffer3Memory3 w1 w2 w3 = '[ '[VObj.List 256 w1 ""], '[VObj.List 256 w2 ""], '[VObj.List 256 w3 ""]]

run :: forall nm1 nm2 nm3 w1 w2 w3
	objss1 objss2 objss3 slbts sbtss sd sc sg sl sm1 sm2 sm3 sds . (
	Vk.DscStLyt.BindingTypeListBufferOnlyDynamics (TIndex.I1_2 slbts) ~ '[ '[]],
	sbtss ~ '[slbts],
	Show (HPList.PL
		(HPList.PL KObj.Length)
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics (TIndex.I1_2 slbts))),
	Storable w1, Storable w2, Storable w3,
	Vk.Mem.OffsetSize nm1 (VObj.List 256 w1 "") objss1 0,
	Vk.Mem.OffsetSize nm2 (VObj.List 256 w2 "") objss2 0,
	Vk.Mem.OffsetSize nm3 (VObj.List 256 w3 "") objss3 0,
	InfixIndex '[slbts] sbtss ) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdBuf.C sc ->
	Vk.Ppl.Cmpt.C sg '(sl, sbtss, '[]) ->
	Vk.Ppl.Lyt.P sl sbtss '[] -> Vk.DscSet.D sds slbts -> Word32 ->
	Vk.Mem.M sm1 objss1 -> Vk.Mem.M sm2 objss2 ->
	Vk.Mem.M sm3 objss3 -> IO ([w1], [w2], [w3])
run dvc q cmdBuf ppl pplLyt dscSet dsz memA memB memC = do
	Vk.CmdBuf.begin @'Nothing @'Nothing cmdBuf def $
		Vk.Cmd.bindPipelineCompute cmdBuf Vk.Ppl.BindPointCompute ppl \ccb -> do
			Vk.Cmd.bindDescriptorSetsCompute ccb pplLyt
				(U2 dscSet :** HPList.Nil)
				(HPList.Singleton
					$ HPList.Singleton HPList.Nil)
			Vk.Cmd.dispatch ccb dsz 1 1
	Vk.Q.submit q (U4 submitInfo :** HPList.Nil) Nothing
	Vk.Q.waitIdle q
	(,,)	<$> Vk.Mem.read @nm1 @(VObj.List 256 w1 "") @0 @[w1] dvc memA def
		<*> Vk.Mem.read @nm2 @(VObj.List 256 w2 "") @0 @[w2] dvc memB def
		<*> Vk.Mem.read @nm3 @(VObj.List 256 w3 "") @0 @[w3] dvc memC def
	where	submitInfo :: Vk.SubmitInfo 'Nothing _ _ _
		submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = TMaybe.N,
			Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
			Vk.submitInfoCommandBuffers = cmdBuf :** HPList.Nil,
			Vk.submitInfoSignalSemaphores = HPList.Nil }

dscStLytInfo :: Vk.DscStLyt.CreateInfo 'Nothing
	'[ 'Vk.DscStLyt.Buffer '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""]]
dscStLytInfo = Vk.DscStLyt.CreateInfo {
	Vk.DscStLyt.createInfoNext = TMaybe.N,
	Vk.DscStLyt.createInfoFlags = def,
	Vk.DscStLyt.createInfoBindings = binding0 :** HPList.Nil }

binding0 :: Vk.DscStLyt.Binding ('Vk.DscStLyt.Buffer objs)
binding0 = Vk.DscStLyt.BindingBuffer {
	Vk.DscStLyt.bindingBufferDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscStLyt.bindingBufferStageFlags = Vk.ShaderStageComputeBit }

prepareMems ::
	forall bts w1 w2 w3 sd sl nm1 nm2 nm3 a . (
	Default (HPList.PL
		(HPList.PL KObj.Length)
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts)),
	Storable w1, Storable w2, Storable w3,
	Vk.DscSet.BindingAndArrayElemBuffer bts '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""] 0,
	Vk.DscSet.UpdateDynamicLength bts '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""] ) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.DscStLyt.D sl bts ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (forall sds sm1 sb1 sm2 sb2 sm3 sb3 .
		Vk.DscSet.D sds '(sl, bts) ->
		Vk.Mem.M sm1 '[ '( sb1, 'Vk.Mem.BufferArg nm1 '[VObj.List 256 w1 ""])] ->
		Vk.Mem.M sm2 '[ '( sb2, 'Vk.Mem.BufferArg nm2 '[VObj.List 256 w2 ""])] ->
		Vk.Mem.M sm3 '[ '( sb3, 'Vk.Mem.BufferArg nm3 '[VObj.List 256 w3 ""])] -> IO a) -> IO a
prepareMems phdvc dvc dscSetLyt da db dc f =
	Vk.DscPool.create dvc dscPoolInfo nil \dscPool ->
	Vk.DscSet.allocateDs dvc (dscSetInfo dscPool dscSetLyt)
		\(dscSet :** HPList.Nil) ->
	storageBufferNew3' dvc phdvc da db dc \ba ma bb mb bc mc ->
	Vk.DscSet.updateDs dvc (U5
		(writeDscSet @w1 @w2 @w3 dscSet ba bb bc) :** HPList.Nil) HPList.Nil >>
	f dscSet ma mb mc

prepareMems' ::
	forall bts w1 w2 w3 sd sl a . (
	Default (HPList.PL
		(HPList.PL KObj.Length)
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts)),
	Storable w1, Storable w2, Storable w3,
	Vk.DscSet.BindingAndArrayElemBuffer bts '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""] 0,
	Vk.DscSet.UpdateDynamicLength bts '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""] ) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.DscStLyt.D sl bts ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (forall sds sm sb1 sb2 sb3 .
		Vk.DscSet.D sds '(sl, bts) ->
		Vk.Mem.M sm '[
			'(sb1, 'Vk.Mem.BufferArg "buffer1" '[VObj.List 256 w1 ""]),
			'(sb2, 'Vk.Mem.BufferArg "buffer2" '[VObj.List 256 w2 ""]),
			'(sb3, 'Vk.Mem.BufferArg "buffer3" '[VObj.List 256 w3 ""])
			] -> IO a) -> IO a
prepareMems' phdvc dvc dscSetLyt da db dc f =
	Vk.DscPool.create dvc dscPoolInfo nil \dscPool ->
	Vk.DscSet.allocateDs dvc (dscSetInfo dscPool dscSetLyt)
		\(dscSet :** HPList.Nil) ->
	storage3BufferNew dvc phdvc da db dc \ba bb bc m ->
	Vk.DscSet.updateDs dvc (U5
		(writeDscSet @w1 @w2 @w3 dscSet ba bb bc) :** HPList.Nil) HPList.Nil >>
	f dscSet m

prepareMems'' :: forall w1 w2 w3 sd sl bts nm a . (
	Default (HPList.PL
		(HPList.PL KObj.Length)
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts)),
	Storable w1, Storable w2, Storable w3,
	VObj.OffsetRange (VObj.List 256 w2 "") '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 "" ] 0,
	VObj.OffsetRange (VObj.List 256 w3 "") '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 "" ] 0,
	VObj.LengthOf
		(VObj.List 256 w2 "") '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""],
	VObj.LengthOf
		(VObj.List 256 w3 "") '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""],
	Vk.DscSet.BindingAndArrayElemBuffer bts '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""] 0,
	Vk.DscSet.UpdateDynamicLength bts '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""] ) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.DscStLyt.D sl bts ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (forall sds sm sb .
		Vk.DscSet.D sds '(sl, bts) ->
		Vk.Mem.M sm '[ '(sb, 'Vk.Mem.BufferArg nm '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""])] -> IO a) -> IO a
prepareMems'' phdvc dvc dscSetLyt da db dc f =
	Vk.DscPool.create dvc dscPoolInfo nil \dscPool ->
	Vk.DscSet.allocateDs dvc (dscSetInfo dscPool dscSetLyt)
		\(dscSet :** HPList.Nil) ->
	storage1BufferNew dvc phdvc da db dc \b m ->
	Vk.DscSet.updateDs dvc (U5
		(writeDscSet' @w1 @w2 @w3 dscSet b) :** HPList.Nil) HPList.Nil >>
	f dscSet m

storageBufferNew3' :: (Storable w1, Storable w2, Storable w3) =>
	Vk.Dvc.D sd -> Vk.Phd.P ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (
		forall sb1 sm1 sb2 sm2 sb3 sm3 .
		Vk.Buffer.Binded sm1 sb1 nm1 '[VObj.List 256 w1 ""] ->
		Vk.Mem.M sm1 '[ '(sb1, 'Vk.Mem.BufferArg nm1 '[VObj.List 256 w1 ""])] ->
		Vk.Buffer.Binded sm2 sb2 nm2 '[VObj.List 256 w2 ""] ->
		Vk.Mem.M sm2 '[ '(sb2, 'Vk.Mem.BufferArg nm2 '[VObj.List 256 w2 ""])] ->
		Vk.Buffer.Binded sm3 sb3 nm3 '[VObj.List 256 w3 ""] ->
		Vk.Mem.M sm3 '[ '(sb3, 'Vk.Mem.BufferArg nm3 '[VObj.List 256 w3 ""])] -> IO a ) -> IO a
storageBufferNew3' dvc phdvc x y z f =
	storageBufferNews dvc phdvc (x :** y :** z :** HPList.Nil) $ addArg3 f

addArg3 :: (forall sb1 sm1 sb2 sm2 sb3 sm3 .
	Vk.Buffer.Binded sm1 sb1 nm1 '[VObj.List 256 w1 ""] ->
	Vk.Mem.M sm1 '[ '(sb1, 'Vk.Mem.BufferArg nm1 '[VObj.List 256 w1 ""])] ->
	Vk.Buffer.Binded sm2 sb2 nm2 '[VObj.List 256 w2 ""] ->
	Vk.Mem.M sm2 '[ '(sb2, 'Vk.Mem.BufferArg nm2 '[VObj.List 256 w2 ""])] ->
	Vk.Buffer.Binded sm3 sb3 nm3 '[VObj.List 256 w3 ""] ->
	Vk.Mem.M sm3 '[ '(sb3, 'Vk.Mem.BufferArg nm3 '[VObj.List 256 w3 ""])] -> r) ->
	Arg nm1 w1 (Arg nm2 w2 (Arg nm3 w3 r))
addArg3 f = Arg \b1 m1 -> Arg \b2 m2 -> Arg \b3 m3 -> f b1 m1 b2 m2 b3 m3

class StorageBufferNews f a where
	type Vectors f :: [Type]
	storageBufferNews :: Vk.Dvc.D sd -> Vk.Phd.P ->
		HPList.PL V.Vector (Vectors f) -> f -> IO a

data Arg nm w f = Arg (forall sb sm .
	Vk.Buffer.Binded sm sb nm '[VObj.List 256 w ""] ->
	Vk.Mem.M sm '[ '(sb, 'Vk.Mem.BufferArg nm '[VObj.List 256 w ""])] -> f)

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
		Vk.Buffer.Binded sm sb nm '[VObj.List 256 w ""]  ->
		Vk.Mem.M sm '[ '(sb, 'Vk.Mem.BufferArg nm '[VObj.List 256 w ""])] -> IO a ) -> IO a
storageBufferNew dvc phdvc xs f =
	Vk.Buffer.create dvc (bufferInfo xs) nil \buffer -> do
		memoryInfo <- getMemoryInfo phdvc dvc buffer
		Vk.Mem.allocateBind dvc (U2 (Vk.Mem.Buffer buffer) :** HPList.Nil) memoryInfo
			nil \(U2 (Vk.Mem.BufferBinded binded) :** HPList.Nil) memory -> do
			Vk.Mem.write @nm @(VObj.List 256 w "") @0 dvc memory def xs
			f binded memory

storage3BufferNew :: forall sd w1 w2 w3 a . (
	Storable w1, Storable w2, Storable w3
	) =>
	Vk.Dvc.D sd -> Vk.Phd.P ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (
		forall sb1 sb2 sb3 sm .
		Vk.Buffer.Binded sm sb1 "buffer1" '[VObj.List 256 w1 ""] ->
		Vk.Buffer.Binded sm sb2 "buffer2" '[VObj.List 256 w2 ""] ->
		Vk.Buffer.Binded sm sb3 "buffer3" '[VObj.List 256 w3 ""] ->
		Vk.Mem.M sm '[
			'(sb1, 'Vk.Mem.BufferArg "buffer1" '[VObj.List 256 w1 ""]),
			'(sb2, 'Vk.Mem.BufferArg "buffer2" '[VObj.List 256 w2 ""]),
			'(sb3, 'Vk.Mem.BufferArg "buffer3" '[VObj.List 256 w3 ""]) ] -> IO a
		) -> IO a
storage3BufferNew dvc phdvc xs ys zs f =
	storage3BufferNewGen dvc phdvc xs ys zs \bnd1 bnd2 bnd3 mem -> do
		Vk.Mem.write @"buffer1" @(VObj.List 256 w1 "") @0 dvc mem def xs
		Vk.Mem.write @"buffer2" @(VObj.List 256 w2 "") @0 dvc mem def ys
		Vk.Mem.write @"buffer3" @(VObj.List 256 w3 "") @0 dvc mem def zs
		f bnd1 bnd2 bnd3 mem

storage3BufferNewGen :: forall sd w1 w2 w3 a . (
	Storable w1, Storable w2, Storable w3
	) =>
	Vk.Dvc.D sd -> Vk.Phd.P ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (
		forall sb1 sb2 sb3 sm .
		Vk.Buffer.Binded sm sb1 "buffer1" '[VObj.List 256 w1 ""] ->
		Vk.Buffer.Binded sm sb2 "buffer2" '[VObj.List 256 w2 ""] ->
		Vk.Buffer.Binded sm sb3 "buffer3" '[VObj.List 256 w3 ""] ->
		Vk.Mem.M sm '[
			'(sb1, 'Vk.Mem.BufferArg "buffer1" '[VObj.List 256 w1 ""]),
			'(sb2, 'Vk.Mem.BufferArg "buffer2" '[VObj.List 256 w2 ""]),
			'(sb3, 'Vk.Mem.BufferArg "buffer3" '[VObj.List 256 w3 ""]) ] -> IO a
		) -> IO a
storage3BufferNewGen dvc phdvc xs ys zs f =
	Vk.Buffer.create dvc (bufferInfo xs) nil \buf1 -> do
		memInfo1 <- getMemoryInfo phdvc dvc buf1
		Vk.Buffer.create dvc (bufferInfo ys) nil \buf2 -> do
			memInfo2 <- getMemoryInfo phdvc dvc buf2
			Vk.Buffer.create dvc (bufferInfo zs) nil \buf3 -> do
				memInfo3 <- getMemoryInfo phdvc dvc buf3
				if (memInfo1 == memInfo2 && memInfo2 == memInfo3) then
					Vk.Mem.allocateBind dvc (
						U2 (Vk.Mem.Buffer buf1) :**
						U2 (Vk.Mem.Buffer buf2) :**
						U2 (Vk.Mem.Buffer buf3) :** HPList.Nil
						) memInfo1 nil
						\(	U2 (Vk.Mem.BufferBinded bnd1) :**
							U2 (Vk.Mem.BufferBinded bnd2) :**
							U2 (Vk.Mem.BufferBinded bnd3) :** HPList.Nil ) mem ->
						f bnd1 bnd2 bnd3 mem
					else error "bad"

bufferInfo :: Storable w => V.Vector w -> Vk.Buffer.CreateInfo 'Nothing '[VObj.List 256 w ""]
bufferInfo xs = Vk.Buffer.CreateInfo {
	Vk.Buffer.createInfoNext = TMaybe.N,
	Vk.Buffer.createInfoFlags = def,
	Vk.Buffer.createInfoLengths =
		VObj.LengthList (fromIntegral $ V.length xs) :** HPList.Nil,
	Vk.Buffer.createInfoUsage = Vk.Buffer.UsageStorageBufferBit,
	Vk.Buffer.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Buffer.createInfoQueueFamilyIndices = [] }

storage1BufferNew :: forall sd nm w1 w2 w3 a . (
	Storable w1, Storable w2, Storable w3,
	VObj.OffsetRange (VObj.List 256 w2 "") '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""] 0,
	VObj.OffsetRange (VObj.List 256 w3 "") '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""] 0,
	VObj.LengthOf (VObj.List 256 w2 "") '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""],
	VObj.LengthOf (VObj.List 256 w3 "") '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""] ) =>
	Vk.Dvc.D sd -> Vk.Phd.P ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (
		forall sb sm . -- (
		Vk.Buffer.Binded sm sb nm '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""] ->
		Vk.Mem.M sm
			'[ '(sb, 'Vk.Mem.BufferArg nm '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""])] -> IO a) -> IO a
storage1BufferNew dvc phdvc xs ys zs f =
	Vk.Buffer.create dvc (bufferInfo' xs ys zs) nil \buf -> do
		memInfo <- getMemoryInfo phdvc dvc buf
		Vk.Mem.allocateBind dvc (HPList.Singleton . U2 $ Vk.Mem.Buffer buf)
			memInfo nil \(HPList.Singleton (U2 (Vk.Mem.BufferBinded bnd))) mem -> do
			Vk.Mem.write @nm @(VObj.List 256 w1 "") @0 dvc mem def xs
			Vk.Mem.write @nm @(VObj.List 256 w2 "") @0 dvc mem def ys
			Vk.Mem.write @nm @(VObj.List 256 w3 "") @0 dvc mem def zs
			f bnd mem

bufferInfo' :: (
	Storable w1, Storable w2, Storable w3 ) =>
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 ->
	Vk.Buffer.CreateInfo 'Nothing '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""]
bufferInfo' xs ys zs = Vk.Buffer.CreateInfo {
	Vk.Buffer.createInfoNext = TMaybe.N,
	Vk.Buffer.createInfoFlags = def,
	Vk.Buffer.createInfoLengths =
		VObj.LengthList (fromIntegral $ V.length xs) :**
		VObj.LengthList (fromIntegral $ V.length ys) :**
		VObj.LengthList (fromIntegral $ V.length zs) :** HPList.Nil,
	Vk.Buffer.createInfoUsage = Vk.Buffer.UsageStorageBufferBit,
	Vk.Buffer.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Buffer.createInfoQueueFamilyIndices = [] }

pplLayoutInfoNew :: Vk.DscStLyt.D sl bts -> Vk.Ppl.Lyt.CreateInfo 'Nothing '[ '(sl, bts)]
	('Vk.PushConstant.Layout '[] '[])
pplLayoutInfoNew dsl = Vk.Ppl.Lyt.CreateInfo {
	Vk.Ppl.Lyt.createInfoNext = TMaybe.N,
	Vk.Ppl.Lyt.createInfoFlags = def,
	Vk.Ppl.Lyt.createInfoSetLayouts =
		U2 dsl :** HPList.Nil }

computePipelineInfo :: Vk.Ppl.Lyt.P sl sbtss '[] ->
	Vk.Ppl.Cmpt.CreateInfo 'Nothing
		'( 'Nothing, 'Nothing, 'GlslComputeShader, 'Nothing, '[Word32, Word32])
		'(sl, sbtss, '[]) sbph
computePipelineInfo pl = Vk.Ppl.Cmpt.CreateInfo {
	Vk.Ppl.Cmpt.createInfoNext = TMaybe.N,
	Vk.Ppl.Cmpt.createInfoFlags = def,
	Vk.Ppl.Cmpt.createInfoStage = U5 shaderStageInfo,
	Vk.Ppl.Cmpt.createInfoLayout = U3 pl,
	Vk.Ppl.Cmpt.createInfoBasePipelineHandleOrIndex = Nothing }

dscSetInfo :: Vk.DscPool.P sp -> Vk.DscStLyt.D sl bts ->
	Vk.DscSet.AllocateInfo 'Nothing sp '[ '(sl, bts)]
dscSetInfo pl lyt = Vk.DscSet.AllocateInfo {
	Vk.DscSet.allocateInfoNext = TMaybe.N,
	Vk.DscSet.allocateInfoDescriptorPool = pl,
	Vk.DscSet.allocateInfoSetLayouts = U2 lyt :** HPList.Nil }

commandBufferInfo :: Vk.CmdPl.C s -> Vk.CmdBuf.AllocateInfo 'Nothing s '[ '()]
commandBufferInfo cmdPool = Vk.CmdBuf.AllocateInfo {
	Vk.CmdBuf.allocateInfoNext = TMaybe.N,
	Vk.CmdBuf.allocateInfoCommandPool = cmdPool,
	Vk.CmdBuf.allocateInfoLevel = Vk.CmdBuf.LevelPrimary }

dscPoolInfo :: Vk.DscPool.CreateInfo 'Nothing
dscPoolInfo = Vk.DscPool.CreateInfo {
	Vk.DscPool.createInfoNext = TMaybe.N,
	Vk.DscPool.createInfoFlags = Vk.DscPool.CreateFreeDescriptorSetBit,
	Vk.DscPool.createInfoMaxSets = 1,
	Vk.DscPool.createInfoPoolSizes = [poolSize] }
	where poolSize = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeStorageBuffer,
		Vk.DscPool.sizeDescriptorCount = 10 }

getMemoryInfo :: Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Buffer.B sb nm objs ->
	IO (Vk.Mem.AllocateInfo 'Nothing)
getMemoryInfo phdvc dvc buffer = do
	requirements <- Vk.Buffer.getMemoryRequirements dvc buffer
	memTypeIdx <- findMemoryTypeIndex phdvc requirements (
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

writeDscSet ::
	forall w1 w2 w3 slbts sb1 sb2 sb3 sm1 sm2 sm3 nm1 nm2 nm3 objs1 objs2 objs3 sds . (
	Show (HPList.PL VObj.Length objs1),
	Show (HPList.PL VObj.Length objs2),
	Show (HPList.PL VObj.Length objs3),
	VObj.OffsetRange (VObj.List 256 w1 "") objs1 0,
	VObj.OffsetRange (VObj.List 256 w2 "") objs2 0,
	VObj.OffsetRange (VObj.List 256 w3 "") objs3 0 ) =>
	Vk.DscSet.D sds slbts ->
	Vk.Buffer.Binded sm1 sb1 nm1 objs1 -> Vk.Buffer.Binded sm2 sb2 nm2 objs2 ->
	Vk.Buffer.Binded sm3 sb3 nm3 objs3 ->
	Vk.DscSet.Write 'Nothing sds slbts ('Vk.DscSet.WriteSourcesArgBuffer '[
		'(sm1, sb1, nm1, VObj.List 256 w1 "", 0), '(sm2, sb2, nm2, VObj.List 256 w2 "", 0),
		'(sm3, sb3, nm3, VObj.List 256 w3 "", 0) ]) 0
writeDscSet ds ba bb bc = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = ds,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		U5 (bufferInfoList @w1 ba) :** U5 (bufferInfoList @w2 bb) :**
		U5 (bufferInfoList @w3 bc) :** HPList.Nil }

writeDscSet' :: forall w1 w2 w3 slbts sb sm nm objs sds . (
	Show (HPList.PL VObj.Length objs),
	VObj.OffsetRange (VObj.List 256 w1 "") objs 0,
	VObj.OffsetRange (VObj.List 256 w2 "") objs 0,
	VObj.OffsetRange (VObj.List 256 w3 "") objs 0 ) =>
	Vk.DscSet.D sds slbts ->
	Vk.Buffer.Binded sm sb nm objs ->
	Vk.DscSet.Write 'Nothing sds slbts ('Vk.DscSet.WriteSourcesArgBuffer '[
		'(sm, sb, nm, VObj.List 256 w1 "", 0), '(sm, sb, nm, VObj.List 256 w2 "", 0),
		'(sm, sb, nm, VObj.List 256 w3 "", 0) ]) 0
writeDscSet' ds b = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = ds,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		U5 (bufferInfoList @w1 b) :** U5 (bufferInfoList @w2 b) :**
		U5 (bufferInfoList @w3 b) :** HPList.Nil }

bufferInfoList :: forall t {sb} {sm} {nm} {objs} . (
	Show (HPList.PL VObj.Length objs),
	VObj.OffsetRange (VObj.List 256 t "") objs 0 ) =>
	Vk.Buffer.Binded sm sb nm objs ->
	Vk.Dsc.BufferInfo sm sb nm (VObj.List 256 t "") 0
bufferInfoList = Vk.Dsc.BufferInfo

shaderStageInfo :: Vk.Ppl.ShaderSt.CreateInfo
	'Nothing 'Nothing 'GlslComputeShader 'Nothing '[Word32, Word32]
shaderStageInfo = Vk.Ppl.ShaderSt.CreateInfo {
	Vk.Ppl.ShaderSt.createInfoNext = TMaybe.N,
	Vk.Ppl.ShaderSt.createInfoFlags = def,
	Vk.Ppl.ShaderSt.createInfoStage = Vk.ShaderStageComputeBit,
	Vk.Ppl.ShaderSt.createInfoModule = (shaderModInfo, nil),
	Vk.Ppl.ShaderSt.createInfoName = "main",
	Vk.Ppl.ShaderSt.createInfoSpecializationInfo = Just $ HPList.Id 3 :** HPList.Id 10 :** HPList.Nil }
	where shaderModInfo = Vk.ShaderMod.CreateInfo {
		Vk.ShaderMod.createInfoNext = TMaybe.N,
		Vk.ShaderMod.createInfoFlags = def,
		Vk.ShaderMod.createInfoCode = glslComputeShaderMain }

-- WITH OPTIONS

withOptions :: (BufMem -> IO ()) -> IO ()
withOptions a = getArgs >>= \args ->
	let	(opts, noopts, emsgs) = getOpt RequireOrder options args in
	case (emsgs, noopts) of
		([], []) -> a $ processOptions opts
		_ -> putOptErr emsgs noopts

data BufMem = Buffer1Memory1 | Buffer3Memory1 | Buffer3Memory3 deriving Show

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

processOptions :: [Option] -> BufMem
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
