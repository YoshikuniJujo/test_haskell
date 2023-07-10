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

module Main where

import Foreign.Storable
import Data.Kind
import Data.Kind.Object qualified as KObj
import Gpu.Vulkan.Object qualified as VObj
import Data.Default
import Data.Bits
import Data.List.Length
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.Index qualified as TIndex
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.List
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Word
import System.Environment
import System.Console.GetOpt

import qualified Data.Vector.Storable as V

import Shaderc.TH
import Shaderc.EnumAuto
import Gpu.Vulkan.Misc

import qualified Gpu.Vulkan as Vk
import qualified "try-my-vulkan-snd" Gpu.Vulkan.Enum as Vk
import qualified Gpu.Vulkan.Instance as Vk.Inst
import qualified Gpu.Vulkan.PhysicalDevice as Vk.PhDvc
import qualified Gpu.Vulkan.PhysicalDevice.Struct as Vk.PhDvc
import qualified Gpu.Vulkan.Queue as Vk.Queue
import qualified Gpu.Vulkan.Queue.Enum as Vk.Queue
import qualified Gpu.Vulkan.QueueFamily as Vk.QFam
import qualified Gpu.Vulkan.Device as Vk.Dvc
import qualified Gpu.Vulkan.CommandPool as Vk.CommandPool
import qualified "try-my-vulkan-snd" Gpu.Vulkan.CommandPool.Enum as Vk.CommandPool
import qualified "try-my-vulkan-snd" Gpu.Vulkan.Buffer.Enum as Vk.Buffer
import qualified Gpu.Vulkan.Memory as Vk.Mem
import qualified Gpu.Vulkan.Memory.Kind as Vk.Mem.K
import qualified Gpu.Vulkan.Memory.Enum as Vk.Mem
import qualified Gpu.Vulkan.Memory.Middle as Vk.Mem.M
import qualified Gpu.Vulkan.Descriptor as Vk.Dsc
import qualified "try-my-vulkan-snd" Gpu.Vulkan.Descriptor.Enum as Vk.Dsc
import qualified Gpu.Vulkan.DescriptorPool as Vk.DscPool
import qualified "try-my-vulkan-snd" Gpu.Vulkan.DescriptorPool.Enum as Vk.DscPool
import qualified Gpu.Vulkan.ShaderModule as Vk.ShaderMod
import qualified "try-my-vulkan-snd" Gpu.Vulkan.Pipeline.Enum as Vk.Ppl
import qualified Gpu.Vulkan.PipelineLayout as Vk.Ppl.Lyt
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShaderSt
import qualified Gpu.Vulkan.Pipeline.Compute as Vk.Ppl.Cmpt
import qualified Gpu.Vulkan.DescriptorSet as Vk.DscSet
import qualified Gpu.Vulkan.DescriptorSetLayout.UpdateDynamicLengths as Vk.DscSet
import qualified Gpu.Vulkan.DescriptorSet.BindingAndArrayElem.Buffer as Vk.DscSet
import qualified Gpu.Vulkan.CommandBuffer as Vk.CmdBuf
import qualified "try-my-vulkan-snd" Gpu.Vulkan.CommandBuffer.Enum as Vk.CmdBuf
import qualified Gpu.Vulkan.Cmd as Vk.Cmd

import qualified Gpu.Vulkan.Buffer as Vk.Buffer
import qualified Gpu.Vulkan.Memory.AllocateInfo as Vk.Dvc.Mem.Buffer
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscSetLyt

import qualified Gpu.Vulkan.Khr as Vk.Khr
import qualified Gpu.Vulkan.PushConstant as Vk.PushConstant

main :: IO ()
main = do
	args <- getArgs
	let	(opts, noopts, emsgs) = getOpt RequireOrder options args
	case (emsgs, noopts) of
		([], []) -> do
			let	opt' = processOptions opts
			print opt'
			(r1, r2, r3) <- calc opt' datA datB datC
			print . take 20 $ unW1 <$> r1
			print . take 20 $ unW2 <$> r2
			print . take 20 $ unW3 <$> r3
		_ -> do	putStrLn `mapM_` emsgs
			putStrLn "Unsuitable args:"
			putStrLn `mapM_` noopts

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
data BufMem = Buffer1Memory1 | Buffer3Memory1 | Buffer3Memory3 deriving Show

processOptions :: [Option] -> BufMem
processOptions opts = case (b1, m1) of
	(False, False) -> Buffer3Memory3
	(False, True) -> Buffer3Memory1
	(True, True) -> Buffer1Memory1
	_ -> Buffer3Memory3
	where b1 = Buffer1 `elem` opts; m1 = Memory1 `elem` opts

newtype W1 = W1 { unW1 :: Float } deriving (Show, Storable)
newtype W2 = W2 { unW2 :: Float } deriving (Show, Storable)
newtype W3 = W3 { unW3 :: Float } deriving (Show, Storable)

dataSize :: Num n => n
dataSize = 1000000

datA :: V.Vector W1; datA = V.replicate dataSize $ W1 3
datB :: V.Vector W2; datB = V.fromList $ W2 <$> [1 .. dataSize]
datC :: V.Vector W3; datC = V.replicate dataSize $ W3 0

calc :: forall w1 w2 w3 . (
	Storable w1, Storable w2, Storable w3,
	Vk.Mem.OffsetSizeObject (VObj.List 256 w2 "") '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""],
	Vk.Mem.OffsetSizeObject (VObj.List 256 w3 "") '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""],
	VObj.Offset (VObj.List 256 w2 "") (ListBuffer1 w1 w2 w3),
	VObj.Offset (VObj.List 256 w3 "") (ListBuffer1 w1 w2 w3)
	) =>
	BufMem -> V.Vector w1 -> V.Vector w2 -> V.Vector w3 ->
	IO ([w1], [w2], [w3])
calc opt da db dc = withDevice \phdvc qFam dvc maxGroupCountX ->
	Vk.DscSetLyt.create dvc (dscSetLayoutInfo @w1 @w2 @w3) nil' \dscSetLyt ->

	let	n = fromIntegral maxGroupCountX
		da' = V.take n da; db' = V.take n db; dc' = V.take n dc in

	case opt of
		Buffer3Memory3 ->
			prepareMems
				phdvc dvc dscSetLyt da' db' dc' \dscSet
					(ma :: Vk.Mem.M sm1 '[ '( sb1, 'Vk.Mem.K.Buffer nm1 '[VObj.List 256 w1 ""])])
					(mb :: Vk.Mem.M sm2 '[ '( sb2, 'Vk.Mem.K.Buffer nm2 '[VObj.List 256 w2 ""])])
					(mc :: Vk.Mem.M sm3 '[ '( sb3, 'Vk.Mem.K.Buffer nm3 '[VObj.List 256 w3 ""])]) ->
			calc' @_ @_ @_ @nm1 @nm2 @nm3 dvc qFam dscSetLyt dscSet maxGroupCountX ma mb mc
		Buffer3Memory1 ->
			prepareMems' phdvc dvc dscSetLyt da' db' dc' \dscSet m ->
			calc' @_ @_ @_ @"buffer1" @"buffer2" @"buffer3" dvc qFam dscSetLyt dscSet maxGroupCountX m m m
		Buffer1Memory1 ->
			prepareMems'' phdvc dvc dscSetLyt da' db' dc' \dscSet
				(m :: Vk.Mem.M sm '[ '(sb, 'Vk.Mem.K.Buffer nm '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""])]) ->
			calc' @_ @_ @_ @nm @nm @nm dvc qFam dscSetLyt dscSet maxGroupCountX m m m

calc' :: forall w1 w2 w3 nm1 nm2 nm3 objss1 objss2 objss3 slbts sl bts sd sm1 sm2 sm3 sds .
	(
	Vk.DscSetLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[]],
	slbts ~ '(sl, bts),
	Show (HeteroParList.PL
		(HeteroParList.PL KObj.ObjectLength)
		(Vk.DscSetLyt.BindingTypeListBufferOnlyDynamics (TIndex.I1_2 slbts))),
	Storable w1, Storable w2, Storable w3,
	Vk.Mem.OffsetSize' nm1 (VObj.List 256 w1 "") objss1,
	Vk.Mem.OffsetSize' nm2 (VObj.List 256 w2 "") objss2,
	Vk.Mem.OffsetSize' nm3 (VObj.List 256 w3 "") objss3,
	InfixIndex '[slbts] '[ '(sl, bts)]) =>
	Vk.Dvc.D sd -> Vk.QFam.Index -> Vk.DscSetLyt.L sl bts ->
	Vk.DscSet.D sds slbts -> Word32 ->
	Vk.Mem.M sm1 objss1 -> Vk.Mem.M sm2 objss2 ->
	Vk.Mem.M sm3 objss3 -> IO ([w1], [w2], [w3])
calc' dvc qFam dscSetLyt dscSet dsz ma mb mc =
	Vk.Ppl.Lyt.createNew dvc (pplLayoutInfo dscSetLyt) nil' \pplLyt ->
	Vk.Ppl.Cmpt.createCsNew
		dvc Nothing (U4 (computePipelineInfo pplLyt) :** HeteroParList.Nil)
		nil' \(ppl :** HeteroParList.Nil) ->
	Vk.CommandPool.create dvc (commandPoolInfo qFam) nil' \cmdPool ->
	Vk.CmdBuf.allocate dvc (commandBufferInfo cmdPool) \(cmdBuf :*. HeteroParList.Nil) ->
		run @nm1 @nm2 @nm3 dvc qFam cmdBuf ppl pplLyt dscSet dsz ma mb mc

type ListBuffer1 w1 w2 w3 = '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""]
type ListBuffer3Memory3 w1 w2 w3 = '[ '[VObj.List 256 w1 ""], '[VObj.List 256 w2 ""], '[VObj.List 256 w3 ""]]

run :: forall nm1 nm2 nm3 w1 w2 w3
	objss1 objss2 objss3 slbts sbtss sd sc sg sl sm1 sm2 sm3 sds . (
	Vk.Cmd.LayoutArgListOnlyDynamics sbtss ~ '[ '[ '[]]],
	sbtss ~ '[slbts],
	Show (HeteroParList.PL
		(HeteroParList.PL KObj.ObjectLength)
		(Vk.DscSetLyt.BindingTypeListBufferOnlyDynamics (TIndex.I1_2 slbts))),
	Storable w1, Storable w2, Storable w3,
	Vk.Mem.OffsetSize' nm1 (VObj.List 256 w1 "") objss1,
	Vk.Mem.OffsetSize' nm2 (VObj.List 256 w2 "") objss2,
	Vk.Mem.OffsetSize' nm3 (VObj.List 256 w3 "") objss3,
	InfixIndex '[slbts] sbtss ) =>
	Vk.Dvc.D sd -> Vk.QFam.Index -> Vk.CmdBuf.C sc -> Vk.Ppl.Cmpt.C sg '(sl, sbtss, '[]) ->
	Vk.Ppl.Lyt.L sl sbtss '[] -> Vk.DscSet.D sds slbts -> Word32 ->
	Vk.Mem.M sm1 objss1 -> Vk.Mem.M sm2 objss2 ->
	Vk.Mem.M sm3 objss3 -> IO ([w1], [w2], [w3])
run dvc qFam cmdBuf ppl pplLyt dscSet dsz memA memB memC = do
	queue <- Vk.Dvc.getQueue dvc qFam 0
	Vk.CmdBuf.begin @'Nothing @'Nothing cmdBuf def $
		Vk.Cmd.bindPipelineCompute cmdBuf Vk.Ppl.BindPointCompute ppl \ccb -> do
			Vk.Cmd.bindDescriptorSetsCompute ccb pplLyt
				(U2 dscSet :** HeteroParList.Nil)
				(HeteroParList.Singleton $ HeteroParList.Singleton HeteroParList.Nil)
			Vk.Cmd.dispatch ccb dsz 1 1
	Vk.Queue.submit queue (U4 submitInfo :** HeteroParList.Nil) Nothing
	Vk.Queue.waitIdle queue
	(,,)	<$> Vk.Mem.read @nm1 @(VObj.List 256 w1 "") @[w1] dvc memA def
		<*> Vk.Mem.read @nm2 @(VObj.List 256 w2 "") @[w2] dvc memB def
		<*> Vk.Mem.read @nm3 @(VObj.List 256 w3 "") @[w3] dvc memC def
	where	submitInfo :: Vk.SubmitInfo 'Nothing _ _ _
		submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = TMaybe.N,
			Vk.submitInfoWaitSemaphoreDstStageMasks = HeteroParList.Nil,
			Vk.submitInfoCommandBuffers = cmdBuf :** HeteroParList.Nil,
			Vk.submitInfoSignalSemaphores = HeteroParList.Nil }

withDevice ::
	(forall sd . Vk.PhDvc.P -> Vk.QFam.Index -> Vk.Dvc.D sd -> Word32 -> IO a) -> IO a
withDevice f = Vk.Inst.create @_ @'Nothing instInfo nil' \inst -> do
	phdvc <- head <$> Vk.PhDvc.enumerate inst
	limits <- Vk.PhDvc.propertiesLimits <$> Vk.PhDvc.getProperties phdvc
	let	maxGroupCountX :. _ =
			Vk.PhDvc.limitsMaxComputeWorkGroupCount limits
	putStrLn $ "maxGroupCountX: " ++ show maxGroupCountX
	qFam <- findQueueFamily phdvc Vk.Queue.ComputeBit
	Vk.Dvc.create @'Nothing @'[ 'Nothing] phdvc (dvcInfo qFam) nil' $ \dvc -> f phdvc qFam dvc maxGroupCountX
	where
	dvcInfo qFam = Vk.Dvc.CreateInfo {
		Vk.Dvc.createInfoNext = TMaybe.N,
		Vk.Dvc.createInfoFlags = def,
		Vk.Dvc.createInfoQueueCreateInfos = HeteroParList.Singleton $ queueInfo qFam,
		Vk.Dvc.createInfoEnabledLayerNames =
			[Vk.Khr.validationLayerName],
		Vk.Dvc.createInfoEnabledExtensionNames = [],
		Vk.Dvc.createInfoEnabledFeatures = Nothing }
	queueInfo qFam = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = def,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qFam,
		Vk.Dvc.queueCreateInfoQueuePriorities = [0] }

instInfo :: Vk.Inst.CreateInfo 'Nothing 'Nothing
instInfo = def {
	Vk.Inst.createInfoEnabledLayerNames = [Vk.Khr.validationLayerName] }

findQueueFamily ::
	Vk.PhDvc.P -> Vk.Queue.FlagBits -> IO Vk.QFam.Index
findQueueFamily phdvc qb = do
	qFamProperties <- Vk.PhDvc.getQueueFamilyProperties phdvc
	pure . fst . head $ filter ((/= zeroBits)
			. (.&. qb) . Vk.QFam.propertiesQueueFlags . snd)
		qFamProperties

dscSetLayoutInfo :: Vk.DscSetLyt.CreateInfo 'Nothing
	'[ 'Vk.DscSetLyt.Buffer '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""]]
dscSetLayoutInfo = Vk.DscSetLyt.CreateInfo {
	Vk.DscSetLyt.createInfoNext = TMaybe.N,
	Vk.DscSetLyt.createInfoFlags = def,
	Vk.DscSetLyt.createInfoBindings = binding0 :** HeteroParList.Nil }

binding0 :: Vk.DscSetLyt.Binding ('Vk.DscSetLyt.Buffer objs)
binding0 = Vk.DscSetLyt.BindingBuffer {
	Vk.DscSetLyt.bindingBufferDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSetLyt.bindingBufferStageFlags = Vk.ShaderStageComputeBit }

prepareMems ::
	forall bts w1 w2 w3 sd sl nm1 nm2 nm3 a . (
	Default (HeteroParList.PL
		(HeteroParList.PL KObj.ObjectLength)
		(Vk.DscSetLyt.BindingTypeListBufferOnlyDynamics bts)),
	Storable w1, Storable w2, Storable w3,

	Vk.DscSet.BindingAndArrayElemBuffer bts '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""] 0,
	Vk.DscSet.UpdateDynamicLength bts '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""]
	) =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.DscSetLyt.L sl bts ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (forall sds sm1 sb1 sm2 sb2 sm3 sb3 .
		Vk.DscSet.D sds '(sl, bts) ->
		Vk.Mem.M sm1 '[ '( sb1, 'Vk.Mem.K.Buffer nm1 '[VObj.List 256 w1 ""])] ->
		Vk.Mem.M sm2 '[ '( sb2, 'Vk.Mem.K.Buffer nm2 '[VObj.List 256 w2 ""])] ->
		Vk.Mem.M sm3 '[ '( sb3, 'Vk.Mem.K.Buffer nm3 '[VObj.List 256 w3 ""])] -> IO a) -> IO a
prepareMems phdvc dvc dscSetLyt da db dc f =
	Vk.DscPool.create dvc dscPoolInfo nil' \dscPool ->
	Vk.DscSet.allocateDs dvc (dscSetInfo dscPool dscSetLyt)
		\(dscSet :** HeteroParList.Nil) ->
	storageBufferNew3' dvc phdvc da db dc \ba ma bb mb bc mc ->
	Vk.DscSet.updateDs dvc (U5
		(writeDscSet @w1 @w2 @w3 dscSet ba bb bc) :** HeteroParList.Nil) HeteroParList.Nil >>
	f dscSet ma mb mc

prepareMems' ::
	forall bts w1 w2 w3 sd sl a . (
	Default (HeteroParList.PL
		(HeteroParList.PL KObj.ObjectLength)
		(Vk.DscSetLyt.BindingTypeListBufferOnlyDynamics bts)),
	Storable w1, Storable w2, Storable w3,
	Vk.DscSet.BindingAndArrayElemBuffer bts '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""] 0,
	Vk.DscSet.UpdateDynamicLength bts '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""] ) =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.DscSetLyt.L sl bts ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (forall sds sm sb1 sb2 sb3 .
		Vk.DscSet.D sds '(sl, bts) ->
		Vk.Mem.M sm '[
			'(sb1, 'Vk.Mem.K.Buffer "buffer1" '[VObj.List 256 w1 ""]),
			'(sb2, 'Vk.Mem.K.Buffer "buffer2" '[VObj.List 256 w2 ""]),
			'(sb3, 'Vk.Mem.K.Buffer "buffer3" '[VObj.List 256 w3 ""])
			] -> IO a) -> IO a
prepareMems' phdvc dvc dscSetLyt da db dc f =
	Vk.DscPool.create dvc dscPoolInfo nil' \dscPool ->
	Vk.DscSet.allocateDs dvc (dscSetInfo dscPool dscSetLyt)
		\(dscSet :** HeteroParList.Nil) ->
	storage3BufferNew dvc phdvc da db dc \ba bb bc m ->
	Vk.DscSet.updateDs dvc (U5
		(writeDscSet @w1 @w2 @w3 dscSet ba bb bc) :** HeteroParList.Nil) HeteroParList.Nil >>
	f dscSet m

prepareMems'' :: forall w1 w2 w3 sd sl bts nm a . (
	Default (HeteroParList.PL
		(HeteroParList.PL KObj.ObjectLength)
		(Vk.DscSetLyt.BindingTypeListBufferOnlyDynamics bts)),
	Storable w1, Storable w2, Storable w3,
	VObj.Offset (VObj.List 256 w2 "") '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 "" ],
	VObj.Offset (VObj.List 256 w3 "") '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 "" ],
	Vk.Mem.OffsetSizeObject
		(VObj.List 256 w2 "") '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""],
	Vk.Mem.OffsetSizeObject
		(VObj.List 256 w3 "") '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""],
	Vk.DscSet.BindingAndArrayElemBuffer bts '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""] 0,
	Vk.DscSet.UpdateDynamicLength bts '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""]
	) =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.DscSetLyt.L sl bts ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (forall sds sm sb .
		Vk.DscSet.D sds '(sl, bts) ->
		Vk.Mem.M sm '[ '(sb, 'Vk.Mem.K.Buffer nm '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""])] -> IO a) -> IO a
prepareMems'' phdvc dvc dscSetLyt da db dc f =
	Vk.DscPool.create dvc dscPoolInfo nil' \dscPool ->
	Vk.DscSet.allocateDs dvc (dscSetInfo dscPool dscSetLyt)
		\(dscSet :** HeteroParList.Nil) ->
	storage1BufferNew dvc phdvc da db dc \b m ->
	Vk.DscSet.updateDs dvc (U5
		(writeDscSet' @w1 @w2 @w3 dscSet b) :** HeteroParList.Nil) HeteroParList.Nil >>
	f dscSet m

storageBufferNew3' :: (Storable w1, Storable w2, Storable w3) =>
	Vk.Dvc.D sd -> Vk.PhDvc.P ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (
		forall sb1 sm1 sb2 sm2 sb3 sm3 .
		Vk.Buffer.Binded sm1 sb1 nm1 '[VObj.List 256 w1 ""] ->
		Vk.Mem.M sm1 '[ '(sb1, 'Vk.Mem.K.Buffer nm1 '[VObj.List 256 w1 ""])] ->
		Vk.Buffer.Binded sm2 sb2 nm2 '[VObj.List 256 w2 ""] ->
		Vk.Mem.M sm2 '[ '(sb2, 'Vk.Mem.K.Buffer nm2 '[VObj.List 256 w2 ""])] ->
		Vk.Buffer.Binded sm3 sb3 nm3 '[VObj.List 256 w3 ""] ->
		Vk.Mem.M sm3 '[ '(sb3, 'Vk.Mem.K.Buffer nm3 '[VObj.List 256 w3 ""])] -> IO a ) -> IO a
storageBufferNew3' dvc phdvc x y z f =
	storageBufferNews dvc phdvc (x :** y :** z :** HeteroParList.Nil) $ addArg3 f

addArg3 :: (forall sb1 sm1 sb2 sm2 sb3 sm3 .
	Vk.Buffer.Binded sm1 sb1 nm1 '[VObj.List 256 w1 ""] ->
	Vk.Mem.M sm1 '[ '(sb1, 'Vk.Mem.K.Buffer nm1 '[VObj.List 256 w1 ""])] ->
	Vk.Buffer.Binded sm2 sb2 nm2 '[VObj.List 256 w2 ""] ->
	Vk.Mem.M sm2 '[ '(sb2, 'Vk.Mem.K.Buffer nm2 '[VObj.List 256 w2 ""])] ->
	Vk.Buffer.Binded sm3 sb3 nm3 '[VObj.List 256 w3 ""] ->
	Vk.Mem.M sm3 '[ '(sb3, 'Vk.Mem.K.Buffer nm3 '[VObj.List 256 w3 ""])] -> r) ->
	Arg nm1 w1 (Arg nm2 w2 (Arg nm3 w3 r))
addArg3 f = Arg \b1 m1 -> Arg \b2 m2 -> Arg \b3 m3 -> f b1 m1 b2 m2 b3 m3

class StorageBufferNews f a where
	type Vectors f :: [Type]
	storageBufferNews :: Vk.Dvc.D sd -> Vk.PhDvc.P ->
		HeteroParList.PL V.Vector (Vectors f) -> f -> IO a

data Arg nm w f = Arg (forall sb sm .
	Vk.Buffer.Binded sm sb nm '[VObj.List 256 w ""] ->
	Vk.Mem.M sm '[ '(sb, 'Vk.Mem.K.Buffer nm '[VObj.List 256 w ""])] -> f)

instance StorageBufferNews (IO a) a where
	type Vectors (IO a) = '[]
	storageBufferNews _dvc _phdvc HeteroParList.Nil f = f

instance (Storable w, StorageBufferNews f a) =>
	StorageBufferNews (Arg nm w f) a where
	type Vectors (Arg nm w f) = w ': Vectors f
	storageBufferNews dvc phdvc (vs :** vss) (Arg f) =
		storageBufferNew dvc phdvc vs \buf mem ->
		storageBufferNews @f @a dvc phdvc vss (f buf mem)

storageBufferNew :: forall sd nm w a . Storable w =>
	Vk.Dvc.D sd -> Vk.PhDvc.P -> V.Vector w -> (
		forall sb sm .
		Vk.Buffer.Binded sm sb nm '[VObj.List 256 w ""]  ->
		Vk.Mem.M sm '[ '(sb, 'Vk.Mem.K.Buffer nm '[VObj.List 256 w ""])] -> IO a ) -> IO a
storageBufferNew dvc phdvc xs f =
	Vk.Buffer.create dvc (bufferInfo xs) nil' \buffer -> do
		memoryInfo <- getMemoryInfo phdvc dvc buffer
		Vk.Mem.allocateBind dvc (U2 (Vk.Mem.Buffer buffer) :** HeteroParList.Nil) memoryInfo
			nil' \(U2 (Vk.Mem.BufferBinded binded) :** HeteroParList.Nil) memory -> do
			Vk.Mem.write @nm @(VObj.List 256 w "") dvc memory def xs
			f binded memory

storage3BufferNew :: forall sd w1 w2 w3 a . (
	Storable w1, Storable w2, Storable w3
	) =>
	Vk.Dvc.D sd -> Vk.PhDvc.P ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (
		forall sb1 sb2 sb3 sm .
		Vk.Buffer.Binded sm sb1 "buffer1" '[VObj.List 256 w1 ""] ->
		Vk.Buffer.Binded sm sb2 "buffer2" '[VObj.List 256 w2 ""] ->
		Vk.Buffer.Binded sm sb3 "buffer3" '[VObj.List 256 w3 ""] ->
		Vk.Mem.M sm '[
			'(sb1, 'Vk.Mem.K.Buffer "buffer1" '[VObj.List 256 w1 ""]),
			'(sb2, 'Vk.Mem.K.Buffer "buffer2" '[VObj.List 256 w2 ""]),
			'(sb3, 'Vk.Mem.K.Buffer "buffer3" '[VObj.List 256 w3 ""]) ] -> IO a
		) -> IO a
storage3BufferNew dvc phdvc xs ys zs f =
	storage3BufferNewGen dvc phdvc xs ys zs \bnd1 bnd2 bnd3 mem -> do
		Vk.Mem.write @"buffer1" @(VObj.List 256 w1 "") dvc mem def xs
		Vk.Mem.write @"buffer2" @(VObj.List 256 w2 "") dvc mem def ys
		Vk.Mem.write @"buffer3" @(VObj.List 256 w3 "") dvc mem def zs
		f bnd1 bnd2 bnd3 mem

storage3BufferNewGen :: forall sd w1 w2 w3 a . (
	Storable w1, Storable w2, Storable w3
	) =>
	Vk.Dvc.D sd -> Vk.PhDvc.P ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (
		forall sb1 sb2 sb3 sm .
		Vk.Buffer.Binded sm sb1 "buffer1" '[VObj.List 256 w1 ""] ->
		Vk.Buffer.Binded sm sb2 "buffer2" '[VObj.List 256 w2 ""] ->
		Vk.Buffer.Binded sm sb3 "buffer3" '[VObj.List 256 w3 ""] ->
		Vk.Mem.M sm '[
			'(sb1, 'Vk.Mem.K.Buffer "buffer1" '[VObj.List 256 w1 ""]),
			'(sb2, 'Vk.Mem.K.Buffer "buffer2" '[VObj.List 256 w2 ""]),
			'(sb3, 'Vk.Mem.K.Buffer "buffer3" '[VObj.List 256 w3 ""]) ] -> IO a
		) -> IO a
storage3BufferNewGen dvc phdvc xs ys zs f =
	Vk.Buffer.create dvc (bufferInfo xs) nil' \buf1 -> do
		memInfo1 <- getMemoryInfo phdvc dvc buf1
		Vk.Buffer.create dvc (bufferInfo ys) nil' \buf2 -> do
			memInfo2 <- getMemoryInfo phdvc dvc buf2
			Vk.Buffer.create dvc (bufferInfo zs) nil' \buf3 -> do
				memInfo3 <- getMemoryInfo phdvc dvc buf3
				if (memInfo1 == memInfo2 && memInfo2 == memInfo3) then
					Vk.Mem.allocateBind dvc (
						U2 (Vk.Mem.Buffer buf1) :**
						U2 (Vk.Mem.Buffer buf2) :**
						U2 (Vk.Mem.Buffer buf3) :** HeteroParList.Nil
						) memInfo1 nil'
						\(	U2 (Vk.Mem.BufferBinded bnd1) :**
							U2 (Vk.Mem.BufferBinded bnd2) :**
							U2 (Vk.Mem.BufferBinded bnd3) :** HeteroParList.Nil ) mem ->
						f bnd1 bnd2 bnd3 mem
					else error "bad"

bufferInfo :: Storable w => V.Vector w -> Vk.Buffer.CreateInfo 'Nothing '[VObj.List 256 w ""]
bufferInfo xs = Vk.Buffer.CreateInfo {
	Vk.Buffer.createInfoNext = TMaybe.N,
	Vk.Buffer.createInfoFlags = def,
	Vk.Buffer.createInfoLengths =
		VObj.ObjectLengthList (V.length xs) :** HeteroParList.Nil,
	Vk.Buffer.createInfoUsage = Vk.Buffer.UsageStorageBufferBit,
	Vk.Buffer.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Buffer.createInfoQueueFamilyIndices = [] }

storage1BufferNew :: forall sd nm w1 w2 w3 a . (
	Storable w1, Storable w2, Storable w3,
	Vk.Mem.OffsetSizeObject (VObj.List 256 w2 "") '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""],
	Vk.Mem.OffsetSizeObject (VObj.List 256 w3 "") '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""] ) =>
	Vk.Dvc.D sd -> Vk.PhDvc.P ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (
		forall sb sm . -- (
		Vk.Buffer.Binded sm sb nm '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""] ->
		Vk.Mem.M sm
			'[ '(sb, 'Vk.Mem.K.Buffer nm '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""])] -> IO a) -> IO a
storage1BufferNew dvc phdvc xs ys zs f =
	Vk.Buffer.create dvc (bufferInfo' xs ys zs) nil' \buf -> do
		memInfo <- getMemoryInfo phdvc dvc buf
		Vk.Mem.allocateBind dvc (HeteroParList.Singleton . U2 $ Vk.Mem.Buffer buf)
			memInfo nil' \(HeteroParList.Singleton (U2 (Vk.Mem.BufferBinded bnd))) mem -> do
			Vk.Mem.write @nm @(VObj.List 256 w1 "") dvc mem def xs
			Vk.Mem.write @nm @(VObj.List 256 w2 "") dvc mem def ys
			Vk.Mem.write @nm @(VObj.List 256 w3 "") dvc mem def zs
			f bnd mem

bufferInfo' :: (
	Storable w1, Storable w2, Storable w3 ) =>
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 ->
	Vk.Buffer.CreateInfo 'Nothing '[VObj.List 256 w1 "",VObj.List 256 w2 "",VObj.List 256 w3 ""]
bufferInfo' xs ys zs = Vk.Buffer.CreateInfo {
	Vk.Buffer.createInfoNext = TMaybe.N,
	Vk.Buffer.createInfoFlags = def,
	Vk.Buffer.createInfoLengths =
		VObj.ObjectLengthList (V.length xs) :**
		VObj.ObjectLengthList (V.length ys) :**
		VObj.ObjectLengthList (V.length zs) :** HeteroParList.Nil,
	Vk.Buffer.createInfoUsage = Vk.Buffer.UsageStorageBufferBit,
	Vk.Buffer.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Buffer.createInfoQueueFamilyIndices = [] }

pplLayoutInfo :: Vk.DscSetLyt.L sl bts ->
	Vk.Ppl.Lyt.CreateInfoNew 'Nothing '[ '(sl, bts)]
		('Vk.PushConstant.PushConstantLayout '[] '[])
pplLayoutInfo dsl = Vk.Ppl.Lyt.CreateInfoNew {
	Vk.Ppl.Lyt.createInfoNextNew = TMaybe.N,
	Vk.Ppl.Lyt.createInfoFlagsNew = def,
	Vk.Ppl.Lyt.createInfoSetLayoutsNew =
		U2 dsl :** HeteroParList.Nil }

computePipelineInfo :: Vk.Ppl.Lyt.L sl sbtss '[] ->
	Vk.Ppl.Cmpt.CreateInfo 'Nothing
		'( 'Nothing, 'Nothing, 'GlslComputeShader, 'Nothing, '[Word32, Word32])
		'(sl, sbtss, '[]) sbph
computePipelineInfo pl = Vk.Ppl.Cmpt.CreateInfo {
	Vk.Ppl.Cmpt.createInfoNext = TMaybe.N,
	Vk.Ppl.Cmpt.createInfoFlags = def,
	Vk.Ppl.Cmpt.createInfoStage = U5 shaderStageInfo,
	Vk.Ppl.Cmpt.createInfoLayout = U3 pl,
	Vk.Ppl.Cmpt.createInfoBasePipelineHandleOrIndex = Nothing }

commandPoolInfo :: Vk.QFam.Index -> Vk.CommandPool.CreateInfo 'Nothing
commandPoolInfo qFam = Vk.CommandPool.CreateInfo {
	Vk.CommandPool.createInfoNext = TMaybe.N,
	Vk.CommandPool.createInfoFlags =
		Vk.CommandPool.CreateResetCommandBufferBit,
	Vk.CommandPool.createInfoQueueFamilyIndex = qFam }

dscSetInfo :: Vk.DscPool.P sp -> Vk.DscSetLyt.L sl bts ->
	Vk.DscSet.AllocateInfo 'Nothing sp '[ '(sl, bts)]
dscSetInfo pl lyt = Vk.DscSet.AllocateInfo {
	Vk.DscSet.allocateInfoNext = TMaybe.N,
	Vk.DscSet.allocateInfoDescriptorPool = pl,
	Vk.DscSet.allocateInfoSetLayouts = U2 lyt :** HeteroParList.Nil }

commandBufferInfo :: Vk.CommandPool.C s -> Vk.CmdBuf.AllocateInfo 'Nothing s '[ '()]
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

getMemoryInfo :: Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Buffer.B sb nm objs ->
	IO (Vk.Dvc.Mem.Buffer.AllocateInfo 'Nothing)
getMemoryInfo phdvc dvc buffer = do
	requirements <- Vk.Buffer.getMemoryRequirements dvc buffer
	memTypeIdx <- findMemoryTypeIndex phdvc requirements (
		Vk.Mem.PropertyHostVisibleBit .|.
		Vk.Mem.PropertyHostCoherentBit )
	pure Vk.Dvc.Mem.Buffer.AllocateInfo {
		Vk.Dvc.Mem.Buffer.allocateInfoNext = TMaybe.N,
		Vk.Dvc.Mem.Buffer.allocateInfoMemoryTypeIndex = memTypeIdx }

findMemoryTypeIndex ::
	Vk.PhDvc.P -> Vk.Mem.M.Requirements -> Vk.Mem.PropertyFlags ->
	IO Vk.Mem.M.TypeIndex
findMemoryTypeIndex physicalDevice requirements memoryProp = do
	memoryProperties <- Vk.PhDvc.getMemoryProperties physicalDevice
	let	reqTypes = Vk.Mem.M.requirementsMemoryTypeBits requirements
		memPropTypes = (fst <$>)
			. filter (checkBits memoryProp
				. Vk.Mem.M.mTypePropertyFlags . snd)
			$ Vk.PhDvc.memoryPropertiesMemoryTypes memoryProperties
	case filter (`Vk.Mem.M.elemTypeIndex` reqTypes) memPropTypes of
		[] -> error "No available memory types"
		i : _ -> pure i

checkBits :: Bits bs => bs -> bs -> Bool
checkBits bs0 = (== bs0) . (.&. bs0)

writeDscSet ::
	forall w1 w2 w3 slbts sb1 sb2 sb3 sm1 sm2 sm3 nm1 nm2 nm3 objs1 objs2 objs3 sds . (
	Show (HeteroParList.PL VObj.ObjectLength objs1),
	Show (HeteroParList.PL VObj.ObjectLength objs2),
	Show (HeteroParList.PL VObj.ObjectLength objs3),
	VObj.Offset (VObj.List 256 w1 "") objs1,
	VObj.Offset (VObj.List 256 w2 "") objs2,
	VObj.Offset (VObj.List 256 w3 "") objs3 ) =>
	Vk.DscSet.D sds slbts ->
	Vk.Buffer.Binded sm1 sb1 nm1 objs1 -> Vk.Buffer.Binded sm2 sb2 nm2 objs2 ->
	Vk.Buffer.Binded sm3 sb3 nm3 objs3 ->
	Vk.DscSet.Write 'Nothing sds slbts ('Vk.DscSet.WriteSourcesArgBuffer '[
		'(sm1, sb1, nm1, VObj.List 256 w1 ""), '(sm2, sb2, nm2, VObj.List 256 w2 ""),
		'(sm3, sb3, nm3, VObj.List 256 w3 "") ]) 0
writeDscSet ds ba bb bc = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = ds,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		U4 (bufferInfoList @w1 ba) :**
		U4 (bufferInfoList @w2 bb) :**
		U4 (bufferInfoList @w3 bc) :** HeteroParList.Nil }

writeDscSet' :: forall w1 w2 w3 slbts sb sm nm objs sds . (
	Show (HeteroParList.PL VObj.ObjectLength objs),
	VObj.Offset (VObj.List 256 w1 "") objs,
	VObj.Offset (VObj.List 256 w2 "") objs,
	VObj.Offset (VObj.List 256 w3 "") objs ) =>
	Vk.DscSet.D sds slbts ->
	Vk.Buffer.Binded sm sb nm objs ->
	Vk.DscSet.Write 'Nothing sds slbts ('Vk.DscSet.WriteSourcesArgBuffer '[
		'(sm, sb, nm, VObj.List 256 w1 ""),
		'(sm, sb, nm, VObj.List 256 w2 ""),
		'(sm, sb, nm, VObj.List 256 w3 "") ]) 0
writeDscSet' ds b = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = ds,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		U4 (bufferInfoList @w1 b) :**
		U4 (bufferInfoList @w2 b) :**
		U4 (bufferInfoList @w3 b) :** HeteroParList.Nil }

bufferInfoList :: forall t {sb} {sm} {nm} {objs} . (
	Show (HeteroParList.PL VObj.ObjectLength objs),
	VObj.Offset (VObj.List 256 t "") objs ) =>
	Vk.Buffer.Binded sm sb nm objs ->
	Vk.Dsc.BufferInfo sm sb nm (VObj.List 256 t "")
bufferInfoList = Vk.Dsc.BufferInfo

shaderStageInfo :: Vk.Ppl.ShaderSt.CreateInfoNew
	'Nothing 'Nothing 'GlslComputeShader 'Nothing '[Word32, Word32]
shaderStageInfo = Vk.Ppl.ShaderSt.CreateInfoNew {
	Vk.Ppl.ShaderSt.createInfoNextNew = TMaybe.N,
	Vk.Ppl.ShaderSt.createInfoFlagsNew = def,
	Vk.Ppl.ShaderSt.createInfoStageNew = Vk.ShaderStageComputeBit,
	Vk.Ppl.ShaderSt.createInfoModuleNew = Vk.ShaderMod.M shaderModInfo nil',
	Vk.Ppl.ShaderSt.createInfoNameNew = "main",
	Vk.Ppl.ShaderSt.createInfoSpecializationInfoNew =
		Just $ HeteroParList.Id 3 :** HeteroParList.Id 10 :** HeteroParList.Nil }
	where shaderModInfo = Vk.ShaderMod.CreateInfo {
		Vk.ShaderMod.createInfoNext = TMaybe.N,
		Vk.ShaderMod.createInfoFlags = def,
		Vk.ShaderMod.createInfoCode = glslComputeShaderMain }

[glslComputeShader|

#version 460
layout(local_size_x = 1, local_size_y = 1) in;
layout(binding = 0) buffer Data {
	float val[];
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
