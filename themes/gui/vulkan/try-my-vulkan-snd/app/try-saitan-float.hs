{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.Storable
import Data.Kind
import Data.Kind.Object
import Data.Default
import Data.Bits
import Data.List.Length
import Data.HeteroList
import Data.Word
import System.Environment
import System.Console.GetOpt

import qualified Data.Vector.Storable as V

import Shaderc.TH
import Shaderc.EnumAuto
import Gpu.Vulkan.Base

import qualified Gpu.Vulkan as Vk
import qualified Gpu.Vulkan.Enum as Vk
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
import qualified Gpu.Vulkan.Memory.Tmp as Vk.Mem
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

import qualified Old.Gpu.Vulkan.Device.Memory.Buffer as Vk.Dvc.Mem.Buffer
import qualified Old.Gpu.Vulkan.Device.Memory.Buffer.TypeLevel as Vk.Dvc.Mem.Buffer

import qualified Gpu.Vulkan.Khr as Vk.Khr

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
	Vk.Dvc.Mem.Buffer.OffsetSize ('List w2 "") (ListBuffer3Memory3 w1 w2 w3),
	Vk.Dvc.Mem.Buffer.OffsetSize ('List w3 "") (ListBuffer3Memory3 w1 w2 w3),
	Offset ('List w2 "") (ListBuffer1 w1 w2 w3),
	Offset ('List w3 "") (ListBuffer1 w1 w2 w3),
	Vk.Dvc.Mem.Buffer.OffsetSize ('List w2 "") '[ListBuffer1 w1 w2 w3],
	Vk.Dvc.Mem.Buffer.OffsetSize ('List w3 "") '[ListBuffer1 w1 w2 w3] ) =>
	BufMem -> V.Vector w1 -> V.Vector w2 -> V.Vector w3 ->
	IO ([w1], [w2], [w3])
calc opt da db dc = withDevice \phdvc qFam dvc maxGroupCountX ->
	Vk.DscSetLyt.create dvc (dscSetLayoutInfo @w1 @w2 @w3) nil nil \dscSetLyt ->

	let	n = fromIntegral maxGroupCountX
		da' = V.take n da; db' = V.take n db; dc' = V.take n dc in

	case opt of
		Buffer3Memory3 ->
			prepareMems
				phdvc dvc dscSetLyt da' db' dc' \dscSet ma mb mc ->
			calc' dvc qFam dscSetLyt dscSet maxGroupCountX ma mb mc
		Buffer3Memory1 ->
			prepareMems' phdvc dvc dscSetLyt da' db' dc' \dscSet m ->
			calc' dvc qFam dscSetLyt dscSet maxGroupCountX m m m
		Buffer1Memory1 ->
			prepareMems'' phdvc dvc dscSetLyt da' db' dc' \dscSet m ->
			calc' dvc qFam dscSetLyt dscSet maxGroupCountX m m m

calc' :: (
	Storable w1, Storable w2, Storable w3,
	Vk.Dvc.Mem.Buffer.OffsetSize ('List w1 "") objss1,
	Vk.Dvc.Mem.Buffer.OffsetSize ('List w2 "") objss2,
	Vk.Dvc.Mem.Buffer.OffsetSize ('List w3 "") objss3,
	Vk.Cmd.SetPos '[slbts] '[ '(sl, bts)]) =>
	Vk.Dvc.D sd -> Vk.QFam.Index -> Vk.DscSetLyt.L sl bts ->
	Vk.DscSet.S sd sp slbts -> Word32 ->
	Vk.Dvc.Mem.Buffer.M sm1 objss1 -> Vk.Dvc.Mem.Buffer.M sm2 objss2 ->
	Vk.Dvc.Mem.Buffer.M sm3 objss3 -> IO ([w1], [w2], [w3])
calc' dvc qFam dscSetLyt dscSet dsz ma mb mc =
	Vk.Ppl.Lyt.create dvc (pplLayoutInfo dscSetLyt) nil nil \pplLyt ->
	Vk.Ppl.Cmpt.createCs @'[ '(Word32 :.: Word32 :.: (), _, _, _)] dvc Nothing
		(Vk.Ppl.Cmpt.CreateInfo_
			(computePipelineInfo pplLyt) :...: HVNil)
		nil nil \(Vk.Ppl.Cmpt.Pipeline ppl :...: HVNil) ->
	Vk.CommandPool.create dvc (commandPoolInfo qFam) nil nil \cmdPool ->
	Vk.CmdBuf.allocate dvc (commandBufferInfo cmdPool) \case
		[cmdBuf] -> run dvc qFam cmdBuf ppl pplLyt dscSet dsz ma mb mc
		_ -> error "never occur"

type ListBuffer1 w1 w2 w3 = '[ 'List w1 "", 'List w2 "", 'List w3 ""]
type ListBuffer3Memory3 w1 w2 w3 = '[ '[ 'List w1 ""], '[ 'List w2 ""], '[ 'List w3 ""]]

run :: forall w1 w2 w3
	objss1 objss2 objss3 slbts sbtss sd sc vs sg sl sp sm1 sm2 sm3 . (
	Storable w1, Storable w2, Storable w3,
	Vk.Dvc.Mem.Buffer.OffsetSize ('List w1 "") objss1,
	Vk.Dvc.Mem.Buffer.OffsetSize ('List w2 "") objss2,
	Vk.Dvc.Mem.Buffer.OffsetSize ('List w3 "") objss3,
	Vk.Cmd.SetPos '[slbts] sbtss ) =>
	Vk.Dvc.D sd -> Vk.QFam.Index -> Vk.CmdBuf.C sc vs -> Vk.Ppl.Cmpt.C sg ->
	Vk.Ppl.Lyt.LL sl sbtss -> Vk.DscSet.S sd sp slbts -> Word32 ->
	Vk.Dvc.Mem.Buffer.M sm1 objss1 -> Vk.Dvc.Mem.Buffer.M sm2 objss2 ->
	Vk.Dvc.Mem.Buffer.M sm3 objss3 -> IO ([w1], [w2], [w3])
run dvc qFam cmdBuf ppl pplLyt dscSet dsz memA memB memC = do
	queue <- Vk.Dvc.getQueue dvc qFam 0
	Vk.CmdBuf.begin @() @() cmdBuf def do
		Vk.Cmd.bindPipelineCompute cmdBuf Vk.Ppl.BindPointCompute ppl
		Vk.Cmd.bindDescriptorSets cmdBuf Vk.Ppl.BindPointCompute pplLyt
			(Vk.Cmd.DescriptorSet dscSet :...: HVNil) []
		Vk.Cmd.dispatch cmdBuf dsz 1 1
	Vk.Queue.submit @() queue [submitInfo] Nothing
	Vk.Queue.waitIdle queue
	(,,)	<$> Vk.Dvc.Mem.Buffer.read @[w1] @('List w1 "") dvc memA def
		<*> Vk.Dvc.Mem.Buffer.read @[w2] @('List w2 "") dvc memB def
		<*> Vk.Dvc.Mem.Buffer.read @[w3] @('List w3 "") dvc memC def
	where	submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = Nothing,
			Vk.submitInfoWaitSemaphoreDstStageMasks = HVNil,
			Vk.submitInfoCommandBuffers = [cmdBuf],
			Vk.submitInfoSignalSemaphores = [] }

withDevice ::
	(forall sd . Vk.PhDvc.P -> Vk.QFam.Index -> Vk.Dvc.D sd -> Word32 -> IO a) -> IO a
withDevice f = Vk.Inst.create @() @() instInfo nil nil \inst -> do
	phdvc <- head <$> Vk.PhDvc.enumerate inst
	limits <- Vk.PhDvc.propertiesLimits <$> Vk.PhDvc.getProperties phdvc
	let	maxGroupCountX :. _ =
			Vk.PhDvc.limitsMaxComputeWorkGroupCount limits
	putStrLn $ "maxGroupCountX: " ++ show maxGroupCountX
	qFam <- findQueueFamily phdvc Vk.Queue.ComputeBit
	Vk.Dvc.create @() @'[()] phdvc (dvcInfo qFam) nil nil $ \dvc -> f phdvc qFam dvc maxGroupCountX
	where
	dvcInfo qFam = Vk.Dvc.CreateInfo {
		Vk.Dvc.createInfoNext = Nothing,
		Vk.Dvc.createInfoFlags = def,
		Vk.Dvc.createInfoQueueCreateInfos = Singleton $ queueInfo qFam,
		Vk.Dvc.createInfoEnabledLayerNames =
			[Vk.Khr.validationLayerName],
		Vk.Dvc.createInfoEnabledExtensionNames = [],
		Vk.Dvc.createInfoEnabledFeatures = Nothing }
	queueInfo qFam = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = Nothing,
		Vk.Dvc.queueCreateInfoFlags = def,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qFam,
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
	'[ 'Vk.DscSetLyt.Buffer '[ 'List w1 "", 'List w2 "", 'List w3 ""]]
dscSetLayoutInfo = Vk.DscSetLyt.CreateInfo {
	Vk.DscSetLyt.createInfoNext = Nothing,
	Vk.DscSetLyt.createInfoFlags = def,
	Vk.DscSetLyt.createInfoBindings = binding0 :...: HVNil }

binding0 :: Vk.DscSetLyt.Binding ('Vk.DscSetLyt.Buffer objs)
binding0 = Vk.DscSetLyt.BindingBuffer {
	Vk.DscSetLyt.bindingBufferDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSetLyt.bindingBufferStageFlags = Vk.ShaderStageComputeBit }

prepareMems ::
	forall bts w1 w2 w3 sd sl a . (
	Storable w1, Storable w2, Storable w3
	) =>
	Vk.DscSet.BindingAndArrayElem bts '[ 'List w1 "", 'List w2 "", 'List w3 ""] =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.DscSetLyt.L sl bts ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (forall s sm1 sm2 sm3 .
		Vk.DscSet.S sd s '(sl, bts) ->
		Vk.Dvc.Mem.Buffer.M sm1 '[ '[ 'List w1 ""]] ->
		Vk.Dvc.Mem.Buffer.M sm2 '[ '[ 'List w2 ""]] ->
		Vk.Dvc.Mem.Buffer.M sm3 '[ '[ 'List w3 ""]] -> IO a) -> IO a
prepareMems phdvc dvc dscSetLyt da db dc f =
	Vk.DscPool.create dvc dscPoolInfo nil nil \dscPool ->
	Vk.DscSet.allocateSs dvc (dscSetInfo dscPool dscSetLyt)
		>>= \(dscSet :...: HVNil) ->
	storageBufferNew3' dvc phdvc da db dc \ba ma bb mb bc mc ->
	Vk.DscSet.updateDs @() @() dvc (Vk.DscSet.Write_
		(writeDscSet @w1 @w2 @w3 dscSet ba bb bc) :...: HVNil) [] >>
	f dscSet ma mb mc

prepareMems' ::
	forall bts w1 w2 w3 sd sl a . (
	Storable w1, Storable w2, Storable w3,
	Vk.Dvc.Mem.Buffer.OffsetSize
		('List w2 "") '[ '[ 'List w1 ""], '[ 'List w2 ""], '[ 'List w3 ""]],
	Vk.Dvc.Mem.Buffer.OffsetSize
		('List w3 "") '[ '[ 'List w1 ""], '[ 'List w2 ""], '[ 'List w3 ""]],
	Vk.DscSet.BindingAndArrayElem bts '[ 'List w1 "", 'List w2 "", 'List w3 ""] ) =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.DscSetLyt.L sl bts ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (forall s sm .
		Vk.DscSet.S sd s '(sl, bts) ->
		Vk.Dvc.Mem.Buffer.M sm '[ '[ 'List w1 ""], '[ 'List w2 ""], '[ 'List w3 ""]] -> IO a) -> IO a
prepareMems' phdvc dvc dscSetLyt da db dc f =
	Vk.DscPool.create dvc dscPoolInfo nil nil \dscPool ->
	Vk.DscSet.allocateSs dvc (dscSetInfo dscPool dscSetLyt)
		>>= \(dscSet :...: HVNil) ->
	storage3BufferNew dvc phdvc da db dc \ba bb bc m ->
	Vk.DscSet.updateDs @() @() dvc (Vk.DscSet.Write_
		(writeDscSet @w1 @w2 @w3 dscSet ba bb bc) :...: HVNil) [] >>
	f dscSet m

prepareMems'' :: forall w1 w2 w3 sd sl bts a . (
	Storable w1, Storable w2, Storable w3,
	Offset ('List w2 "") '[ 'List w1 "", 'List w2 "", 'List w3 "" ],
	Offset ('List w3 "") '[ 'List w1 "", 'List w2 "", 'List w3 "" ],
	Vk.Dvc.Mem.Buffer.OffsetSize
		('List w2 "") '[ '[ 'List w1 "", 'List w2 "", 'List w3 ""]],
	Vk.Dvc.Mem.Buffer.OffsetSize
		('List w3 "") '[ '[ 'List w1 "", 'List w2 "", 'List w3 ""]],
	Vk.DscSet.BindingAndArrayElem bts '[ 'List w1 "", 'List w2 "", 'List w3 ""] ) =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.DscSetLyt.L sl bts ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (forall s sm .
		Vk.DscSet.S sd s '(sl, bts) ->
		Vk.Dvc.Mem.Buffer.M sm '[ '[ 'List w1 "", 'List w2 "", 'List w3 ""]] -> IO a) -> IO a
prepareMems'' phdvc dvc dscSetLyt da db dc f =
	Vk.DscPool.create dvc dscPoolInfo nil nil \dscPool ->
	Vk.DscSet.allocateSs dvc (dscSetInfo dscPool dscSetLyt)
		>>= \(dscSet :...: HVNil) ->
	storage1BufferNew dvc phdvc da db dc \b m ->
	Vk.DscSet.updateDs @() @() dvc (Vk.DscSet.Write_
		(writeDscSet' @w1 @w2 @w3 dscSet b) :...: HVNil) [] >>
	f dscSet m

storageBufferNew3' :: (Storable w1, Storable w2, Storable w3) =>
	Vk.Dvc.D sd -> Vk.PhDvc.P ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (
		forall sb1 sm1 sb2 sm2 sb3 sm3 .
		Vk.Buffer.Binded sm1 sb1 nm1 '[ 'List w1 ""] ->
		Vk.Dvc.Mem.Buffer.M sm1 '[ '[ 'List w1 ""]] ->
		Vk.Buffer.Binded sm2 sb2 nm2 '[ 'List w2 ""] ->
		Vk.Dvc.Mem.Buffer.M sm2 '[ '[ 'List w2 ""]] ->
		Vk.Buffer.Binded sm3 sb3 nm3 '[ 'List w3 ""] ->
		Vk.Dvc.Mem.Buffer.M sm3 '[ '[ 'List w3 ""]] -> IO a ) -> IO a
storageBufferNew3' dvc phdvc x y z f =
	storageBufferNews dvc phdvc (x :...: y :...: z :...: HVNil) $ addArg3 f

addArg3 :: (forall sb1 sm1 sb2 sm2 sb3 sm3 .
	Vk.Buffer.Binded sm1 sb1 nm1 '[ 'List w1 ""] ->
	Vk.Dvc.Mem.Buffer.M sm1 '[ '[ 'List w1 ""]] ->
	Vk.Buffer.Binded sm2 sb2 nm2 '[ 'List w2 ""] ->
	Vk.Dvc.Mem.Buffer.M sm2 '[ '[ 'List w2 ""]] ->
	Vk.Buffer.Binded sm3 sb3 nm3 '[ 'List w3 ""] ->
	Vk.Dvc.Mem.Buffer.M sm3 '[ '[ 'List w3 ""]] -> r) ->
	Arg nm1 w1 (Arg nm2 w2 (Arg nm3 w3 r))
addArg3 f = Arg \b1 m1 -> Arg \b2 m2 -> Arg \b3 m3 -> f b1 m1 b2 m2 b3 m3

class StorageBufferNews f a where
	type Vectors f :: [Type]
	storageBufferNews :: Vk.Dvc.D sd -> Vk.PhDvc.P ->
		HeteroVarList V.Vector (Vectors f) -> f -> IO a

data Arg nm w f = Arg (forall sb sm .
	Vk.Buffer.Binded sm sb nm '[ 'List w ""] ->
	Vk.Dvc.Mem.Buffer.M sm '[ '[ 'List w ""]] -> f)

instance StorageBufferNews (IO a) a where
	type Vectors (IO a) = '[]
	storageBufferNews _dvc _phdvc HVNil f = f

instance (Storable w, StorageBufferNews f a) =>
	StorageBufferNews (Arg nm w f) a where
	type Vectors (Arg nm w f) = w ': Vectors f
	storageBufferNews dvc phdvc (vs :...: vss) (Arg f) =
		storageBufferNew dvc phdvc vs \buf mem ->
		storageBufferNews @f @a dvc phdvc vss (f buf mem)

storageBufferNew :: forall sd nm w a . Storable w =>
	Vk.Dvc.D sd -> Vk.PhDvc.P -> V.Vector w -> (
		forall sb sm .
		Vk.Buffer.Binded sm sb nm '[ 'List w ""]  ->
		Vk.Dvc.Mem.Buffer.M sm '[ '[ 'List w ""]] -> IO a ) -> IO a
storageBufferNew dvc phdvc xs f =
	Vk.Buffer.create dvc (bufferInfo xs) nil nil \buffer -> do
		memoryInfo <- getMemoryInfo phdvc dvc buffer
		Vk.Buffer.allocateBind dvc (V3 buffer :...: HVNil) memoryInfo
			nil nil \(V3 binded :...: HVNil) memory -> do
			Vk.Dvc.Mem.Buffer.write @('List w "") dvc memory def xs
			f binded memory

storage3BufferNew :: forall sd nm1 nm2 nm3 w1 w2 w3 a . (
	Storable w1, Storable w2, Storable w3,
	Vk.Dvc.Mem.Buffer.OffsetSize ('List w2 "") '[ '[ 'List w1 ""], '[ 'List w2 ""] , '[ 'List w3 ""]],
	Vk.Dvc.Mem.Buffer.OffsetSize ('List w3 "") '[ '[ 'List w1 ""], '[ 'List w2 ""], '[ 'List w3 ""]]
	) =>
	Vk.Dvc.D sd -> Vk.PhDvc.P ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (
		forall sb1 sb2 sb3 sm .
		Vk.Buffer.Binded sm sb1 nm1 '[ 'List w1 ""] ->
		Vk.Buffer.Binded sm sb2 nm2 '[ 'List w2 ""] ->
		Vk.Buffer.Binded sm sb3 nm3 '[ 'List w3 ""] ->
		Vk.Dvc.Mem.Buffer.M sm
			'[ '[ 'List w1 ""], '[ 'List w2 ""], '[ 'List w3 ""]] -> IO a
		) -> IO a
storage3BufferNew dvc phdvc xs ys zs f =
	Vk.Buffer.create dvc (bufferInfo xs) nil nil \buf1 -> do
		memInfo1 <- getMemoryInfo phdvc dvc buf1
		Vk.Buffer.create dvc (bufferInfo ys) nil nil \buf2 -> do
			memInfo2 <- getMemoryInfo phdvc dvc buf2
			Vk.Buffer.create dvc (bufferInfo zs) nil nil \buf3 -> do
				memInfo3 <- getMemoryInfo phdvc dvc buf3
				if (memInfo1 == memInfo2 && memInfo2 == memInfo3) then
					Vk.Buffer.allocateBind dvc (
						V3 buf1 :...: V3 buf2 :...:
						V3 buf3 :...: HVNil
						) memInfo1 nil nil
						\(	V3 bnd1 :...:
							V3 bnd2 :...:
							V3 bnd3 :...: HVNil ) mem -> do
						Vk.Dvc.Mem.Buffer.write @('List w1 "") dvc mem def xs
						Vk.Dvc.Mem.Buffer.write @('List w2 "") dvc mem def ys
						Vk.Dvc.Mem.Buffer.write @('List w3 "") dvc mem def zs
						f bnd1 bnd2 bnd3 mem
					else error "bad"

bufferInfo :: Storable w => V.Vector w -> Vk.Buffer.CreateInfo () '[ 'List w ""]
bufferInfo xs = Vk.Buffer.CreateInfo {
	Vk.Buffer.createInfoNext = Nothing,
	Vk.Buffer.createInfoFlags = def,
	Vk.Buffer.createInfoLengths =
		ObjectLengthList (V.length xs) :...: HVNil,
	Vk.Buffer.createInfoUsage = Vk.Buffer.UsageStorageBufferBit,
	Vk.Buffer.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Buffer.createInfoQueueFamilyIndices = [] }

storage1BufferNew :: forall sd nm w1 w2 w3 a . (
	Storable w1, Storable w2, Storable w3,
	Vk.Dvc.Mem.Buffer.OffsetSize
		('List w2 "") '[ '[ 'List w1 "", 'List w2 "", 'List w3 ""]],
	Vk.Dvc.Mem.Buffer.OffsetSize
		('List w3 "") '[ '[ 'List w1 "", 'List w2 "", 'List w3 ""]] ) =>
	Vk.Dvc.D sd -> Vk.PhDvc.P ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (
		forall sb sm .
		Vk.Buffer.Binded sm sb nm '[ 'List w1 "", 'List w2 "", 'List w3 ""] ->
		Vk.Dvc.Mem.Buffer.M sm
			'[ '[ 'List w1 "", 'List w2 "", 'List w3 ""]] -> IO a) -> IO a
storage1BufferNew dvc phdvc xs ys zs f =
	Vk.Buffer.create dvc (bufferInfo' xs ys zs) nil nil \buf -> do
		memInfo <- getMemoryInfo phdvc dvc buf
		Vk.Buffer.allocateBind dvc (V3 buf :...: HVNil)
			memInfo nil nil \(V3 bnd :...: HVNil) mem -> do
			Vk.Dvc.Mem.Buffer.write @('List w1 "") dvc mem def xs
			Vk.Dvc.Mem.Buffer.write @('List w2 "") dvc mem def ys
			Vk.Dvc.Mem.Buffer.write @('List w3 "") dvc mem def zs
			f bnd mem

bufferInfo' :: (
	Storable w1, Storable w2, Storable w3 ) =>
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 ->
	Vk.Buffer.CreateInfo () '[ 'List w1 "", 'List w2 "", 'List w3 ""]
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

pplLayoutInfo :: Vk.DscSetLyt.L sl bts -> Vk.Ppl.Lyt.CreateInfo () '[ '(sl, bts)]
pplLayoutInfo dsl = Vk.Ppl.Lyt.CreateInfo {
	Vk.Ppl.Lyt.createInfoNext = Nothing,
	Vk.Ppl.Lyt.createInfoFlags = def,
	Vk.Ppl.Lyt.createInfoSetLayouts = Vk.Ppl.Lyt.Layout dsl :...: HVNil,
	Vk.Ppl.Lyt.createInfoPushConstantRanges = [] }

computePipelineInfo :: Vk.Ppl.Lyt.LL sl sbtss ->
	Vk.Ppl.Cmpt.CreateInfo () () () () () (Word32 :.: Word32 :.: ()) sl sbtss sbph
computePipelineInfo pl = Vk.Ppl.Cmpt.CreateInfo {
			Vk.Ppl.Cmpt.createInfoNext = Nothing,
			Vk.Ppl.Cmpt.createInfoFlags = def,
			Vk.Ppl.Cmpt.createInfoStage = shaderStageInfo,
			Vk.Ppl.Cmpt.createInfoLayout = pl,
			Vk.Ppl.Cmpt.createInfoBasePipelineHandle = Nothing,
			Vk.Ppl.Cmpt.createInfoBasePipelineIndex = Nothing }

commandPoolInfo :: Vk.QFam.Index -> Vk.CommandPool.CreateInfo ()
commandPoolInfo qFam = Vk.CommandPool.CreateInfo {
	Vk.CommandPool.createInfoNext = Nothing,
	Vk.CommandPool.createInfoFlags =
		Vk.CommandPool.CreateResetCommandBufferBit,
	Vk.CommandPool.createInfoQueueFamilyIndex = qFam }

dscSetInfo :: Vk.DscPool.P sp -> Vk.DscSetLyt.L sl bts ->
	Vk.DscSet.AllocateInfo () sp '[ '(sl, bts)]
dscSetInfo pl lyt = Vk.DscSet.AllocateInfo {
	Vk.DscSet.allocateInfoNext = Nothing,
	Vk.DscSet.allocateInfoDescriptorPool = pl,
	Vk.DscSet.allocateInfoSetLayouts = Vk.DscSet.Layout lyt :...: HVNil }

commandBufferInfo :: Vk.CommandPool.C s -> Vk.CmdBuf.AllocateInfo () s
commandBufferInfo cmdPool = Vk.CmdBuf.AllocateInfo {
	Vk.CmdBuf.allocateInfoNext = Nothing,
	Vk.CmdBuf.allocateInfoCommandPool = cmdPool,
	Vk.CmdBuf.allocateInfoLevel = Vk.CmdBuf.LevelPrimary,
	Vk.CmdBuf.allocateInfoCommandBufferCount = 1 }

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
	requirements <- Vk.Buffer.getMemoryRequirements dvc buffer
	memTypeIdx <- findMemoryTypeIndex phdvc requirements (
		Vk.Mem.PropertyHostVisibleBit .|.
		Vk.Mem.PropertyHostCoherentBit )
	pure Vk.Dvc.Mem.Buffer.AllocateInfo {
		Vk.Dvc.Mem.Buffer.allocateInfoNext = Nothing,
		Vk.Dvc.Mem.Buffer.allocateInfoMemoryTypeIndex = memTypeIdx }

findMemoryTypeIndex ::
	Vk.PhDvc.P -> Vk.Mem.M.Requirements -> Vk.Mem.PropertyFlags ->
	IO Vk.Mem.TypeIndex
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
	forall w1 w2 w3 sd sp slbts sb1 sb2 sb3 sm1 sm2 sm3 nm1 nm2 nm3 objs1 objs2 objs3 .
	Vk.DscSet.S sd sp slbts ->
	Vk.Buffer.Binded sm1 sb1 nm1 objs1 -> Vk.Buffer.Binded sm2 sb2 nm2 objs2 ->
	Vk.Buffer.Binded sm3 sb3 nm3 objs3 ->
	Vk.DscSet.Write () sd sp slbts ('Vk.DscSet.WriteSourcesArgBuffer '[
		'(sb1, sm1, nm1, objs1, 'List w1 ""), '(sb2, sm2, nm2, objs2, 'List w2 ""),
		'(sb3, sm3, nm3, objs3, 'List w3 "") ])
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
		'(sb, sm, nm, objs, 'List w1 ""), '(sb, sm, nm, objs, 'List w2 ""),
		'(sb, sm, nm, objs, 'List w3 "") ])
writeDscSet' ds b = Vk.DscSet.Write {
	Vk.DscSet.writeNext = Nothing,
	Vk.DscSet.writeDstSet = ds,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		bufferInfoList @w1 b :...: bufferInfoList @w2 b :...:
		bufferInfoList @w3 b :...: HVNil }

bufferInfoList :: forall t {sb} {sm} {nm} {objs} .
	Vk.Buffer.Binded sm sb nm objs ->
	Vk.Dsc.BufferInfo '(sb, sm, nm, objs, 'List t "")
bufferInfoList = Vk.Dsc.BufferInfoList

type BindedMem sm sb nm w = (
	Vk.Buffer.Binded sm sb nm '[ 'List w ""],
	Vk.Dvc.Mem.Buffer.M sm '[ '[ 'List w ""]] )

type BindedMem3 sb1 sm1 nm1 w1 sb2 sm2 nm2 w2 sb3 sm3 nm3 w3 =
	(BindedMem sm1 sb1 nm1 w1, BindedMem sm2 sb2 nm2 w2, BindedMem sm3 sb3 nm3 w3)

shaderStageInfo :: Vk.Ppl.ShaderSt.CreateInfo () () 'GlslComputeShader () () (Word32 :.: Word32 :.: ())
shaderStageInfo = Vk.Ppl.ShaderSt.CreateInfo {
	Vk.Ppl.ShaderSt.createInfoNext = Nothing,
	Vk.Ppl.ShaderSt.createInfoFlags = def,
	Vk.Ppl.ShaderSt.createInfoStage = Vk.ShaderStageComputeBit,
	Vk.Ppl.ShaderSt.createInfoModule = Vk.ShaderMod.M shaderModInfo nil nil,
	Vk.Ppl.ShaderSt.createInfoName = "main",
	Vk.Ppl.ShaderSt.createInfoSpecializationInfo = Just $ 3 :.: 10 :.: () }
	where shaderModInfo = Vk.ShaderMod.CreateInfo {
		Vk.ShaderMod.createInfoNext = Nothing,
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
