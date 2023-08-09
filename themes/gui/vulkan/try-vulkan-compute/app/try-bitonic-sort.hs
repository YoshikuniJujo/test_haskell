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

module Main (main, maximumExponentOf2) where

import qualified Gpu.Vulkan.Memory as Vk.Mem

import Foreign.Storable
import Gpu.Vulkan.Object qualified as VObj
import Gpu.Vulkan.Object.Base qualified as KObj
import Data.Default
import Data.Bits
import Data.Bool
import Data.List.Length
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.List
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Word

import qualified Data.Vector.Storable as V

import Language.SpirV.Shaderc.TH
import Language.SpirV.ShaderKind
import Gpu.Vulkan.Misc

import qualified Gpu.Vulkan as Vk
import qualified "try-gpu-vulkan" Gpu.Vulkan.Enum as Vk
import qualified Gpu.Vulkan.Instance as Vk.Inst
import qualified Gpu.Vulkan.PhysicalDevice as Vk.PhDvc
import qualified Gpu.Vulkan.PhysicalDevice.Struct as Vk.PhDvc
import qualified Gpu.Vulkan.Queue as Vk.Queue
import qualified Gpu.Vulkan.Queue.Enum as Vk.Queue
import qualified Gpu.Vulkan.QueueFamily as Vk.QFam
import qualified Gpu.Vulkan.Device as Vk.Dvc
import qualified Gpu.Vulkan.CommandPool as Vk.CmdPool
import qualified "try-gpu-vulkan" Gpu.Vulkan.CommandPool.Enum as Vk.CmdPool
import qualified "try-gpu-vulkan" Gpu.Vulkan.Buffer.Enum as Vk.Buffer
import qualified Gpu.Vulkan.Memory as Vk.Mm
import qualified Gpu.Vulkan.Memory.Enum as Vk.Mm
import qualified Gpu.Vulkan.Memory as Vk.Mm.M
import qualified Gpu.Vulkan.Descriptor as Vk.Dsc
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

import qualified Gpu.Vulkan.Khr as Vk.Khr
import qualified Gpu.Vulkan.PushConstant as Vk.PushConstant

import System.Random

import Gpu.Vulkan.Semaphore qualified as Vk.Semaphore
import Gpu.Vulkan.Fence qualified as Vk.Fence
import Gpu.Vulkan.Pipeline.Enum qualified as Vk.Ppl

import Data.List qualified as L

import Data.IORef
import System.IO.Unsafe

import Control.Concurrent

import Data.Time

---------------------------------------------------------------------------

-- MAIN
-- PREPARE MEMORIES
-- CALC
-- COMPUTE PIPELINE INFO

---------------------------------------------------------------------------

-- MAIN

getRandoms :: Random a => Int -> IO [a]
getRandoms n = take n . randoms <$> getStdGen

getRandomRs :: Random a => (a, a) -> Int -> IO [a]
getRandomRs r n = take n . randomRs r <$> getStdGen

main :: IO ()
main = withDevice \phdvc qFam dvc mgcx -> do
	let	eot :: Integral n => n
		eot = maximumExponentOf2 mgcx
		pot :: Integral n => n
		pot = 2 ^ eot
	rs <- getRandomRs (1, 1000000) (pot * 2 ^ 5)
	let	pss = bitonicSortPairs False 0 (eot + 5)
		das@(da : _) = V.fromList . (W1 . fst <$>) <$> pss
		dbs@(db : _) = V.fromList . (W2 . snd <$>) <$> pss
		dc = V.fromList $ W3 <$> rs
	print mgcx
	print eot
	print pot
	print $ map V.length das
--	print $ map V.length dbs
	ct0 <- getCurrentTime
	(r1, r2, r3) <-
		Vk.DscSetLyt.create dvc dscSetLayoutInfo nil' \dscSetLyt ->
		prepareMems phdvc dvc dscSetLyt da db dc \dscSet ma mb mc ->
		calc dvc qFam dscSetLyt dscSet ma mb das dbs pot >> threadDelay 1000000 >>
		(,,)	<$> Vk.Mm.read @"" @(VObj.List 256 W1 "") @[W1] dvc ma def
			<*> Vk.Mm.read @"" @(VObj.List 256 W2 "") @[W2] dvc mb def
			<*> Vk.Mm.read @"" @(VObj.List 256 W3 "") @[W3] dvc mc def
	print . take 20 $ unW1 <$> r1
	print . take 20 $ unW2 <$> r2
	print . take 20 $ unW3 <$> r3
	print . checkSorted 0 $ unW3 <$> r3
	ct1 <- getCurrentTime
	print $ diffUTCTime ct1 ct0

checkSorted :: Ord a => Int -> [a] -> (Int, Bool, [a])
checkSorted i [_] = (i, True, [])
checkSorted i (x : xs@(y : _))
	| x <= y = checkSorted (i + 1) xs
	| otherwise = (i, False, [x, y])

maximumExponentOf2 :: Integral n => n -> n
maximumExponentOf2 n | n < 2 = 0
maximumExponentOf2 n = 1 + maximumExponentOf2 (n `div` 2)

newtype W1 = W1 { unW1 :: Word32 } deriving (Show, Storable)
newtype W2 = W2 { unW2 :: Word32 } deriving (Show, Storable)
newtype W3 = W3 { unW3 :: Word32 } deriving (Show, Storable)

withDevice ::
	(forall sd . Vk.PhDvc.P -> Vk.QFam.Index -> Vk.Dvc.D sd ->
	(forall c . Integral c => c) -> IO a) -> IO a
withDevice f = Vk.Inst.create @_ @'Nothing instInfo nil' \inst -> do
	phdvc <- head <$> Vk.PhDvc.enumerate inst
	qFam <- fst . head . filter (
			(/= zeroBits) . (.&. Vk.Queue.ComputeBit)
				. Vk.QFam.propertiesQueueFlags . snd )
		<$> Vk.PhDvc.getQueueFamilyProperties phdvc
	mgcx :. mgcy :. mgcz :. _ <- Vk.PhDvc.limitsMaxComputeWorkGroupCount
		. Vk.PhDvc.propertiesLimits <$> Vk.PhDvc.getProperties phdvc
	print mgcx
	print mgcy
	print mgcz
	Vk.Dvc.create phdvc (dvcInfo qFam) nil' $ \dvc ->
		f phdvc qFam dvc (fromIntegral mgcx)

instInfo :: Vk.Inst.CreateInfo 'Nothing 'Nothing
instInfo = def {
	Vk.Inst.createInfoEnabledLayerNames = [Vk.Khr.validationLayerName] }
	
dvcInfo :: Vk.QFam.Index -> Vk.Dvc.CreateInfo 'Nothing '[ 'Nothing]
dvcInfo qFam = Vk.Dvc.CreateInfo {
	Vk.Dvc.createInfoNext = TMaybe.N,
	Vk.Dvc.createInfoFlags = def,
	Vk.Dvc.createInfoQueueCreateInfos = HeteroParList.Singleton queueInfo,
	Vk.Dvc.createInfoEnabledLayerNames = [Vk.Khr.validationLayerName],
	Vk.Dvc.createInfoEnabledExtensionNames = [],
	Vk.Dvc.createInfoEnabledFeatures = Nothing }
	where queueInfo = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = def,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qFam,
		Vk.Dvc.queueCreateInfoQueuePriorities = [0] }

dscSetLayoutInfo :: Vk.DscSetLyt.CreateInfo 'Nothing '[
	'Vk.DscSetLyt.Buffer '[VObj.List 256 W1 "",VObj.List 256 W2 "",VObj.List 256 W3 ""] ]
dscSetLayoutInfo = Vk.DscSetLyt.CreateInfo {
	Vk.DscSetLyt.createInfoNext = TMaybe.N,
	Vk.DscSetLyt.createInfoFlags = zeroBits,
	Vk.DscSetLyt.createInfoBindings = HeteroParList.Singleton binding }

binding :: Vk.DscSetLyt.Binding ('Vk.DscSetLyt.Buffer objs)
binding = Vk.DscSetLyt.BindingBuffer {
	Vk.DscSetLyt.bindingBufferDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSetLyt.bindingBufferStageFlags = Vk.ShaderStageComputeBit }

-- PREPARE MEMORIES

prepareMems :: (
	Default (HeteroParList.PL
		(HeteroParList.PL KObj.Length)
		(Vk.DscSetLyt.BindingTypeListBufferOnlyDynamics bts)),
	Vk.DscSet.BindingAndArrayElemBuffer bts '[
		VObj.List 256 W1 "",VObj.List 256 W2 "",VObj.List 256 W3 "" ] 0,
	Vk.DscSet.UpdateDynamicLength bts '[
		VObj.List 256 W1 "",VObj.List 256 W2 "",VObj.List 256 W3 "" ]
	) =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.DscSetLyt.D sl bts ->
	V.Vector W1 -> V.Vector W2 -> V.Vector W3 -> (forall sds sm1 sb1 sm2 sb2 sm3 sb3 .
		Vk.DscSet.D sds '(sl, bts) ->
		Vk.Mm.M sm1 '[ '( sb1, 'Vk.Mm.BufferArg "" '[VObj.List 256 W1 ""])] ->
		Vk.Mm.M sm2 '[ '( sb2, 'Vk.Mm.BufferArg "" '[VObj.List 256 W2 ""])] ->
		Vk.Mm.M sm3 '[ '( sb3, 'Vk.Mm.BufferArg "" '[VObj.List 256 W3 ""])] -> IO a) -> IO a
prepareMems phdvc dvc dscSetLyt da db dc f =
	Vk.DscPool.create dvc dscPoolInfo nil' \dscPool ->
	Vk.DscSet.allocateDs dvc (dscSetInfo dscPool dscSetLyt)
		\(HeteroParList.Singleton dscSet) ->
	storageBufferNew dvc phdvc da \ba ma ->
	storageBufferNew dvc phdvc db \bb mb ->
	storageBufferNew dvc phdvc dc \bc mc ->
	Vk.DscSet.updateDs dvc
		(HeteroParList.Singleton . U5 $ writeDscSet dscSet ba bb bc)
		HeteroParList.Nil >>
	f dscSet ma mb mc

dscPoolInfo :: Vk.DscPool.CreateInfo 'Nothing
dscPoolInfo = Vk.DscPool.CreateInfo {
	Vk.DscPool.createInfoNext = TMaybe.N,
	Vk.DscPool.createInfoFlags = Vk.DscPool.CreateFreeDescriptorSetBit,
	Vk.DscPool.createInfoMaxSets = 1,
	Vk.DscPool.createInfoPoolSizes = [poolSize] }
	where poolSize = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeStorageBuffer,
		Vk.DscPool.sizeDescriptorCount = 10 }

dscSetInfo :: Vk.DscPool.P sp -> Vk.DscSetLyt.D sl bts ->
	Vk.DscSet.AllocateInfo 'Nothing sp '[ '(sl, bts)]
dscSetInfo pl lyt = Vk.DscSet.AllocateInfo {
	Vk.DscSet.allocateInfoNext = TMaybe.N,
	Vk.DscSet.allocateInfoDescriptorPool = pl,
	Vk.DscSet.allocateInfoSetLayouts =
		HeteroParList.Singleton $ U2 lyt }

storageBufferNew :: forall sd nm w a . Storable w =>
	Vk.Dvc.D sd -> Vk.PhDvc.P -> V.Vector w -> (forall sb sm .
		Vk.Buffer.Binded sm sb nm '[VObj.List 256 w ""]  ->
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[VObj.List 256 w ""])] ->
		IO a) -> IO a
storageBufferNew dvc phdvc xs f =
	Vk.Buffer.create dvc (bufferInfo xs) nil' \bf ->
	getMemoryInfo phdvc dvc bf >>= \mmi ->
	Vk.Mm.allocateBind dvc
		(HeteroParList.Singleton . U2 $ Vk.Mm.Buffer bf) mmi nil'
		\(HeteroParList.Singleton (U2 (Vk.Mm.BufferBinded bnd))) mm ->
	f bnd mm

bufferInfo :: Storable w => V.Vector w -> Vk.Buffer.CreateInfo 'Nothing '[VObj.List 256 w ""]
bufferInfo xs = Vk.Buffer.CreateInfo {
	Vk.Buffer.createInfoNext = TMaybe.N,
	Vk.Buffer.createInfoFlags = def,
	Vk.Buffer.createInfoLengths =
		VObj.LengthList (fromIntegral $ V.length xs) :** HeteroParList.Nil,
	Vk.Buffer.createInfoUsage = Vk.Buffer.UsageStorageBufferBit,
	Vk.Buffer.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Buffer.createInfoQueueFamilyIndices = [] }

getMemoryInfo :: Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Buffer.B sb nm objs ->
	IO (Vk.Mem.AllocateInfo 'Nothing)
getMemoryInfo phdvc dvc buffer = do
	rqs <- Vk.Buffer.getMemoryRequirements dvc buffer
	mti <- findMemoryTypeIndex phdvc rqs
		$ Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit
	pure Vk.Mem.AllocateInfo {
		Vk.Mem.allocateInfoNext = TMaybe.N,
		Vk.Mem.allocateInfoMemoryTypeIndex = mti }

findMemoryTypeIndex ::
	Vk.PhDvc.P -> Vk.Mm.M.Requirements -> Vk.Mm.PropertyFlags ->
	IO Vk.Mm.M.TypeIndex
findMemoryTypeIndex phdvc rqs prp0 = do
	memoryProperties <- Vk.PhDvc.getMemoryProperties phdvc
	let	reqTypes = Vk.Mm.M.requirementsMemoryTypeBits rqs
		memPropTypes = (fst <$>)
			. filter (checkBits prp0
				. Vk.Mm.M.mTypePropertyFlags . snd)
			$ Vk.PhDvc.memoryPropertiesMemoryTypes memoryProperties
	case filter (`Vk.Mm.M.elemTypeIndex` reqTypes) memPropTypes of
		[] -> error "No available memory types"
		i : _ -> pure i

checkBits :: Bits bs => bs -> bs -> Bool
checkBits bs0 = (== bs0) . (.&. bs0)

writeDscSet ::
	forall slbts sb1 sb2 sb3 sm1 sm2 sm3 objs1 objs2 objs3 sds . (
	Show (HeteroParList.PL VObj.Length objs1),
	Show (HeteroParList.PL VObj.Length objs2),
	Show (HeteroParList.PL VObj.Length objs3),
	VObj.OffsetRange (VObj.List 256 W1 "") objs1,
	VObj.OffsetRange (VObj.List 256 W2 "") objs2,
	VObj.OffsetRange (VObj.List 256 W3 "") objs3
	) =>
	Vk.DscSet.D sds slbts ->
	Vk.Buffer.Binded sm1 sb1 "" objs1 -> Vk.Buffer.Binded sm2 sb2 "" objs2 ->
	Vk.Buffer.Binded sm3 sb3 "" objs3 ->
	Vk.DscSet.Write 'Nothing sds slbts ('Vk.DscSet.WriteSourcesArgBuffer '[
		'(sm1, sb1, "", VObj.List 256 W1 ""), '(sm2, sb2, "", VObj.List 256 W2 ""),
		'(sm3, sb3, "", VObj.List 256 W3 "") ]) 0
writeDscSet ds ba bb bc = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = ds,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		U4 (Vk.Dsc.BufferInfo @_ @_ @_ @(VObj.List 256 W1 "") ba) :**
		U4 (Vk.Dsc.BufferInfo @_ @_ @_ @(VObj.List 256 W2 "") bb) :**
		U4 (Vk.Dsc.BufferInfo @_ @_ @_ @(VObj.List 256 W3 "") bc) :**
		HeteroParList.Nil }

-- CALC

calc :: forall slbts sl bts sd sds sm1 sb1 sm2 sb2 . (
	slbts ~ '(sl, bts),
	Vk.DscSetLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[]],
	InfixIndex '[slbts] '[ '(sl, bts)]) =>
	Vk.Dvc.D sd -> Vk.QFam.Index -> Vk.DscSetLyt.D sl bts ->
	Vk.DscSet.D sds slbts ->
	Vk.Mm.M sm1 '[ '( sb1, 'Vk.Mm.BufferArg "" '[VObj.List 256 W1 ""])] ->
	Vk.Mm.M sm2 '[ '( sb2, 'Vk.Mm.BufferArg "" '[VObj.List 256 W2 ""])] ->
	[V.Vector W1] -> [V.Vector W2] ->
	Word32 -> IO ()
calc dvc qFam dscSetLyt dscSet ma mb das dbs dsz =
	Vk.Ppl.Lyt.create dvc (pplLayoutInfo dscSetLyt) nil' \plyt ->
	Vk.Ppl.Cmpt.createCs dvc Nothing
		(HeteroParList.Singleton . U4 $ computePipelineInfo plyt)
		nil' \(ppl :** HeteroParList.Nil) ->
	Vk.CmdPool.create dvc (commandPoolInfo qFam) nil' \cmdPool ->
	Vk.CmdBuf.allocateNew dvc (commandBufferInfoNew cmdPool) \cbs@(cb0 : cb1 : cb2 : cb3 : cb4 : cb5 : cb6 : _) ->
		putStrLn "BEGIN CALC" >>
		runAll dvc qFam ppl plyt dscSet dsz ma mb (L.zip3 cbs das dbs) \fnc ->
		Vk.Fence.waitForFs dvc (HeteroParList.Singleton fnc) True Nothing

runAll :: (
	sbtss ~ '[slbts],
	Vk.Cmd.LayoutArgListOnlyDynamics sbtss ~ '[ '[ '[]]] ) =>
	Vk.Dvc.D sd -> Vk.QFam.Index -> Vk.Ppl.Cmpt.C sg '(sl, sbtss, '[]) ->
	Vk.Ppl.Lyt.P sl sbtss '[] -> Vk.DscSet.D sds slbts -> Word32 ->
	Vk.Mm.M.M sm1 '[ '(sb1, 'Vk.Mm.M.BufferArg "" '[VObj.List 256 W1 ""])] ->
	Vk.Mm.M.M sm2 '[ '(sb2, 'Vk.Mm.M.BufferArg "" '[VObj.List 256 W2 ""])] ->
	[(Vk.CmdBuf.C sc,
		V.Vector W1, V.Vector W2)] ->
	(forall sf . Vk.Fence.F sf -> IO c) -> IO c
runAll dvc qFam ppl plyt dscSet dsz ma mb = repeatBeginEnd
	(writeAndRunBegin dvc qFam ppl plyt dscSet dsz ma mb)
	(writeAndRun dvc qFam ppl plyt dscSet dsz ma mb)
	(writeAndRunEnd dvc qFam ppl plyt dscSet dsz ma mb)

writeAndRunBegin :: (
	sbtss ~ '[slbts],
	Vk.Cmd.LayoutArgListOnlyDynamics sbtss ~ '[ '[ '[]]] ) =>
	Vk.Dvc.D sd -> Vk.QFam.Index -> Vk.Ppl.Cmpt.C sg '(sl, sbtss, '[]) ->
	Vk.Ppl.Lyt.P sl sbtss '[] -> Vk.DscSet.D sds slbts -> Word32 ->
	Vk.Mm.M sm1 '[ '( sb1, 'Vk.Mm.BufferArg "" '[VObj.List 256 W1 ""])] ->
	Vk.Mm.M sm2 '[ '( sb2, 'Vk.Mm.BufferArg "" '[VObj.List 256 W2 ""])] ->
	(	Vk.CmdBuf.C sc,
		V.Vector W1,  V.Vector W2 ) ->
	(forall ss' . Vk.Semaphore.S ss' -> IO b) -> IO b
writeAndRunBegin dvc qFam ppl plyt dscSet dsz ma mb (cb, da, db) f = do
	Vk.Mm.write @"" @(VObj.List 256 W1 "") dvc ma def da
	Vk.Mm.write @"" @(VObj.List 256 W2 "") dvc mb def db
	run dvc qFam cb ppl plyt dscSet dsz HeteroParList.Nil f

forDebug :: IORef Int
forDebug = unsafePerformIO $ newIORef 0

writeAndRun :: (
	sbtss ~ '[slbts],
	Vk.Cmd.LayoutArgListOnlyDynamics sbtss ~ '[ '[ '[]]] ) =>
	Vk.Dvc.D sd -> Vk.QFam.Index -> Vk.Ppl.Cmpt.C sg '(sl, sbtss, '[]) ->
	Vk.Ppl.Lyt.P sl sbtss '[] -> Vk.DscSet.D sds slbts -> Word32 ->
	Vk.Mm.M sm1 '[ '( sb1, 'Vk.Mm.BufferArg "" '[VObj.List 256 W1 ""])] ->
	Vk.Mm.M sm2 '[ '( sb2, 'Vk.Mm.BufferArg "" '[VObj.List 256 W2 ""])] ->
	Vk.Semaphore.S ss -> (
		Vk.CmdBuf.C sc,
		V.Vector W1,  V.Vector W2 ) ->
	(forall ss' . Vk.Semaphore.S ss' -> IO b) -> IO b
writeAndRun dvc qFam ppl plyt dscSet dsz ma mb s (cb, da, db) f = do
--	print =<< readIORef forDebug
--	modifyIORef forDebug (+ 1)
	Vk.Mm.write @"" @(VObj.List 256 W1 "") dvc ma def da
	Vk.Mm.write @"" @(VObj.List 256 W2 "") dvc mb def db
	run dvc qFam cb ppl plyt dscSet dsz (HeteroParList.Singleton s) f

writeAndRunEnd :: (
	sbtss ~ '[slbts],
	Vk.Cmd.LayoutArgListOnlyDynamics sbtss ~ '[ '[ '[]]] ) =>
	Vk.Dvc.D sd -> Vk.QFam.Index -> Vk.Ppl.Cmpt.C sg '(sl, sbtss, '[]) ->
	Vk.Ppl.Lyt.P sl sbtss '[] -> Vk.DscSet.D sds slbts -> Word32 ->
	Vk.Mm.M sm1 '[ '( sb1, 'Vk.Mm.BufferArg "" '[VObj.List 256 W1 ""])] ->
	Vk.Mm.M sm2 '[ '( sb2, 'Vk.Mm.BufferArg "" '[VObj.List 256 W2 ""])] ->
	Vk.Semaphore.S ss -> (
		Vk.CmdBuf.C sc,
		V.Vector W1,  V.Vector W2 ) ->
	(forall sf . Vk.Fence.F sf -> IO b) -> IO b
writeAndRunEnd dvc qFam ppl plyt dscSet dsz ma mb s (cb, da, db) f = do
	Vk.Mm.write @"" @(VObj.List 256 W1 "") dvc ma def da
	Vk.Mm.write @"" @(VObj.List 256 W2 "") dvc mb def db
	run' dvc qFam cb ppl plyt dscSet dsz (HeteroParList.Singleton s) f

repeatBeginEnd ::
	(forall b . a -> (forall ss . s ss -> IO b) -> IO b) ->
	(forall ss b . s ss -> a -> (forall ss' . s ss' -> IO b) -> IO b) ->
	(forall ss b . s ss -> a -> (forall st . t st -> IO b) -> IO b) -> [a] ->
	(forall st . t st -> IO c) -> IO c
repeatBeginEnd b m e (x : xs) f = b x \s -> repeatActions s xs m \s x -> e s x f

repeatActions :: Monad m =>
	s ss -> [a] -> (forall ss b . s ss -> a -> (forall ss' . s ss' -> m b) -> m b) -> (forall ss . s ss -> a -> m c) -> m c
repeatActions s [x] _ f = f s x
repeatActions s (x : xs) g f = g s x \s' -> repeatActions s' xs g f

pplLayoutInfo :: Vk.DscSetLyt.D sl bts ->
	Vk.Ppl.Lyt.CreateInfo 'Nothing '[ '(sl, bts)]
		('Vk.PushConstant.Layout '[] '[])
pplLayoutInfo dsl = Vk.Ppl.Lyt.CreateInfo {
	Vk.Ppl.Lyt.createInfoNext = TMaybe.N,
	Vk.Ppl.Lyt.createInfoFlags = zeroBits,
	Vk.Ppl.Lyt.createInfoSetLayouts = HeteroParList.Singleton $ U2 dsl }

commandPoolInfo :: Vk.QFam.Index -> Vk.CmdPool.CreateInfo 'Nothing
commandPoolInfo qFam = Vk.CmdPool.CreateInfo {
	Vk.CmdPool.createInfoNext = TMaybe.N,
	Vk.CmdPool.createInfoFlags = Vk.CmdPool.CreateResetCommandBufferBit,
	Vk.CmdPool.createInfoQueueFamilyIndex = qFam }

commandBufferInfo :: Vk.CmdPool.C s -> Vk.CmdBuf.AllocateInfo 'Nothing s '[ '()]
commandBufferInfo cmdPool = Vk.CmdBuf.AllocateInfo {
	Vk.CmdBuf.allocateInfoNext = TMaybe.N,
	Vk.CmdBuf.allocateInfoCommandPool = cmdPool,
	Vk.CmdBuf.allocateInfoLevel = Vk.CmdBuf.LevelPrimary }

commandBufferInfoNew :: Vk.CmdPool.C s -> Vk.CmdBuf.AllocateInfoNew 'Nothing s
commandBufferInfoNew cmdPool = Vk.CmdBuf.AllocateInfoNew {
	Vk.CmdBuf.allocateInfoNextNew = TMaybe.N,
	Vk.CmdBuf.allocateInfoCommandPoolNew = cmdPool,
	Vk.CmdBuf.allocateInfoLevelNew = Vk.CmdBuf.LevelPrimary,
--	Vk.CmdBuf.allocateInfoCommandBufferCountNew = 10000 }
--	Vk.CmdBuf.allocateInfoCommandBufferCountNew = 1000 }
	Vk.CmdBuf.allocateInfoCommandBufferCountNew = 300 }

run :: forall slbts sbtss sd sc sg sl sds swss a . (
	sbtss ~ '[slbts],
	Vk.Cmd.LayoutArgListOnlyDynamics sbtss ~ '[ '[ '[]]],
	InfixIndex '[slbts] sbtss ) =>
	Vk.Dvc.D sd -> Vk.QFam.Index -> Vk.CmdBuf.C sc -> Vk.Ppl.Cmpt.C sg '(sl, sbtss, '[]) ->
	Vk.Ppl.Lyt.P sl sbtss '[] -> Vk.DscSet.D sds slbts -> Word32 ->
	HeteroParList.PL Vk.Semaphore.S swss ->
	(forall ss . Vk.Semaphore.S ss -> IO a) -> IO a
run dvc qFam cb ppl pplLyt dscSet dsz ws f = do
	q <- Vk.Dvc.getQueue dvc qFam 0
	Vk.CmdBuf.begin @'Nothing @'Nothing cb def $
		Vk.Cmd.bindPipelineCompute cb Vk.Ppl.BindPointCompute ppl \ccb ->
			Vk.Cmd.bindDescriptorSetsCompute ccb
				pplLyt (HeteroParList.Singleton $ U2 dscSet)
				(HeteroParList.Singleton $ HeteroParList.Singleton HeteroParList.Nil ::
					HeteroParList.PL3 Vk.Cmd.DynamicIndex (Vk.Cmd.LayoutArgListOnlyDynamics sbtss)) >>
			Vk.Cmd.dispatch ccb dsz (2 ^ 5) 1
	Vk.Semaphore.create dvc Vk.Semaphore.CreateInfo {
		Vk.Semaphore.createInfoNext = TMaybe.N,
		Vk.Semaphore.createInfoFlags = zeroBits } nil' \s ->
		Vk.Queue.submit q (HeteroParList.Singleton . U4 $ submitInfo ws s) Nothing >> f s
	where
	submitInfo :: forall swss' ss .
		HeteroParList.PL Vk.Semaphore.S swss' ->
		Vk.Semaphore.S ss -> Vk.SubmitInfo 'Nothing swss' '[sc] '[ss]
	submitInfo wss s = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks =
			HeteroParList.map (`Vk.SemaphorePipelineStageFlags` Vk.Ppl.StageComputeShaderBit) wss,
		Vk.submitInfoCommandBuffers = HeteroParList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HeteroParList.Singleton s }

run' :: forall slbts sbtss sd sc sg sl sds swss a . (
	sbtss ~ '[slbts],
	Vk.Cmd.LayoutArgListOnlyDynamics sbtss ~ '[ '[ '[]]],
	InfixIndex '[slbts] sbtss ) =>
	Vk.Dvc.D sd -> Vk.QFam.Index -> Vk.CmdBuf.C sc -> Vk.Ppl.Cmpt.C sg '(sl, sbtss, '[]) ->
	Vk.Ppl.Lyt.P sl sbtss '[] -> Vk.DscSet.D sds slbts -> Word32 ->
	HeteroParList.PL Vk.Semaphore.S swss ->
	(forall sf . Vk.Fence.F sf -> IO a) -> IO a
run' dvc qFam cb ppl pplLyt dscSet dsz ws f = do
	q <- Vk.Dvc.getQueue dvc qFam 0
	Vk.CmdBuf.begin @'Nothing @'Nothing cb def $
		Vk.Cmd.bindPipelineCompute cb Vk.Ppl.BindPointCompute ppl \ccb ->
			Vk.Cmd.bindDescriptorSetsCompute ccb
				pplLyt (HeteroParList.Singleton $ U2 dscSet)
				(HeteroParList.Singleton $ HeteroParList.Singleton HeteroParList.Nil ::
					HeteroParList.PL3 Vk.Cmd.DynamicIndex (Vk.Cmd.LayoutArgListOnlyDynamics sbtss)) >>
			Vk.Cmd.dispatch ccb dsz (2 ^ 5) 1
	Vk.Fence.create dvc Vk.Fence.CreateInfo {
		Vk.Fence.createInfoNext = TMaybe.N,
		Vk.Fence.createInfoFlags = zeroBits } nil' \fnc ->
		Vk.Queue.submit q (HeteroParList.Singleton . U4 $ submitInfo ws) (Just fnc) >> f fnc
	where
	submitInfo :: forall swss' .
		HeteroParList.PL Vk.Semaphore.S swss' ->
		Vk.SubmitInfo 'Nothing swss' '[sc] '[]
	submitInfo wss = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks =
			HeteroParList.map (`Vk.SemaphorePipelineStageFlags` Vk.Ppl.StageComputeShaderBit) wss,
		Vk.submitInfoCommandBuffers = HeteroParList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HeteroParList.Nil }

-- COMPUTE PIPELINE INFO

computePipelineInfo :: Vk.Ppl.Lyt.P sl sbtss '[] ->
	Vk.Ppl.Cmpt.CreateInfo 'Nothing
		'( 'Nothing, 'Nothing, 'GlslComputeShader, 'Nothing, '[])
		'(sl, sbtss, '[]) sbph
computePipelineInfo pl = Vk.Ppl.Cmpt.CreateInfo {
	Vk.Ppl.Cmpt.createInfoNext = TMaybe.N,
	Vk.Ppl.Cmpt.createInfoFlags = zeroBits,
	Vk.Ppl.Cmpt.createInfoStage = U5 shaderStageInfo,
	Vk.Ppl.Cmpt.createInfoLayout = U3 pl,
	Vk.Ppl.Cmpt.createInfoBasePipelineHandleOrIndex = Nothing }

shaderStageInfo ::
	Vk.Ppl.ShaderSt.CreateInfo 'Nothing 'Nothing 'GlslComputeShader 'Nothing '[]
shaderStageInfo = Vk.Ppl.ShaderSt.CreateInfo {
	Vk.Ppl.ShaderSt.createInfoNext = TMaybe.N,
	Vk.Ppl.ShaderSt.createInfoFlags = def,
	Vk.Ppl.ShaderSt.createInfoStage = Vk.ShaderStageComputeBit,
	Vk.Ppl.ShaderSt.createInfoModule = (shdrMdInfo, nil'),
	Vk.Ppl.ShaderSt.createInfoName = "main",
	Vk.Ppl.ShaderSt.createInfoSpecializationInfo = Nothing }
	where shdrMdInfo = Vk.ShaderMod.CreateInfo {
		Vk.ShaderMod.createInfoNext = TMaybe.N,
		Vk.ShaderMod.createInfoFlags = zeroBits,
		Vk.ShaderMod.createInfoCode = glslComputeShaderMain }

bitonicSortPairs :: Integral i => Bool -> i -> i -> [[(i, i)]]
bitonicSortPairs _ _ 0 = []
bitonicSortPairs fl i n =
	zipWith (++)
		(bitonicSortPairs fl i (n - 1))
		(bitonicSortPairs (not fl) (i + 2 ^ (n - 1)) (n - 1)) ++
	bitonicMergePairs fl i n

bitonicMergePairs :: Integral i => Bool -> i -> i -> [[(i, i)]]
bitonicMergePairs _ _ 0 = []
bitonicMergePairs fl i n =
	(bool id flip fl zip)
		[i, i + 1 .. i + 2 ^ (n - 1) - 1]
		[i + 2 ^ (n - 1) , i + 2 ^ (n - 1) + 1 .. i + 2 ^ n - 1] :
	zipWith (++)
		(bitonicMergePairs fl i (n - 1))
		(bitonicMergePairs fl (i + 2 ^ (n - 1)) (n - 1))

[glslComputeShader|

#version 460

layout(binding = 0) buffer Data {
	uint val[];
} data[3];

void
main()
{
	int index = int(gl_GlobalInvocationID.x) + (int(gl_GlobalInvocationID.y) << 15);
	int i1 = int(data[0].val[index]);
	int i2 = int(data[1].val[index]);
	if (data[2].val[i1] > data[2].val[i2]) {
		uint t = data[2].val[i1];
		data[2].val[i1] = data[2].val[i2];
		data[2].val[i2] = t; }
}

|]
