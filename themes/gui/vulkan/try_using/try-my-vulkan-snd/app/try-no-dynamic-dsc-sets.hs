{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
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
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.HeteroParList qualified as HPList
import Data.Word
import System.Environment

import Language.SpirV.Shaderc.TH
import Language.SpirV.ShaderKind

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.Object qualified as Obj
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
import Gpu.Vulkan.Pipeline.Compute qualified as Vk.Ppl.Cmpt
import Gpu.Vulkan.Pipeline.ShaderStage qualified as Vk.Ppl.ShaderSt
import Gpu.Vulkan.PipelineLayout qualified as Vk.Ppl.Lyt
import Gpu.Vulkan.PushConstant qualified as Vk.PushConstant
import Gpu.Vulkan.ShaderModule qualified as Vk.ShaderMod
import Gpu.Vulkan.Descriptor qualified as Vk.Dsc
import Gpu.Vulkan.DescriptorPool qualified as Vk.DscPl
import Gpu.Vulkan.DescriptorSet qualified as Vk.DscSt
import Gpu.Vulkan.DescriptorSetLayout qualified as Vk.DscStLyt

main :: IO ()
main = withDvc \pd d q cpl mgcx -> do
	arg <- (<$> getArgs) \case
		"0" : _ -> X0; "1" : _ -> X1; "2" : _ -> X2; _ -> X0
	let	da = V.fromList $ W1 <$> [1 .. mgcx]
		db = V.fromList $ W2 <$> [100, 200 .. 100 * mgcx]
		dc = V.replicate mgcx $ W3 0
	rs <- Vk.DscStLyt.create d dscStLytInfo nil \dsl ->
		createBffr3Mm3 arg pd d dsl da db dc \dss
			(ma :: Mm sm1 '[ '(sb1, MmBffrArg nm1 '[OList W1])])
			(mb :: Mm sm2 '[ '(sb2, MmBffrArg nm2 '[OList W2])])
			(mc :: Mm sm3 '[ '(sb3, MmBffrArg nm3 '[OList W3])]) ->
		calc d q cpl dsl dss mgcx >> (,,)
			<$> Vk.Mm.read @nm1 @(OList W1) @0 @[W1] d ma zeroBits
			<*> Vk.Mm.read @nm2 @(OList W2) @0 @[W2] d mb zeroBits
			<*> Vk.Mm.read @nm3 @(OList W3) @0 @[W3] d mc zeroBits
	mapTup3M_ (print . take 20)
		$ appTup3 (unW1 <$>) (unW2 <$>) (unW3 <$>) rs

data X = X0 | X1 | X2 deriving Show

newtype W1 = W1 { unW1 :: Word32 } deriving (Show, Storable)
newtype W2 = W2 { unW2 :: Word32 } deriving (Show, Storable)
newtype W3 = W3 { unW3 :: Word32 } deriving (Show, Storable)

type OList w = Obj.List 256 w ""
type OAtom w mnm = Obj.AtomMaybeName 256 w mnm

withDvc :: (forall sd scpl .
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C scpl ->
	(forall c . Integral c => c) -> IO a) -> IO a
withDvc a = Vk.Inst.create instInfo nil \inst -> do
	pd <- head' <$> Vk.Phd.enumerate inst
	mgcx :. _ <- Vk.Phd.limitsMaxComputeWorkGroupCount
		. Vk.Phd.propertiesLimits <$> Vk.Phd.getProperties pd
	qfi <- fst . head' . filter (
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

cmdPlInfo :: Vk.QFam.Index -> Vk.CmdPl.CreateInfo 'Nothing
cmdPlInfo qfi = Vk.CmdPl.CreateInfo {
	Vk.CmdPl.createInfoNext = TMaybe.N,
	Vk.CmdPl.createInfoFlags = Vk.CmdPl.CreateResetCommandBufferBit,
	Vk.CmdPl.createInfoQueueFamilyIndex = qfi }

dscStLytInfo :: Vk.DscStLyt.CreateInfo 'Nothing DscStLytArg
dscStLytInfo = Vk.DscStLyt.CreateInfo {
	Vk.DscStLyt.createInfoNext = TMaybe.N,
	Vk.DscStLyt.createInfoFlags = zeroBits,
	Vk.DscStLyt.createInfoBindings = bdg :** bdg :** HPList.Nil }
	where bdg = Vk.DscStLyt.BindingBuffer {
		Vk.DscStLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeStorageBuffer,
		Vk.DscStLyt.bindingBufferStageFlags = Vk.ShaderStageComputeBit }

type DscStLytArg = '[
	'Vk.DscStLyt.Buffer '[OList W1, OList W2, OList W3],
	'Vk.DscStLyt.Buffer '[OAtom Word32 'Nothing] ]

-- PREPARE MEMORIES

createBffr3Mm3 ::
	X -> Vk.Phd.P -> Vk.Dvc.D sd -> Vk.DscStLyt.D sl DscStLytArg ->
	V.Vector W1 -> V.Vector W2 -> V.Vector W3 -> (forall sds sm1 sm2 sm3 sb1 sb2 sb3 .
		Vk.DscSt.D sds '(sl, DscStLytArg) ->
		Mm sm1 '[ '(sb1, MmBffrArg nm1 '[OList W1])] ->
		Mm sm2 '[ '(sb2, MmBffrArg nm2 '[OList W2])] ->
		Mm sm3 '[ '(sb3, MmBffrArg nm3 '[OList W3])] -> IO a) -> IO a
createBffr3Mm3 arg pd dv dsl da db dc a =
	Vk.DscPl.create dv dscPlInfo nil \dp ->
	Vk.DscSt.allocateDs dv (dscStInfo dp dsl) \(HPList.Singleton dss) ->
	bffr3Mm3 pd dv da db dc \(ba, ma) (bb, mb) (bc, mc) ->
	bffrMmX @Word32
		@(OAtom Word32 ('Just "x0")) @(OAtom Word32 ('Just "x1"))
		@(OAtom Word32 ('Just "x2"))
		pd dv 3 5 7 \bx _mx -> (>> a dss ma mb mc) case arg of
			X0 -> Vk.DscSt.updateDs dv (
				U5 (writeDscStBffr3 dss ba bb bc) :**
				U5 (writeDscStX @"x0" dss bx) :**
				HPList.Nil ) HPList.Nil
			X1 -> Vk.DscSt.updateDs dv (
				U5 (writeDscStBffr3 dss ba bb bc) :**
				U5 (writeDscStX @"x1" dss bx) :**
				HPList.Nil ) HPList.Nil
			X2 -> Vk.DscSt.updateDs dv (
				U5 (writeDscStBffr3 dss ba bb bc) :**
				U5 (writeDscStX @"x2" dss bx) :**
				HPList.Nil ) HPList.Nil

type Mm = Vk.Mm.M
type MmBffrArg = 'Vk.Mm.BufferArg

dscPlInfo :: Vk.DscPl.CreateInfo 'Nothing
dscPlInfo = Vk.DscPl.CreateInfo {
	Vk.DscPl.createInfoNext = TMaybe.N,
	Vk.DscPl.createInfoFlags = Vk.DscPl.CreateFreeDescriptorSetBit,
	Vk.DscPl.createInfoMaxSets = 1,
	Vk.DscPl.createInfoPoolSizes = (: []) Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeStorageBuffer,
		Vk.DscPl.sizeDescriptorCount = 4 } }

dscStInfo :: Vk.DscPl.P sp -> Vk.DscStLyt.D sl bts ->
	Vk.DscSt.AllocateInfo 'Nothing sp '[ '(sl, bts)]
dscStInfo dpl dsl = Vk.DscSt.AllocateInfo {
	Vk.DscSt.allocateInfoNext = TMaybe.N,
	Vk.DscSt.allocateInfoDescriptorPool = dpl,
	Vk.DscSt.allocateInfoSetLayouts = HPList.Singleton $ U2 dsl }

bffr3Mm3 :: Vk.Phd.P -> Vk.Dvc.D sd ->
	V.Vector W1 -> V.Vector W2 -> V.Vector W3 -> (
		forall sm1 sm2 sm3 sb1 sb2 sb3 .
		BffrMm sm1 sb1 nm1 W1 -> BffrMm sm2 sb2 nm2 W2 ->
		BffrMm sm3 sb3 nm3 W3 -> IO a ) -> IO a
bffr3Mm3 pd dv x y z a = bffrMms pd dv (x :** y :** z :** HPList.Nil) $ arg3 a

arg3 :: (forall sm1 sm2 sm3 sb1 sb2 sb3 .
	BffrMm sm1 sb1 nm1 w1 -> BffrMm sm2 sb2 nm2 w2 ->
	BffrMm sm3 sb3 nm3 w3 -> r) -> Arg nm1 w1 (Arg nm2 w2 (Arg nm3 w3 r))
arg3 f = Arg \b1 m1 -> Arg \b2 m2 -> Arg \b3 m3 -> f (b1, m1) (b2, m2) (b3, m3)

type BffrMm sm sb nm w = (
	Vk.Bffr.Binded sm sb nm '[OList w],
	Mm sm '[ '(sb, MmBffrArg nm '[OList w])] )

class BffrMms f a where
	type Vectors f :: [Type]
	bffrMms :: Vk.Phd.P ->
		Vk.Dvc.D sd -> HPList.PL V.Vector (Vectors f) -> f -> IO a

instance BffrMms (IO a) a where
	type Vectors (IO a) = '[]; bffrMms _pd _dv HPList.Nil f = f

instance (Storable w, BffrMms f a) => BffrMms (Arg nm w f) a where
	type Vectors (Arg nm w f) = w ': Vectors f
	bffrMms pd dv (vs :** vss) (Arg f) =
		bffrMm pd dv vs \b m -> bffrMms @f @a pd dv vss $ f b m

data Arg nm w f = Arg (forall sm sb .
	Vk.Bffr.Binded sm sb nm '[OList w] ->
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[OList w])] -> f)

type KBuffer = 'Vk.Mm.BufferArg

bffrMm :: forall {sd} v {nm} obj {a} . (
	Obj.Store v obj, Obj.SizeAlignment obj ) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> v -> (forall sb sm .
		Vk.Bffr.Binded sm sb nm '[obj]  ->
		Vk.Mm.M sm '[ '(sb, KBuffer nm '[obj])] -> IO a) -> IO a
bffrMm pd dv xs a = Vk.Bffr.create dv (bffrInfoBffr3 xs) nil \b ->
	mmInfo pd dv b >>= \mi ->
	Vk.Mm.allocateBind dv (HPList.Singleton . U2 $ Vk.Mm.Buffer b) mi nil
		\(HPList.Singleton (U2 (Vk.Mm.BufferBinded bnd))) mm ->
	Vk.Mm.write @nm @obj @0 dv mm zeroBits xs >> a bnd mm

bffrMmX :: forall {sd} v {nm} obj0 obj1 obj2 {a} . (
	Obj.Store v obj0, Obj.SizeAlignment obj0,
	Obj.Store v obj1, Obj.SizeAlignment obj1,
	Obj.Store v obj2, Obj.SizeAlignment obj2,
	Obj.OffsetRange obj0 '[obj0, obj1, obj2] 0,
	Obj.OffsetRange obj1 '[obj0, obj1, obj2] 0,
	Obj.OffsetRange obj2 '[obj0, obj1, obj2] 0,
	Obj.LengthOf obj0 '[obj0, obj1, obj2],
	Obj.LengthOf obj1 '[obj0, obj1, obj2],
	Obj.LengthOf obj2 '[obj0, obj1, obj2]
	) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> v -> v -> v -> (forall sb sm .
		Vk.Bffr.Binded sm sb nm '[obj0, obj1, obj2]  ->
		Vk.Mm.M sm '[ '(sb, KBuffer nm '[obj0, obj1, obj2])] ->
		IO a) -> IO a
bffrMmX pd dvc x y z f =
	Vk.Bffr.create dvc (bffrInfoX x y z) nil \bff -> do
		mi <- mmInfo pd dvc bff
		Vk.Mm.allocateBind dvc (HPList.Singleton . U2 $ Vk.Mm.Buffer bff) mi
			nil \(HPList.Singleton (U2 (Vk.Mm.BufferBinded bnd))) m -> do
			Vk.Mm.write @nm @obj0 @0 dvc m zeroBits x
			Vk.Mm.write @nm @obj1 @0 dvc m zeroBits y
			Vk.Mm.write @nm @obj2 @0 dvc m zeroBits z
			f bnd m

bffrInfoBffr3 :: Obj.Store v obj => v -> Vk.Bffr.CreateInfo 'Nothing '[obj]
bffrInfoBffr3 xs = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N,
	Vk.Bffr.createInfoFlags = zeroBits,
	Vk.Bffr.createInfoLengths = HPList.Singleton $ Obj.length xs,
	Vk.Bffr.createInfoUsage = Vk.Bffr.UsageStorageBufferBit,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

bffrInfoX :: (
	Obj.Store v obj0, Obj.Store v obj1, Obj.Store v obj2 ) =>
	v -> v -> v -> Vk.Bffr.CreateInfo 'Nothing '[obj0, obj1, obj2]
bffrInfoX x y z = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N,
	Vk.Bffr.createInfoFlags = zeroBits,
	Vk.Bffr.createInfoLengths =
		Obj.length x :** Obj.length y :**
		Obj.length z :** HPList.Nil,
	Vk.Bffr.createInfoUsage = Vk.Bffr.UsageStorageBufferBit,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

mmInfo :: Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Bffr.B sb nm objs -> IO (Vk.Mm.AllocateInfo 'Nothing)
mmInfo pd dv bf = do
	rqs <- Vk.Bffr.getMemoryRequirements dv bf
	mti <- findMmTpIdx pd rqs
		$ Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit
	pure Vk.Mm.AllocateInfo {
		Vk.Mm.allocateInfoNext = TMaybe.N,
		Vk.Mm.allocateInfoMemoryTypeIndex = mti }

findMmTpIdx :: Vk.Phd.P ->
	Vk.Mm.Requirements -> Vk.Mm.PropertyFlags -> IO Vk.Mm.TypeIndex
findMmTpIdx pd rqs wt = Vk.Phd.getMemoryProperties pd >>= \prs ->
	let	rqts = Vk.Mm.requirementsMemoryTypeBits rqs
		wtts = (fst <$>)
			. filter (checkBits wt . Vk.Mm.mTypePropertyFlags . snd)
			$ Vk.Phd.memoryPropertiesMemoryTypes prs in
	case filter (`Vk.Mm.elemTypeIndex` rqts) wtts of
		[] -> error "No available memory types"; i : _ -> pure i

writeDscStBffr3 :: forall
	sds slbts sm1 sm2 sm3 sb1 sb2 sb3 nm1 nm2 nm3 os1 os2 os3 . (
	Show (HPList.PL Obj.Length os1), Show (HPList.PL Obj.Length os2),
	Show (HPList.PL Obj.Length os3),
	Obj.OffsetRange (OList W1) os1 0, Obj.OffsetRange (OList W2) os2 0,
	Obj.OffsetRange (OList W3) os3 0 ) =>
	Vk.DscSt.D sds slbts ->
	Vk.Bffr.Binded sm1 sb1 nm1 os1 -> Vk.Bffr.Binded sm2 sb2 nm2 os2 ->
	Vk.Bffr.Binded sm3 sb3 nm3 os3 ->
	Vk.DscSt.Write 'Nothing sds slbts ('Vk.DscSt.WriteSourcesArgBuffer '[
		'(sm1, sb1, nm1, OList W1, 0), '(sm2, sb2, nm2, OList W2, 0),
		'(sm3, sb3, nm3, OList W3, 0) ]) 0
writeDscStBffr3 ds ba bb bc = Vk.DscSt.Write {
	Vk.DscSt.writeNext = TMaybe.N, Vk.DscSt.writeDstSet = ds,
	Vk.DscSt.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSt.writeSources = Vk.DscSt.BufferInfos $
		U5 (Vk.Dsc.BufferInfo @_ @_ @_ @(OList W1) ba) :**
		U5 (Vk.Dsc.BufferInfo @_ @_ @_ @(OList W2) bb) :**
		U5 (Vk.Dsc.BufferInfo @_ @_ @_ @(OList W3) bc) :**
		HPList.Nil }

writeDscStX :: forall onm os sds sl sm sb bnm . (
	Show (HPList.PL Obj.Length os),
	Obj.OffsetRange (OAtom Word32 ('Just onm)) os 0 ) =>
	Vk.DscSt.D sds '(sl, DscStLytArg) -> Vk.Bffr.Binded sm sb bnm os ->
	Vk.DscSt.Write 'Nothing sds '(sl, DscStLytArg) (
		'Vk.DscSt.WriteSourcesArgBuffer
			'[ '(sm, sb, bnm, OAtom Word32 ('Just onm), 0)] ) 0
writeDscStX ds bx = Vk.DscSt.Write {
	Vk.DscSt.writeNext = TMaybe.N, Vk.DscSt.writeDstSet = ds,
	Vk.DscSt.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSt.writeSources = Vk.DscSt.BufferInfos
		. HPList.Singleton . U5 $ Vk.Dsc.BufferInfo bx }

-- CALC

calc :: forall sl sd scpl sds .
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C scpl ->
	Vk.DscStLyt.D sl DscStLytArg ->
	Vk.DscSt.D sds '(sl, DscStLytArg) -> Word32 -> IO ()
calc dv q cpl dsl dss sz =
	Vk.Ppl.Lyt.create dv (pplLytInfo dsl) nil \pl ->
	Vk.Ppl.Cmpt.createCs dv Nothing (HPList.Singleton . U4 $ cmpPplInfo pl)
		nil \(cppl :** HPList.Nil) ->
	Vk.CBffr.allocate dv (cmdBffrInfo cpl) \(cb :*. HPList.Nil) ->
	run q cb pl cppl dss sz

pplLytInfo :: Vk.DscStLyt.D sl bts ->
	Vk.Ppl.Lyt.CreateInfo 'Nothing '[ '(sl, bts)]
		('Vk.PushConstant.Layout '[] '[])
pplLytInfo dsl = Vk.Ppl.Lyt.CreateInfo {
	Vk.Ppl.Lyt.createInfoNext = TMaybe.N,
	Vk.Ppl.Lyt.createInfoFlags = zeroBits,
	Vk.Ppl.Lyt.createInfoSetLayouts = HPList.Singleton $ U2 dsl }

cmdBffrInfo :: Vk.CmdPl.C s -> Vk.CBffr.AllocateInfo 'Nothing s '[ '()]
cmdBffrInfo cpl = Vk.CBffr.AllocateInfo {
	Vk.CBffr.allocateInfoNext = TMaybe.N,
	Vk.CBffr.allocateInfoCommandPool = cpl,
	Vk.CBffr.allocateInfoLevel = Vk.CBffr.LevelPrimary }

run :: forall sc sg sl sdsl sds .
	Vk.Q.Q -> Vk.CBffr.C sc ->
	Vk.Ppl.Lyt.P sl '[ '(sdsl, DscStLytArg)] '[] ->
	Vk.Ppl.Cmpt.C sg '(sl, '[ '(sdsl, DscStLytArg)], '[]) ->
	Vk.DscSt.D sds '(sdsl, DscStLytArg) -> Word32 -> IO ()
run q cb pl cppl dss sz = do
	Vk.CBffr.begin @'Nothing @'Nothing cb def $
		Vk.Cmd.bindPipelineCompute
			cb Vk.Ppl.BindPointCompute cppl \ccb ->
		Vk.Cmd.bindDescriptorSetsCompute
			ccb pl (HPList.Singleton $ U2 dss)
			(HPList.Singleton $
				HPList.Nil :** HPList.Nil :** HPList.Nil) >>
		Vk.Cmd.dispatch ccb sz 1 1
	Vk.Q.submit q (HPList.Singleton $ U4 sinfo) Nothing
	Vk.Q.waitIdle q
	where sinfo = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Nil }

cmpPplInfo :: Vk.Ppl.Lyt.P sl '[ '(sdsl, DscStLytArg)] '[] ->
	Vk.Ppl.Cmpt.CreateInfo 'Nothing
		'( 'Nothing, 'Nothing, 'GlslComputeShader, 'Nothing,
		'[Word32, Word32]) '(sl, '[ '(sdsl, DscStLytArg)], '[]) sbph
cmpPplInfo pl = Vk.Ppl.Cmpt.CreateInfo {
	Vk.Ppl.Cmpt.createInfoNext = TMaybe.N,
	Vk.Ppl.Cmpt.createInfoFlags = zeroBits,
	Vk.Ppl.Cmpt.createInfoStage = U5 shdrStInfo,
	Vk.Ppl.Cmpt.createInfoLayout = U3 pl,
	Vk.Ppl.Cmpt.createInfoBasePipelineHandleOrIndex = Nothing }

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

head' :: [a] -> a
head' = \case [] -> error "empty list"; x : _ -> x

tail' :: [a] -> [a]
tail' = \case [] -> error "empty list"; _ : xs -> xs

[glslComputeShader|

#version 460

layout(local_size_x = 1, local_size_y = 1) in;
layout(binding = 0) buffer Data { uint val[]; } data[3];
layout(binding = 1) buffer Foo { uint x; } x;

layout(constant_id = 0) const uint sc = 2;
layout(constant_id = 1) const uint sc2 = 3;

void
main()
{
	int i = int(gl_GlobalInvocationID.x);
	data[2].val[i] = (data[0].val[i] + data[1].val[i]) * sc * sc2 + x.x;
}

|]
