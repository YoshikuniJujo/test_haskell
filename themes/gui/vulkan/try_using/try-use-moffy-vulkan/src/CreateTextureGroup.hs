{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CreateTextureGroup (

	-- * GROUP

	txGroup, TextureGroup,

	-- * CREATE AND UPDATE

	createTexture, destroyTexture, updateTexture, createBuffer,

	-- * BEGIN SINGLE TIME COMMANDS

	beginSingleTimeCommands,

	-- * CREATE INFO

	mkImageViewCreateInfo,

	-- * NOT CLASSIFIED YET

	createBffr, createBffr',
	createImgVw', recreateImgVw,
	singleTimeCmds

	) where

import Control.Arrow
import Foreign.Storable
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.TypeLevel.Tuple.Uncurry
import Data.Default
import Data.Bits
import Data.Maybe
import Data.List qualified as L
import Data.HeteroParList qualified as HPList
import Data.HeteroParList (pattern (:**), pattern (:*.))
import Data.Word
import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.PhysicalDevice qualified as Vk.Phd
import Gpu.Vulkan.Device qualified as Vk.Dvc
import Gpu.Vulkan.CommandPool qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer qualified as Vk.CmdBffr
import Gpu.Vulkan.Cmd qualified as Vk.Cmd
import Gpu.Vulkan.Queue qualified as Vk.Q
import Gpu.Vulkan.QueueFamily qualified as Vk.QFamily
import Gpu.Vulkan.Pipeline qualified as Vk.Ppl
import Gpu.Vulkan.Descriptor qualified as Vk.Dsc
import Gpu.Vulkan.DescriptorSet qualified as Vk.DscSet
import Gpu.Vulkan.Sample qualified as Vk.Sample
import Gpu.Vulkan.Sampler qualified as Vk.Smplr
import Gpu.Vulkan.Image qualified as Vk.Img
import Gpu.Vulkan.ImageView qualified as Vk.ImgVw
import Gpu.Vulkan.Buffer qualified as Vk.Bffr
import Gpu.Vulkan.Memory qualified as Vk.Mm
import Gpu.Vulkan.Component qualified as Vk.Component
import Gpu.Vulkan.Object qualified as Vk.Obj
import Gpu.Vulkan.Object.NoAlignment qualified as Vk.ObjNA
import Gpu.Vulkan.Object.Base qualified as Vk.ObjB

import Data.Bits.ToolsYj
import Data.Either.ToolsYj

type TextureGroup sd si nmt sm siv fmt k = (
	Vk.Img.Group sd 'Nothing si k nmt fmt,
	Vk.Mm.Group sd 'Nothing sm k '[ '(si, 'Vk.Mm.ImageArg nmt fmt)],
	Vk.ImgVw.Group sd 'Nothing siv k nmt fmt )

txGroup :: Vk.Dvc.D sd ->
	(forall si sm siv . TextureGroup sd si nmt sm siv fmt k -> IO a) -> IO a
txGroup dv f =
	Vk.Img.group dv nil \mng -> Vk.Mm.group dv nil \mmng ->
	Vk.ImgVw.group dv nil \ivmng -> f (mng, mmng, ivmng)

createTexture :: forall bis nmt img k sd sc sds sdsc sm si siv ss . (
	Vk.ObjB.IsImage img,
	Vk.DscSet.BindingAndArrayElemImage bis
		'[ '(nmt, Vk.ObjB.ImageFormat img)] 0,
	Ord k ) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.DscSet.D sds '(sdsc, bis) ->
	TextureGroup sd si nmt sm siv (Vk.ObjB.ImageFormat img) k ->
	Vk.Smplr.S ss -> img -> k -> IO ()
createTexture phdv dv gq cp ubds (mng, mmng, ivmng) txsmplr img k =
	putStrLn "CREATE TEXTURE BEGIN" >>
	createTextureImage' phdv dv mng mmng gq cp k img >>= \tximg ->

	Vk.ImgVw.create' @_ @_ @(Vk.ObjB.ImageFormat img)
		ivmng k (mkImageViewCreateInfo tximg) >>= \(AlwaysRight tximgvw) ->

	updateDescriptorSetTex dv ubds tximgvw txsmplr >>
	putStrLn "CREATE TEXTURE END"

destroyTexture :: Ord k => TextureGroup sd si nmt sm siv fmt k -> k -> IO ()
destroyTexture (mng, mmng, ivmng) k = do
	putStrLn "DESTROY TEXTURE BEGIN"
	Vk.Img.unsafeDestroy mng k
	Vk.Mm.unsafeFree mmng k
	Vk.ImgVw.unsafeDestroy ivmng k
	putStrLn "DESTROY TEXTURE END"
	pure ()

updateTexture :: (
	Vk.DscSet.BindingAndArrayElemImage bis '[ '(nmt, fmt)] 0,
	Ord k ) =>
	Vk.Dvc.D sd -> Vk.DscSet.D sds '(sdsc, bis) -> Vk.Smplr.S ss ->
	Vk.ImgVw.Group sd 'Nothing siv k nmt fmt -> k -> IO ()
updateTexture dv udbs txsmplr imng k = do
	Just tximgvw <- Vk.ImgVw.lookup imng k
	updateDescriptorSetTex dv udbs tximgvw txsmplr

mkImageViewCreateInfo ::
	Vk.Img.Binded sm si nm ifmt ->
	Vk.ImgVw.CreateInfo 'Nothing sm si nm ifmt ivfmt
mkImageViewCreateInfo sci = Vk.ImgVw.CreateInfo {
	Vk.ImgVw.createInfoNext = TMaybe.N,
	Vk.ImgVw.createInfoFlags = zeroBits,
	Vk.ImgVw.createInfoImage = sci,
	Vk.ImgVw.createInfoViewType = Vk.ImgVw.Type2d,
	Vk.ImgVw.createInfoComponents = components,
	Vk.ImgVw.createInfoSubresourceRange = subresourceRange }
	where
	components = Vk.Component.Mapping {
		Vk.Component.mappingR = def, Vk.Component.mappingG = def,
		Vk.Component.mappingB = def, Vk.Component.mappingA = def }
	subresourceRange = Vk.Img.SubresourceRange {
		Vk.Img.subresourceRangeAspectMask = Vk.Img.AspectColorBit,
		Vk.Img.subresourceRangeBaseMipLevel = 0,
		Vk.Img.subresourceRangeLevelCount = 1,
		Vk.Img.subresourceRangeBaseArrayLayer = 0,
		Vk.Img.subresourceRangeLayerCount = 1 }

createTextureImage' :: forall k sim nm sd smm sc img . (
	Vk.ObjB.IsImage img, Ord k
	) => Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Img.Group sd 'Nothing sim k nm (Vk.ObjB.ImageFormat img) ->
	Vk.Mm.Group sd 'Nothing smm k
		'[ '(sim, 'Vk.Mm.ImageArg nm (Vk.ObjB.ImageFormat img))] ->
	Vk.Q.Q -> Vk.CmdPl.C sc -> k -> img ->
	IO (Vk.Img.Binded smm sim nm (Vk.ObjB.ImageFormat img))
createTextureImage' phdvc dvc mng mmng gq cp k img = do
	let	wdt = fromIntegral $ Vk.ObjB.imageWidth img
		hgt = fromIntegral $ Vk.ObjB.imageHeight img
	(tximg, _txmem) <- createImage' @(Vk.ObjB.ImageFormat img) phdvc dvc mng mmng k
		wdt hgt Vk.Img.TilingOptimal
		(Vk.Img.UsageTransferDstBit .|. Vk.Img.UsageSampledBit)
		Vk.Mm.PropertyDeviceLocalBit
	putStrLn "before createBufferImage"
	createBufferImage @img @_ phdvc dvc
		(fromIntegral wdt, fromIntegral wdt, fromIntegral hgt, 1)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mm.PropertyHostVisibleBit .|.
			Vk.Mm.PropertyHostCoherentBit )
		\(sb :: Vk.Bffr.Binded
			sm sb "texture-buffer" '[ Vk.Obj.Image 1 a inm]) sbm -> do
		Vk.Mm.write @"texture-buffer"
			@(Vk.Obj.Image 1 img inm) @0 dvc sbm zeroBits img -- (MyImage img)
		print sb
		transitionImageLayout dvc gq cp tximg
			Vk.Img.LayoutUndefined
			Vk.Img.LayoutTransferDstOptimal
		copyBufferToImage dvc gq cp sb tximg wdt hgt
		transitionImageLayout dvc gq cp tximg
			Vk.Img.LayoutTransferDstOptimal
			Vk.Img.LayoutShaderReadOnlyOptimal
	pure tximg

copyBufferToImage :: forall sd sc sm sb nm img inm si sm' nm' .
	Storable (Vk.ObjB.ImagePixel img) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Bffr.Binded sm sb nm '[ Vk.Obj.Image 1 img inm]  ->
	Vk.Img.Binded sm' si nm' (Vk.ObjB.ImageFormat img) ->
	Word32 -> Word32 -> IO ()
copyBufferToImage dvc gq cp bf img wdt hgt =
	beginSingleTimeCommands dvc gq cp \cb -> do
	let	region :: Vk.Bffr.ImageCopy img inm
		region = Vk.Bffr.ImageCopy {
			Vk.Bffr.imageCopyImageSubresource = isr,
			Vk.Bffr.imageCopyImageOffset = Vk.Offset3d 0 0 0,
			Vk.Bffr.imageCopyImageExtent = Vk.Extent3d wdt hgt 1 }
		isr = Vk.Img.SubresourceLayers {
			Vk.Img.subresourceLayersAspectMask =
				Vk.Img.AspectColorBit,
			Vk.Img.subresourceLayersMipLevel = 0,
			Vk.Img.subresourceLayersBaseArrayLayer = 0,
			Vk.Img.subresourceLayersLayerCount = 1 }
	Vk.Cmd.copyBufferToImage @1
		cb bf img Vk.Img.LayoutTransferDstOptimal (HPList.Singleton region)

createBufferImage :: Storable (Vk.ObjB.ImagePixel t) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> (Vk.Dvc.Size, Vk.Dvc.Size, Vk.Dvc.Size, Vk.Dvc.Size) ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags ->
	(forall sm sb .
		Vk.Bffr.Binded sm sb nm '[ Vk.Obj.Image 1 t inm] ->
		Vk.Mm.M sm '[ '(
			sb,
			'Vk.Mm.BufferArg nm '[ Vk.Obj.Image 1 t inm])] ->
		IO a) -> IO a
createBufferImage p dv (r, w, h, d) usg props f =
	putStrLn "createBufferImage begin" >>
	createBuffer p dv (Vk.Obj.LengthImage r w h d) usg props f

createBuffer :: forall sd nm o a . Vk.Obj.SizeAlignment o =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Obj.Length o ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[o] ->
		Vk.Mm.M sm
			'[ '(sb, 'Vk.Mm.BufferArg nm '[o])] ->
		IO a) -> IO a
createBuffer p dv ln usg props f = Vk.Bffr.create dv bffrInfo nil \b -> do
	reqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMemoryType p (Vk.Mm.requirementsMemoryTypeBits reqs) props
	Vk.Mm.allocateBind dv (HPList.Singleton . U2 $ Vk.Mm.Buffer b)
		(allcInfo mt) nil
		$ f . \(HPList.Singleton (U2 (Vk.Mm.BufferBinded bnd))) -> bnd
	where
	bffrInfo :: Vk.Bffr.CreateInfo 'Nothing '[o]
	bffrInfo = Vk.Bffr.CreateInfo {
		Vk.Bffr.createInfoNext = TMaybe.N,
		Vk.Bffr.createInfoFlags = zeroBits,
		Vk.Bffr.createInfoLengths = HPList.Singleton ln,
		Vk.Bffr.createInfoUsage = usg,
		Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Bffr.createInfoQueueFamilyIndices = [] }
	allcInfo :: Vk.Mm.TypeIndex -> Vk.Mm.AllocateInfo 'Nothing
	allcInfo mt = Vk.Mm.AllocateInfo {
		Vk.Mm.allocateInfoNext = TMaybe.N,
		Vk.Mm.allocateInfoMemoryTypeIndex = mt }

createImage' :: forall fmt sim smm nm sd k . Ord k => Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Img.Group sd 'Nothing sim k nm fmt ->
	Vk.Mm.Group sd 'Nothing smm k '[ '(sim, 'Vk.Mm.ImageArg nm fmt)] ->
	k ->
	Word32 -> Word32 -> Vk.Img.Tiling ->
	Vk.Img.UsageFlagBits -> Vk.Mm.PropertyFlagBits -> IO (
		Vk.Img.Binded smm sim nm fmt,
		Vk.Mm.M smm
			'[ '(sim, 'Vk.Mm.ImageArg nm fmt)] )
createImage' pd dvc mng mmng k wdt hgt tlng usg prps = do
	AlwaysRight img <- Vk.Img.create' @_ @'Nothing mng k imageInfo
	reqs <- Vk.Img.getMemoryRequirements dvc img
	print reqs
	mt <- findMemoryType pd (Vk.Mm.requirementsMemoryTypeBits reqs) prps
	print mt
	Right (HPList.Singleton (U2 (Vk.Mm.ImageBinded bnd)), m) <-
		Vk.Mm.allocateBind' @_ @'Nothing mmng k
			(HPList.Singleton . U2 $ Vk.Mm.Image img) (memInfo mt)
	pure (bnd :: Vk.Img.Binded smm sim nm fmt, m)
	where
	imageInfo = Vk.Img.CreateInfo {
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
		Vk.Img.createInfoUsage = usg,
		Vk.Img.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Img.createInfoSamples = Vk.Sample.Count1Bit,
		Vk.Img.createInfoFlags = zeroBits,
		Vk.Img.createInfoQueueFamilyIndices = [] }
	memInfo mt = Vk.Mm.AllocateInfo {
		Vk.Mm.allocateInfoNext = TMaybe.N,
		Vk.Mm.allocateInfoMemoryTypeIndex = mt }

findMemoryType :: Vk.Phd.P -> Vk.Mm.TypeBits -> Vk.Mm.PropertyFlags ->
	IO Vk.Mm.TypeIndex
findMemoryType phdvc flt props =
	fromMaybe (error msg) . suitable <$> Vk.Phd.getMemoryProperties phdvc
	where
	msg = "failed to find suitable memory type!"
	suitable props1 = fst <$> L.find ((&&)
		<$> (`Vk.Mm.elemTypeIndex` flt) . fst
		<*> checkBits props . Vk.Mm.mTypePropertyFlags . snd) tps
		where tps = Vk.Phd.memoryPropertiesMemoryTypes props1

transitionImageLayout :: forall sd sc si sm nm fmt .
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Img.Binded sm si nm fmt -> Vk.Img.Layout -> Vk.Img.Layout ->
	IO ()
transitionImageLayout dvc gq cp img olyt nlyt =
	beginSingleTimeCommands dvc gq cp \cb -> do
	let	barrier :: Vk.Img.MemoryBarrier 'Nothing sm si nm fmt
		barrier = Vk.Img.MemoryBarrier {
			Vk.Img.memoryBarrierNext = TMaybe.N,
			Vk.Img.memoryBarrierOldLayout = olyt,
			Vk.Img.memoryBarrierNewLayout = nlyt,
			Vk.Img.memoryBarrierSrcQueueFamilyIndex =
				Vk.QFamily.Ignored,
			Vk.Img.memoryBarrierDstQueueFamilyIndex =
				Vk.QFamily.Ignored,
			Vk.Img.memoryBarrierImage = img,
			Vk.Img.memoryBarrierSubresourceRange = srr,
			Vk.Img.memoryBarrierSrcAccessMask = sam,
			Vk.Img.memoryBarrierDstAccessMask = dam }
		srr = Vk.Img.SubresourceRange {
			Vk.Img.subresourceRangeAspectMask =
				Vk.Img.AspectColorBit,
			Vk.Img.subresourceRangeBaseMipLevel = 0,
			Vk.Img.subresourceRangeLevelCount = 1,
			Vk.Img.subresourceRangeBaseArrayLayer = 0,
			Vk.Img.subresourceRangeLayerCount = 1 }
	Vk.Cmd.pipelineBarrier cb
		sstg dstg zeroBits HPList.Nil HPList.Nil (HPList.Singleton $ U5 barrier)
	where (sam, dam, sstg, dstg) = case (olyt, nlyt) of
		(Vk.Img.LayoutUndefined, Vk.Img.LayoutTransferDstOptimal) -> (
			zeroBits, Vk.AccessTransferWriteBit,
			Vk.Ppl.StageTopOfPipeBit, Vk.Ppl.StageTransferBit )
		(Vk.Img.LayoutTransferDstOptimal,
			Vk.Img.LayoutShaderReadOnlyOptimal ) -> (
			Vk.AccessTransferWriteBit, Vk.AccessShaderReadBit,
			Vk.Ppl.StageTransferBit, Vk.Ppl.StageFragmentShaderBit )
		_ -> error "unsupported layout transition!"

updateDescriptorSetTex ::
	Vk.DscSet.BindingAndArrayElemImage bis
		'[ '(nmt, fmt)] 0 =>
	Vk.Dvc.D sd -> Vk.DscSet.D sds '(sdsc, bis) ->
	Vk.ImgVw.I nmt fmt siv  -> Vk.Smplr.S ss -> IO ()
updateDescriptorSetTex dvc dscs tximgvw txsmp = do
	Vk.DscSet.updateDs dvc (
		U5 (descriptorWrite1 dscs tximgvw txsmp) :** HPList.Nil )
		HPList.Nil

descriptorWrite1 ::
	Vk.DscSet.D sds slbts -> Vk.ImgVw.I nm fmt si -> Vk.Smplr.S ss ->
	Vk.DscSet.Write 'Nothing sds slbts
		('Vk.DscSet.WriteSourcesArgImage '[ '(ss, nm, fmt, si) ]) 0
descriptorWrite1 dscs tiv tsmp = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = dscs,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeCombinedImageSampler,
	Vk.DscSet.writeSources = Vk.DscSet.ImageInfos . HPList.Singleton
		$ U4 Vk.Dsc.ImageInfo {
			Vk.Dsc.imageInfoImageLayout =
				Vk.Img.LayoutShaderReadOnlyOptimal,
			Vk.Dsc.imageInfoImageView = tiv,
			Vk.Dsc.imageInfoSampler = tsmp }
	}

beginSingleTimeCommands :: forall sd sc a .
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	(forall s . Vk.CmdBffr.C s -> IO a) -> IO a
beginSingleTimeCommands dvc gq cp cmd = do
	Vk.CmdBffr.allocate
		dvc allocInfo \((cb :: Vk.CmdBffr.C s) :*. HPList.Nil) -> do
		let	submitInfo :: Vk.SubmitInfo 'Nothing '[] '[s] '[]
			submitInfo = Vk.SubmitInfo {
				Vk.submitInfoNext = TMaybe.N,
				Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
				Vk.submitInfoCommandBuffers = HPList.Singleton cb,
				Vk.submitInfoSignalSemaphores = HPList.Nil }
		Vk.CmdBffr.begin @'Nothing @'Nothing cb beginInfo (cmd cb) <* do
			Vk.Q.submit gq (HPList.Singleton $ U4 submitInfo) Nothing
			Vk.Q.waitIdle gq
	where
	allocInfo :: Vk.CmdBffr.AllocateInfo 'Nothing sc '[ '()]
	allocInfo = Vk.CmdBffr.AllocateInfo {
		Vk.CmdBffr.allocateInfoNext = TMaybe.N,
		Vk.CmdBffr.allocateInfoCommandPool = cp,
		Vk.CmdBffr.allocateInfoLevel = Vk.CmdBffr.LevelPrimary }
	beginInfo = Vk.CmdBffr.BeginInfo {
		Vk.CmdBffr.beginInfoNext = TMaybe.N,
		Vk.CmdBffr.beginInfoFlags = Vk.CmdBffr.UsageOneTimeSubmitBit,
		Vk.CmdBffr.beginInfoInheritanceInfo = Nothing }

{-# COMPLETE AlwaysRight #-}

pattern AlwaysRight :: b -> Either a b
pattern AlwaysRight x <- Right x

-- BEGIN SINGLE TIME COMMANDS

singleTimeCmds :: forall sd sc a . Vk.Dvc.D sd -> Vk.Q.Q ->
	Vk.CmdPl.C sc -> (forall scb . Vk.CmdBffr.C scb -> IO a) -> IO a
singleTimeCmds dv gq cp cmds =
	Vk.CmdBffr.allocate dv ainfo \(cb :*. HPList.Nil) ->
	Vk.CmdBffr.begin @_ @'Nothing cb binfo (cmds cb) <* do
	Vk.Q.submit gq (HPList.Singleton . U4 $ sinfo cb) Nothing
	Vk.Q.waitIdle gq
	where
	ainfo :: Vk.CmdBffr.AllocateInfo 'Nothing sc '[ '()]
	ainfo = Vk.CmdBffr.AllocateInfo {
		Vk.CmdBffr.allocateInfoNext = TMaybe.N,
		Vk.CmdBffr.allocateInfoCommandPool = cp,
		Vk.CmdBffr.allocateInfoLevel = Vk.CmdBffr.LevelPrimary }
	binfo = Vk.CmdBffr.BeginInfo {
		Vk.CmdBffr.beginInfoNext = TMaybe.N,
		Vk.CmdBffr.beginInfoFlags = Vk.CmdBffr.UsageOneTimeSubmitBit,
		Vk.CmdBffr.beginInfoInheritanceInfo = Nothing }
	sinfo cb = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Nil }

-- BUFFER

createBffrImg :: Storable (Vk.ObjB.ImagePixel i) =>
	Vk.Phd.P -> Vk.Dvc.D sd ->
	(Vk.Dvc.Size, Vk.Dvc.Size, Vk.Dvc.Size, Vk.Dvc.Size) ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags ->
	(forall m b .
		Vk.Bffr.Binded m b bn '[ Vk.ObjNA.Image i n] ->
		Vk.Mm.M m '[ '(b, 'Vk.Mm.BufferArg bn '[Vk.ObjNA.Image i n])] ->
		IO a) -> IO a
createBffrImg p dv (r, w, h, d) = createBffr p dv (Vk.Obj.LengthImage r w h d)

createBffr :: forall sd o nm a . Vk.Obj.SizeAlignment o =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Obj.Length o ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[o] ->
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[o])] -> IO a) -> IO a
createBffr p dv ln us prs f = Vk.Bffr.create dv (bffrInfo ln us) nil \b -> do
	rqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMmType p (Vk.Mm.requirementsMemoryTypeBits rqs) prs
	Vk.Mm.allocateBind dv (HPList.Singleton . U2 $ Vk.Mm.Buffer b)
		(mmAllcInfo mt) nil
		$ f . \(HPList.Singleton (U2 (Vk.Mm.BufferBinded bb))) -> bb

createBffr' :: forall sd sm sb k nm o . (Ord k, Vk.Obj.SizeAlignment o) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Bffr.Group sd 'Nothing sb k nm '[o] ->
	Vk.Mm.Group sd 'Nothing sm k '[ '(sb, 'Vk.Mm.BufferArg nm '[o])] -> k ->
	Vk.Obj.Length o -> Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> IO (
		Vk.Bffr.Binded sm sb nm '[o],
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[o])] )
createBffr' p dv bg mg k ln us prs =
	Vk.Bffr.create' bg k (bffrInfo ln us) >>= \(forceRight' -> b) -> do
	rqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMmType p (Vk.Mm.requirementsMemoryTypeBits rqs) prs
	(<$> Vk.Mm.allocateBind' mg k
		(HPList.Singleton . U2 $ Vk.Mm.Buffer b) (mmAllcInfo mt))
		\(forceRight' ->
			(HPList.Singleton (U2 (Vk.Mm.BufferBinded bb)), m)) ->
		(bb, m)

bffrInfo ::
	Vk.Obj.Length s -> Vk.Bffr.UsageFlags -> Vk.Bffr.CreateInfo Nothing '[s]
bffrInfo ln us = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N, Vk.Bffr.createInfoFlags = zeroBits,
	Vk.Bffr.createInfoLengths = HPList.Singleton ln,
	Vk.Bffr.createInfoUsage = us,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

findMmType ::
	Vk.Phd.P -> Vk.Mm.TypeBits -> Vk.Mm.PropertyFlags -> IO Vk.Mm.TypeIndex
findMmType pd flt prs =
	fromMaybe (error msg) . suit <$> Vk.Phd.getMemoryProperties pd
	where
	msg = "failed to find suitable memory type!"
	suit p = fst <$> L.find (uncurry (&&) .
			((`Vk.Mm.elemTypeIndex` flt) ***
			checkBits prs . Vk.Mm.mTypePropertyFlags))
		(Vk.Phd.memoryPropertiesMemoryTypes p)

mmAllcInfo :: Vk.Mm.TypeIndex -> Vk.Mm.AllocateInfo 'Nothing
mmAllcInfo mt = Vk.Mm.AllocateInfo {
	Vk.Mm.allocateInfoNext = TMaybe.N,
	Vk.Mm.allocateInfoMemoryTypeIndex = mt }

-- IMAGE VIEW

createImgVw' :: forall sd sv k nm vfmt sm si ifmt .
	(Ord k, Vk.T.FormatToValue vfmt) =>
	Vk.ImgVw.Group sd 'Nothing sv k nm vfmt ->
	k -> Vk.Img.Binded sm si nm ifmt -> IO (Vk.ImgVw.I nm vfmt sv)
createImgVw' vg k i = forceRight' <$> Vk.ImgVw.create' vg k (imgVwInfo i)

recreateImgVw :: Vk.T.FormatToValue ivfmt => Vk.Dvc.D sd ->
	Vk.Img.Binded sm si nm ifmt -> Vk.ImgVw.I nm ivfmt sv -> IO ()
recreateImgVw dv i = Vk.ImgVw.unsafeRecreate dv (imgVwInfo i) nil

imgVwInfo :: Vk.Img.Binded sm si nm ifmt ->
	Vk.ImgVw.CreateInfo 'Nothing sm si nm ifmt vfmt
imgVwInfo i = Vk.ImgVw.CreateInfo {
	Vk.ImgVw.createInfoNext = TMaybe.N,
	Vk.ImgVw.createInfoFlags = zeroBits,
	Vk.ImgVw.createInfoImage = i,
	Vk.ImgVw.createInfoViewType = Vk.ImgVw.Type2d,
	Vk.ImgVw.createInfoComponents = def,
	Vk.ImgVw.createInfoSubresourceRange = Vk.Img.SubresourceRange {
		Vk.Img.subresourceRangeAspectMask = Vk.Img.AspectColorBit,
		Vk.Img.subresourceRangeBaseMipLevel = 0,
		Vk.Img.subresourceRangeLevelCount = 1,
		Vk.Img.subresourceRangeBaseArrayLayer = 0,
		Vk.Img.subresourceRangeLayerCount = 1 } }
