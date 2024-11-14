{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.Bits
import Data.Bits.ToolsYj
import Data.Default
import Data.Maybe
import Data.Maybe.ToolsYj
import Data.List qualified as L
import Data.List.ToolsYj
import Data.HeteroParList (pattern (:*.))
import Data.HeteroParList qualified as HPList
import Data.Array
import Data.Word
import Data.Int
import Text.Read
import System.Environment
import Codec.Picture

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.Object qualified as Vk.Obj
import Gpu.Vulkan.Object.NoAlignment qualified as Vk.ObjNA
import Gpu.Vulkan.Object.Base qualified as Vk.ObjB
import Gpu.Vulkan.Instance qualified as Vk.Ist
import Gpu.Vulkan.PhysicalDevice qualified as Vk.Phd
import Gpu.Vulkan.Queue qualified as Vk.Q
import Gpu.Vulkan.QueueFamily qualified as Vk.QFam
import Gpu.Vulkan.Device qualified as Vk.Dvc
import Gpu.Vulkan.Memory qualified as Vk.Mm
import Gpu.Vulkan.Buffer qualified as Vk.Bffr
import Gpu.Vulkan.Image qualified as Vk.Img
import Gpu.Vulkan.CommandPool qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer qualified as Vk.CBffr
import Gpu.Vulkan.Cmd qualified as Vk.Cmd
import Gpu.Vulkan.Pipeline qualified as Vk.Ppl
import Gpu.Vulkan.Sample qualified as Vk.Sample

-- MAIN

main :: IO ()
main = getArgs >>= \case
	[ifp, ofp, readMaybe -> Just n, readMaybe -> Just i] -> do
		img <- either error convertRGBA8 <$> readImage ifp
		ImageRgba8 img' <- realMain (ImageRgba8 img) n i
		writePng ofp img'
	_ -> error "bad arguments"

realMain :: ImageRgba8 -> Int32 -> Int32 -> IO ImageRgba8
realMain img n i = createIst \ist -> pickPhd ist >>= \(pd, qfi) ->
	print pd >>
	(print . Vk.Phd.propertiesDeviceName =<< Vk.Phd.getProperties pd) >>
	print qfi >> pure img

createIst :: (forall si . Vk.Ist.I si -> IO a) -> IO a
createIst f = Vk.Ist.create info nil f
	where
	info :: Vk.Ist.CreateInfo 'Nothing 'Nothing
	info = def { Vk.Ist.createInfoEnabledLayerNames = vldLayers }

vldLayers :: [Vk.LayerName]
vldLayers = [Vk.layerKhronosValidation]

pickPhd :: Vk.Ist.I si -> IO (Vk.Phd.P, Vk.QFam.Index)
pickPhd ist = Vk.Phd.enumerate ist >>= \case
	[] -> error "failed to find GPUs with Gpu.Vulkan support!"
	pds -> findMaybeM suit pds >>= \case
		Nothing -> error "failed to find a suitable GPU!"
		Just pdqfi -> pure pdqfi
	where
	suit pd = findf <$> Vk.Phd.getQueueFamilyProperties pd
	findf ps = fst <$> L.find (grbit . snd) ps
	grbit = checkBits Vk.Q.GraphicsBit . Vk.QFam.propertiesQueueFlags

-- DATA TYPE IMAGE RGBA8

newtype ImageRgba8 = ImageRgba8 (Image PixelRGBA8)
newtype PixelRgba8 = PixelRgba8 PixelRGBA8 deriving Show

instance Vk.ObjB.IsImage ImageRgba8 where
	type ImagePixel ImageRgba8 = PixelRgba8
	type ImageFormat ImageRgba8 = 'Vk.T.FormatR8g8b8a8Unorm
	imageRow = Vk.ObjB.imageWidth
	imageWidth (ImageRgba8 i) = fromIntegral $ imageWidth i
	imageHeight (ImageRgba8 i) = fromIntegral $ imageHeight i
	imageDepth _ = 1
	imageBody (ImageRgba8 i) = (<$> [0 .. imageHeight i - 1]) \y ->
		(<$> [0 .. imageWidth i - 1]) \x -> PixelRgba8 $ pixelAt i x y
	imageMake (fromIntegral -> w) (fromIntegral -> h) _d pss =
		ImageRgba8 $ generateImage
			(\x y -> let PixelRgba8 p = (pss' ! y) ! x in p) w h
		where pss' = listArray (0, h - 1) (listArray (0, w - 1) <$> pss)

instance Storable PixelRgba8 where
	sizeOf _ = 4 * sizeOf @Pixel8 undefined
	alignment _ = alignment @Pixel8 undefined
	peek p = PixelRgba8 . (\(r, g, b, a) -> PixelRGBA8 r g b a)
		. listToTuple4 <$> peekArray 4 (castPtr p)
	poke p (PixelRgba8 (PixelRGBA8 r g b a)) =
		pokeArray (castPtr p) [r, g, b, a]
