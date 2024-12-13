{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main(main) where

import Control.Monad.Fix
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.Bits
import Data.Function.ToolsYj
import Data.Tuple.ToolsYj
import Data.List.ToolsYj
import Data.Bool
import Data.Bool.ToolsYj
import Data.Text.IO qualified as Txt
import Data.IORef

import Graphics.UI.GlfwG qualified as GlfwG
import Graphics.UI.GlfwG.Window qualified as GlfwG.Win

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.Instance qualified as Vk.Ist
import Gpu.Vulkan.PhysicalDevice qualified as Vk.Phd

import Gpu.Vulkan.Khr.Surface.Glfw.Window qualified as Vk.Khr.Sfc.Glfw.Win

import Gpu.Vulkan.Ext.DebugUtils qualified as Vk.DbgUtls
import Gpu.Vulkan.Ext.DebugUtils.Messenger qualified as Vk.DbgUtls.Msngr

import Debug

main :: IO ()
main = newIORef False >>= \fr -> withWindow fr \w -> createIst \ist ->
	Vk.Khr.Sfc.Glfw.Win.create ist w nil \sfc ->
	Vk.Phd.enumerate ist >>= \[pd] -> printPhdPrps pd >>
	{-
	Vk.Phd.getProperties pd >>= print >>
	v1213Features pd >>= \v1213fs ->
	let TMaybe.J v13fs = Vk.Phd.vulkan12FeaturesNext v1213fs in
	print v1213fs >>
	print (Vk.Phd.vulkan12FeaturesBufferDeviceAddress v1213fs) >>
	print (Vk.Phd.vulkan12FeaturesDescriptorIndexing v1213fs) >>
	print (Vk.Phd.vulkan13FeaturesDynamicRendering v13fs) >>
	print (Vk.Phd.vulkan13FeaturesSynchronization2 v13fs) >>
	-}
	fix \go ->
	GlfwG.pollEvents >>
	GlfwG.Win.shouldClose w >>= \case
	False -> go
	True -> pure ()
	where
	v1213Features pd = do
		Vk.Phd.Features2 (TMaybe.J v1213fs) _fs <- Vk.Phd.getFeatures2
			@('Just (Vk.Phd.Vulkan12Features ('Just (Vk.Phd.Vulkan13Features 'Nothing)))) pd
		pure v1213fs

printPhdPrps :: Vk.Phd.P -> IO ()
printPhdPrps pd = do
	prps <- Vk.Phd.getProperties pd
	fs <- Vk.Phd.getFeatures2 @(
		'Just (Vk.Phd.Vulkan12Features (
		'Just (Vk.Phd.Vulkan13Features 'Nothing) )) ) pd
	let	v12fs = Vk.Phd.features2Next fs
	print $ Vk.Phd.propertiesApiVersion prps
	print v12fs

type FramebufferResized = IORef Bool

withWindow :: FramebufferResized -> (forall s . GlfwG.Win.W s -> IO a) -> IO a
withWindow fr a = GlfwG.init error $ GlfwG.Win.group \g -> a =<< initWindow fr g

initWindow :: FramebufferResized -> GlfwG.Win.Group s () -> IO (GlfwG.Win.W s)
initWindow fr g = do
	Right w <- do
		GlfwG.Win.hint noApi
		uncurryDup (GlfwG.Win.create' g ()) sizeName Nothing Nothing
	w <$ GlfwG.Win.setFramebufferSizeCallback
		w (Just . const3 $ writeIORef fr True)
	where
	noApi = GlfwG.Win.WindowHint'ClientAPI GlfwG.Win.ClientAPI'NoAPI
	sizeName = ((800, 600), "Vulkan Guide Drawing With Compute")

createIst :: (forall si . Vk.Ist.I si -> IO a) -> IO a
createIst f = do
	errorIf emsg . (debug &&) . elemNotAll vldLayers
		. (Vk.layerPropertiesLayerName <$>)
		=<< Vk.Ist.enumerateLayerProperties
	exts <- bool id (Vk.DbgUtls.extensionName :) debug
		. (Vk.Ist.ExtensionName <$>)
		<$> GlfwG.getRequiredInstanceExtensions
	bool	(Vk.Ist.create (info exts) nil f)
		(Vk.Ist.create (infoDbg exts) nil f) debug
	where
	emsg = "validation layers requested, but not available!"
	info exts = Vk.Ist.CreateInfo {
		Vk.Ist.createInfoNext = TMaybe.N,
		Vk.Ist.createInfoFlags = zeroBits,
		Vk.Ist.createInfoApplicationInfo = Just ainfo,
		Vk.Ist.createInfoEnabledLayerNames = [],
		Vk.Ist.createInfoEnabledExtensionNames = exts }
	infoDbg exts = Vk.Ist.CreateInfo {
		Vk.Ist.createInfoNext = TMaybe.J dbgMsngrInfo,
		Vk.Ist.createInfoFlags = zeroBits,
		Vk.Ist.createInfoApplicationInfo = Just ainfo,
		Vk.Ist.createInfoEnabledLayerNames = vldLayers,
		Vk.Ist.createInfoEnabledExtensionNames = exts }
	ainfo = Vk.ApplicationInfo {
		Vk.applicationInfoNext = TMaybe.N,
		Vk.applicationInfoApplicationName = "Hello Triangle",
		Vk.applicationInfoApplicationVersion =
			Vk.makeApiVersion 0 1 0 0,
		Vk.applicationInfoEngineName = "No Engine",
		Vk.applicationInfoEngineVersion = Vk.makeApiVersion 0 1 0 0,
		Vk.applicationInfoApiVersion = Vk.apiVersion_1_3 }

vldLayers :: [Vk.LayerName]
vldLayers = [Vk.layerKhronosValidation]

dbgMsngrInfo :: Vk.DbgUtls.Msngr.CreateInfo 'Nothing '[] ()
dbgMsngrInfo = Vk.DbgUtls.Msngr.CreateInfo {
	Vk.DbgUtls.Msngr.createInfoNext = TMaybe.N,
	Vk.DbgUtls.Msngr.createInfoFlags = zeroBits,
	Vk.DbgUtls.Msngr.createInfoMessageSeverity =
		Vk.DbgUtls.MessageSeverityVerboseBit .|.
		Vk.DbgUtls.MessageSeverityWarningBit .|.
		Vk.DbgUtls.MessageSeverityErrorBit,
	Vk.DbgUtls.Msngr.createInfoMessageType =
		Vk.DbgUtls.MessageTypeGeneralBit .|.
		Vk.DbgUtls.MessageTypeValidationBit .|.
		Vk.DbgUtls.MessageTypePerformanceBit,
	Vk.DbgUtls.Msngr.createInfoFnUserCallback = dbgCallback,
	Vk.DbgUtls.Msngr.createInfoUserData = Nothing }
	where dbgCallback _svr _tp cbdt _ud = False <$ Txt.putStrLn (
		"validation layer: " <>
		Vk.DbgUtls.Msngr.callbackDataMessage cbdt )
