{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.SimplePolygon.DebugMessenger (
	checkLayer, validationLayers, extensionName, setup, createInfo ) where

import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.Bits
import Data.List qualified as L
import Data.Text.IO qualified as Txt
import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.Instance.Internal qualified as Vk.Ist
import Gpu.Vulkan.Ext.DebugUtils.Enum qualified as Vk.Ext.DbgUtls
import Gpu.Vulkan.Ext.DebugUtils qualified as Vk.Ext.DbgUtls
import Gpu.Vulkan.Ext.DebugUtils.Messenger qualified as Vk.Ext.DbgUtls.Msngr

validationLayers :: [Vk.LayerName]
validationLayers = [Vk.layerKhronosValidation]

extensionName :: Vk.Ist.ExtensionName
extensionName = Vk.Ext.DbgUtls.extensionName

checkLayer :: IO Bool
checkLayer = null
	. (validationLayers L.\\) . (Vk.layerPropertiesLayerName <$>)
	<$> Vk.Ist.enumerateLayerProperties

setup :: Vk.Ist.I si -> IO a -> IO a
setup ist f = Vk.Ext.DbgUtls.Msngr.create ist createInfo TPMaybe.N f

createInfo :: Vk.Ext.DbgUtls.Msngr.CreateInfo 'Nothing '[] ()
createInfo = Vk.Ext.DbgUtls.Msngr.CreateInfo {
	Vk.Ext.DbgUtls.Msngr.createInfoNext = TMaybe.N,
	Vk.Ext.DbgUtls.Msngr.createInfoFlags = zeroBits,
	Vk.Ext.DbgUtls.Msngr.createInfoMessageSeverity =
		Vk.Ext.DbgUtls.MessageSeverityVerboseBit .|.
		Vk.Ext.DbgUtls.MessageSeverityWarningBit .|.
		Vk.Ext.DbgUtls.MessageSeverityErrorBit,
	Vk.Ext.DbgUtls.Msngr.createInfoMessageType =
		Vk.Ext.DbgUtls.MessageTypeGeneralBit .|.
		Vk.Ext.DbgUtls.MessageTypeValidationBit .|.
		Vk.Ext.DbgUtls.MessageTypePerformanceBit,
	Vk.Ext.DbgUtls.Msngr.createInfoFnUserCallback = debugCallback,
	Vk.Ext.DbgUtls.Msngr.createInfoUserData = Nothing }

debugCallback :: Vk.Ext.DbgUtls.Msngr.FnCallback '[] ()
debugCallback _msgSeverity _msgType cbdt _userData = False <$ Txt.putStrLn
	("validation layer: " <> Vk.Ext.DbgUtls.Msngr.callbackDataMessage cbdt)
