{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Ext.DebugUtils where

import Foreign.Ptr
import Foreign.C.String
import Control.Monad.Cont
import Data.String
import System.IO.Unsafe

import qualified Data.Text as T

import Data.Color

import Vulkan
import Vulkan.Enum
import Vulkan.Base

import qualified Vulkan.Ext.DebugUtils.Core as C

foreign import capi "vulkan/vulkan.h value VK_EXT_DEBUG_UTILS_EXTENSION_NAME"
	c_extensionName :: CString

extensionName :: IsString s => s
extensionName = unsafePerformIO $ fromString <$> peekCString c_extensionName

data Label n = Label {
	labelNext :: Maybe n,
	labelLabelName :: T.Text,
	labelColor :: Rgba }
	deriving Show

labelToCore :: Pointable n => Label n -> ContT r IO C.Label
labelToCore Label {
	labelNext = mnxt,
	labelLabelName = ln,
	labelColor = RgbaFloat r g b a } = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	cln <- textToCString ln
	pure C.Label {
		C.labelSType = (),
		C.labelPNext = pnxt,
		C.labelPLabelName = cln,
		C.labelColor = [r, g, b, a] }

data ObjectNameInfo n = ObjectNameInfo {
	objectNameInfoNext :: Maybe n,
	objectNameInfoObjectType :: ObjectType,
	objectNameInfoObjectHandle :: ObjectHandle,
	objectNameInfoObjectName :: T.Text }
	deriving Show

objectNameInfoToCore :: Pointable n => ObjectNameInfo n -> ContT r IO C.ObjectNameInfo
objectNameInfoToCore ObjectNameInfo {
	objectNameInfoNext = mnxt,
	objectNameInfoObjectType = ObjectType ot,
	objectNameInfoObjectHandle = ObjectHandle oh,
	objectNameInfoObjectName = on
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	con <- textToCString on
	pure C.ObjectNameInfo {
		C.objectNameInfoSType = (),
		C.objectNameInfoPNext = pnxt,
		C.objectNameInfoObjectType = ot,
		C.objectNameInfoObjectHandle = oh,
		C.objectNameInfoPObjectName = con }
