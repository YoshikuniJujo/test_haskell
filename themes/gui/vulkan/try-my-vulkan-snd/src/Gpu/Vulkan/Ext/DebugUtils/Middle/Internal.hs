{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Ext.DebugUtils.Middle.Internal (
	extensionName,
	Label(..), labelFromCore,
	ObjectNameInfo(..), objectNameInfoFromCore
	) where

import Foreign.Ptr
import Foreign.Storable.PeekPoke
import Foreign.C.String
import Data.Maybe
import Data.String
import System.IO.Unsafe

import qualified Data.Text as T

import Data.Color

import Gpu.Vulkan.Middle.Internal
import Gpu.Vulkan.Enum
import Gpu.Vulkan.Misc.Middle.Internal

import qualified Gpu.Vulkan.Ext.DebugUtils.Core as C

foreign import capi "vulkan/vulkan.h value VK_EXT_DEBUG_UTILS_EXTENSION_NAME"
	c_extensionName :: CString

extensionName :: IsString s => s
extensionName = unsafePerformIO $ fromString <$> peekCString c_extensionName

data Label n = Label {
	labelNext :: Maybe n,
	labelLabelName :: T.Text,
	labelColor :: Rgba Float }
	deriving Show

{-
labelToCore :: Pointable n => Label n -> ContT r IO C.Label
labelToCore Label {
	labelNext = mnxt,
	labelLabelName = ln,
	labelColor = RgbaDouble r g b a } = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	cln <- ContT $ textToCString ln
	pure C.Label {
		C.labelSType = (),
		C.labelPNext = pnxt,
		C.labelPLabelName = cln,
		C.labelColor = [r, g, b, a] }
-}

labelFromCore :: Peek n => C.Label -> IO (Label n)
labelFromCore C.Label {
	C.labelPNext = pnxt,
	C.labelPLabelName = cln,
	C.labelColor = [r, g, b, a] } = do
	mnxt <- peekMaybe $ castPtr pnxt
	ln <- cstrToText cln
	pure Label {
		labelNext = mnxt,
		labelLabelName = ln,
		labelColor = fromJust $ rgbaDouble r g b a }
labelFromCore _ = error "C.labelColor should be [r, g, b, a]"

data ObjectNameInfo n = ObjectNameInfo {
	objectNameInfoNext :: Maybe n,
	objectNameInfoObjectType :: ObjectType,
	objectNameInfoObjectHandle :: ObjectHandle,
	objectNameInfoObjectName :: Maybe T.Text }
	deriving Show

{-
objectNameInfoToCore :: Pointable n => ObjectNameInfo n -> ContT r IO C.ObjectNameInfo
objectNameInfoToCore ObjectNameInfo {
	objectNameInfoNext = mnxt,
	objectNameInfoObjectType = ObjectType ot,
	objectNameInfoObjectHandle = ObjectHandle oh,
	objectNameInfoObjectName = mon
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	con <- maybe (pure NullPtr) (ContT . textToCString) mon
	pure C.ObjectNameInfo {
		C.objectNameInfoSType = (),
		C.objectNameInfoPNext = pnxt,
		C.objectNameInfoObjectType = ot,
		C.objectNameInfoObjectHandle = oh,
		C.objectNameInfoPObjectName = con }
-}

objectNameInfoFromCore :: Peek n => C.ObjectNameInfo -> IO (ObjectNameInfo n)
objectNameInfoFromCore C.ObjectNameInfo {
	C.objectNameInfoPNext = pnxt,
	C.objectNameInfoObjectType = ot,
	C.objectNameInfoObjectHandle = oh,
	C.objectNameInfoPObjectName = con
	} = do
	mnxt <- peekMaybe $ castPtr pnxt
	mon <- case con of
		NullPtr -> pure Nothing
		p -> Just <$> cstrToText p
	pure ObjectNameInfo {
		objectNameInfoNext = mnxt,
		objectNameInfoObjectType = ObjectType ot,
		objectNameInfoObjectHandle = ObjectHandle oh,
		objectNameInfoObjectName = mon }
