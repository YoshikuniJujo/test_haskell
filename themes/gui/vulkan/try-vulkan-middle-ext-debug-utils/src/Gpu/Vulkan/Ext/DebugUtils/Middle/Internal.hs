{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Ext.DebugUtils.Middle.Internal (
	extensionName,
	Label(..), labelFromCore,
	ObjectNameInfo(..), objectNameInfoFromCore,
	ObjectNameInfoResult(..), objectNameInfoResultFromCore ) where

import Foreign.Ptr
import Foreign.Storable.PeekPoke
import Foreign.C.String
import Data.Maybe
import Data.String
import System.IO.Unsafe

import qualified Data.Text as T
import Data.Text.Foreign.Misc as T

import Data.Color

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Base.Middle.Internal
-- import Gpu.Vulkan.Misc.Middle

import qualified Gpu.Vulkan.Ext.DebugUtils.Core as C

foreign import capi "vulkan/vulkan.h value VK_EXT_DEBUG_UTILS_EXTENSION_NAME"
	c_extensionName :: CString

extensionName :: IsString s => s
extensionName = unsafePerformIO $ fromString <$> peekCString c_extensionName

data Label = Label {
	labelLabelName :: T.Text,
	labelColor :: Rgba Float }
	deriving Show

labelFromCore :: C.Label -> IO Label
labelFromCore C.Label {
	C.labelPNext = _pnxt,
	C.labelPLabelName = cln,
	C.labelColor = [r, g, b, a] } = do
	ln <- cStringToText cln
	pure Label {
		labelLabelName = ln,
		labelColor = fromJust $ rgbaDouble r g b a }
labelFromCore _ = error "C.labelColor should be [r, g, b, a]"

data ObjectNameInfo n = ObjectNameInfo {
	objectNameInfoNext :: Maybe n,
	objectNameInfoObjectType :: ObjectType,
	objectNameInfoObjectHandle :: ObjectHandle,
	objectNameInfoObjectName :: Maybe T.Text }
	deriving Show

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
		p -> Just <$> cStringToText p
	pure ObjectNameInfo {
		objectNameInfoNext = mnxt,
		objectNameInfoObjectType = ObjectType ot,
		objectNameInfoObjectHandle = ObjectHandle oh,
		objectNameInfoObjectName = mon }

data ObjectNameInfoResult = ObjectNameInfoResult {
	objectNameInfoResultObjectType :: ObjectType,
	objectNameInfoResultObjectHandle :: ObjectHandle,
	objectNameInfoResultObjectName :: Maybe T.Text }
	deriving Show

objectNameInfoResultFromCore :: C.ObjectNameInfo -> IO ObjectNameInfoResult
objectNameInfoResultFromCore C.ObjectNameInfo {
	C.objectNameInfoPNext = _pnxt,
	C.objectNameInfoObjectType = ot,
	C.objectNameInfoObjectHandle = oh,
	C.objectNameInfoPObjectName = con
	} = do
	mon <- case con of
		NullPtr -> pure Nothing
		p -> Just <$> cStringToText p
	pure ObjectNameInfoResult {
		objectNameInfoResultObjectType = ObjectType ot,
		objectNameInfoResultObjectHandle = ObjectHandle oh,
		objectNameInfoResultObjectName = mon }
