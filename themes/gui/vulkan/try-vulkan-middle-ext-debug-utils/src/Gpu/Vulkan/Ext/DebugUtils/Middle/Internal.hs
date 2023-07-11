{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Ext.DebugUtils.Middle.Internal (

	-- * EXTENSION NAME

	extensionName,

	-- * LABEL AND OBJECT NAME INFO

	Label(..), labelFromCore,
	ObjectNameInfo(..), objectNameInfoFromCore,
	ObjectNameInfoNoNext(..), objectNameInfoNoNextFromCore,
	ObjectNameInfoResult(..), objectNameInfoResultFromCore

	) where

import Foreign.Ptr
import Foreign.Storable.PeekPoke
import Foreign.C.String
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.Maybe
import Data.String
import System.IO.Unsafe

import qualified Data.Text as T
import Data.Text.Foreign.Misc as T

import Data.Color

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Base.Middle.Internal

import qualified Gpu.Vulkan.Ext.DebugUtils.Core as C

import Data.HeteroParList qualified as HeteroParList
import Gpu.Vulkan.PNext.Middle.Internal

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

data ObjectNameInfo mn = ObjectNameInfo {
	objectNameInfoNext :: TMaybe.M mn,
	objectNameInfoObjectType :: ObjectType,
	objectNameInfoObjectHandle :: ObjectHandle,
	objectNameInfoObjectName :: Maybe T.Text }

deriving instance Show (TMaybe.M mn) => Show (ObjectNameInfo mn)

objectNameInfoFromCore :: Peek (TMaybe.M mn) => C.ObjectNameInfo -> IO (ObjectNameInfo mn)
objectNameInfoFromCore C.ObjectNameInfo {
	C.objectNameInfoPNext = pnxt,
	C.objectNameInfoObjectType = ot,
	C.objectNameInfoObjectHandle = oh,
	C.objectNameInfoPObjectName = con
	} = do
	mnxt <- peek' $ castPtr pnxt
	mon <- case con of
		NullPtr -> pure Nothing
		p -> Just <$> cStringToText p
	pure ObjectNameInfo {
		objectNameInfoNext = mnxt,
		objectNameInfoObjectType = ObjectType ot,
		objectNameInfoObjectHandle = ObjectHandle oh,
		objectNameInfoObjectName = mon }

data ObjectNameInfoNoNext = ObjectNameInfoNoNext {
	objectNameInfoNoNextObjectType :: ObjectType,
	objectNameInfoNoNextObjectHandle :: ObjectHandle,
	objectNameInfoNoNextObjectName :: Maybe T.Text }
	deriving Show

objectNameInfoNoNextFromCore :: C.ObjectNameInfo -> IO ObjectNameInfoNoNext
objectNameInfoNoNextFromCore C.ObjectNameInfo {
	C.objectNameInfoPNext = _pnxt,
	C.objectNameInfoObjectType = ot,
	C.objectNameInfoObjectHandle = oh,
	C.objectNameInfoPObjectName = con
	} = do
	mon <- case con of
		NullPtr -> pure Nothing
		p -> Just <$> cStringToText p
	pure ObjectNameInfoNoNext {
		objectNameInfoNoNextObjectType = ObjectType ot,
		objectNameInfoNoNextObjectHandle = ObjectHandle oh,
		objectNameInfoNoNextObjectName = mon }

data ObjectNameInfoResult ns = ObjectNameInfoResult {
	objectNameInfoResultNextList :: HeteroParList.PL Maybe ns,
	objectNameInfoResultObjectType :: ObjectType,
	objectNameInfoResultObjectHandle :: ObjectHandle,
	objectNameInfoResultObjectName :: Maybe T.Text }

deriving instance Show (HeteroParList.PL Maybe ns) =>
	Show (ObjectNameInfoResult ns)

objectNameInfoResultFromCore ::
	FindPNextChainAll ns => C.ObjectNameInfo -> IO (ObjectNameInfoResult ns)
objectNameInfoResultFromCore C.ObjectNameInfo {
	C.objectNameInfoPNext = pnxt,
	C.objectNameInfoObjectType = ot,
	C.objectNameInfoObjectHandle = oh,
	C.objectNameInfoPObjectName = con
	} = do
	mon <- case con of
		NullPtr -> pure Nothing
		p -> Just <$> cStringToText p
	nxts <- findPNextChainAll pnxt
	pure ObjectNameInfoResult {
		objectNameInfoResultNextList = nxts,
		objectNameInfoResultObjectType = ObjectType ot,
		objectNameInfoResultObjectHandle = ObjectHandle oh,
		objectNameInfoResultObjectName = mon }
