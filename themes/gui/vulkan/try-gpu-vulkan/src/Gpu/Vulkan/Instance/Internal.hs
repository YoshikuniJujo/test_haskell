{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Instance.Internal (

	-- * CREATE

	create, I(..), CreateInfo(..), ExtensionName(..),

	-- * ENUMERATE

	enumerateLayerProperties,
	enumerateExtensionProperties, ExtensionProperties(..),

	) where

import Foreign.Storable.PeekPoke
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry
import Data.Default

import Gpu.Vulkan.Internal
import Gpu.Vulkan.Instance.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import Gpu.Vulkan.Instance.Enum
import qualified Gpu.Vulkan.Instance.Middle as M

import Data.Text qualified as T
import Gpu.Vulkan.Middle qualified as M

create :: (
	WithPoked (TMaybe.M mn), WithPoked (TMaybe.M ai),
	AllocationCallbacks.ToMiddle mac ) =>
	CreateInfo mn ai ->
	TPMaybe.M (U2 AllocationCallbacks.A) mac ->
	(forall s . I s -> IO a) -> IO a
create (createInfoToMiddle -> ci) (AllocationCallbacks.toMiddle -> mac) f =
	bracket (M.create ci mac) (`M.destroy` mac) (f . I)

data CreateInfo mn ai = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoApplicationInfo :: Maybe (ApplicationInfo ai),
	createInfoEnabledLayerNames :: [LayerName],
	createInfoEnabledExtensionNames :: [ExtensionName] }

newtype ExtensionName = ExtensionName { unExtensionName :: T.Text }
	deriving (Show, Eq)

deriving instance (Show (TMaybe.M mn), Show (TMaybe.M ai)) =>
	Show (CreateInfo mn ai)

instance Default (CreateInfo 'Nothing a) where
	def = CreateInfo {
		createInfoNext = TMaybe.N,
		createInfoFlags = CreateFlagsZero,
		createInfoApplicationInfo = Nothing,
		createInfoEnabledLayerNames = [],
		createInfoEnabledExtensionNames = [] }

createInfoToMiddle :: CreateInfo mn ai -> M.CreateInfo mn ai
createInfoToMiddle CreateInfo {
	createInfoNext = nxt,
	createInfoFlags = flgs,
	createInfoApplicationInfo = ai,
	createInfoEnabledLayerNames = lns,
	createInfoEnabledExtensionNames = ens } = M.CreateInfo {
	M.createInfoNext = nxt,
	M.createInfoFlags = flgs,
	M.createInfoApplicationInfo = ai,
	M.createInfoEnabledLayerNames = unLayerName <$> lns,
	M.createInfoEnabledExtensionNames = unExtensionName <$> ens }

enumerateLayerProperties :: IO [LayerProperties]
enumerateLayerProperties =
	(layerPropertiesFromMiddle <$>) <$> M.enumerateLayerProperties

enumerateExtensionProperties ::
	Maybe LayerName -> IO [ExtensionProperties]
enumerateExtensionProperties (((\(LayerName ln) -> ln) <$>) -> mln) =
	(extensionPropertiesFromMiddle <$>)
		<$> M.enumerateExtensionProperties mln

data ExtensionProperties = ExtensionProperties {
	extensionPropertiesExtensionName :: ExtensionName,
	extensionPropertiesSpecVersion :: M.ApiVersion }
	deriving Show

extensionPropertiesFromMiddle :: M.ExtensionProperties -> ExtensionProperties
extensionPropertiesFromMiddle M.ExtensionProperties {
	M.extensionPropertiesExtensionName = en,
	M.extensionPropertiesSpecVersion = sv } = ExtensionProperties {
	extensionPropertiesExtensionName = ExtensionName en,
	extensionPropertiesSpecVersion = sv }
