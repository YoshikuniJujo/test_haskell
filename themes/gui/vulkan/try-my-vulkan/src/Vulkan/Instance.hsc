{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Instance where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String

import Vulkan.Base
import Vulkan.Exception
import Vulkan.AllocationCallbacks

import qualified Vulkan.Internal as I
import qualified Vulkan.AllocationCallbacks.Internal as I

data InstanceCreateInfo a b = InstanceCreateInfo {
	instanceCreateInfoNext :: Maybe a,
	instanceCreateInfoFlags :: I.InstanceCreateFlags,
	instanceCreateInfoApplicationInfo :: ApplicationInfo b,
	instanceCreateInfoEnabledLayers :: [String],
	instanceCreateInfoEnabledExtensions :: [String] }
	deriving Show

withInstanceCreateInfoPtr :: (Pointable a, Pointable b) =>
	InstanceCreateInfo a b -> (Ptr I.InstanceCreateInfo -> IO c) -> IO c
withInstanceCreateInfoPtr ici f =
	withInstanceCreateInfo ici \(I.InstanceCreateInfo_ fici) ->
		withForeignPtr fici f

withInstanceCreateInfo :: (Pointable a, Pointable b) =>
	InstanceCreateInfo a b -> (I.InstanceCreateInfo -> IO c) -> IO c
withInstanceCreateInfo ic f = withMaybePointer (instanceCreateInfoNext ic) \pnxt ->
	withApplicationInfo (instanceCreateInfoApplicationInfo ic) \(I.ApplicationInfo_ fai) ->
		I.withCStrings (instanceCreateInfoEnabledLayers ic) \eln els ->
			I.withCStrings (instanceCreateInfoEnabledExtensions ic) \en es ->
				withForeignPtr fai \pai ->
					f $ I.InstanceCreateInfo () (castPtr pnxt)
						(instanceCreateInfoFlags ic) pai
						eln els en es

newtype Instance = Instance (Ptr Instance) deriving (Show, Storable)

createInstance :: (Pointable a, Pointable b, Storable c) =>
	InstanceCreateInfo a b -> Maybe (AllocationCallbacks c) -> IO Instance
createInstance ici mac = alloca \pist -> withInstanceCreateInfoPtr ici \pici -> do
	r <- case mac of
		Nothing -> c_vkCreateInstance pici nullPtr pist
		Just ac -> withAllocationCallbacksPtr ac \pac ->
			c_vkCreateInstance pici pac pist
	throwUnlessSuccess r
	peek pist

foreign import ccall "vkCreateInstance" c_vkCreateInstance ::
	Ptr I.InstanceCreateInfo -> Ptr I.AllocationCallbacks -> Ptr Instance ->
	IO Result

data ApplicationInfo a = ApplicationInfo {
	applicationInfoNext :: Maybe a,
	applicationInfoApplicationName :: String,
	applicationInfoApplicationVersion :: I.ApiVersion,
	applicationInfoEngineName :: String,
	applicationInfoEngineVersion :: I.ApiVersion,
	applicationInfoApiVersion :: I.ApiVersion }
	deriving Show

pattern NullPtr :: Ptr a
pattern NullPtr <- ((== nullPtr) -> True) where NullPtr = nullPtr

pattern NullFunPtr :: FunPtr a
pattern NullFunPtr <- ((== nullFunPtr) -> True) where NullFunPtr = nullFunPtr

withMaybePointer :: Pointable a => Maybe a -> (Ptr a -> IO b) -> IO b
withMaybePointer mx f = case mx of
	Nothing -> f nullPtr
	Just x -> withPointer x f

withApplicationInfo :: Pointable a =>
	ApplicationInfo a -> (I.ApplicationInfo -> IO b) -> IO b
withApplicationInfo ai f = withMaybePointer (applicationInfoNext ai) \pnxt ->
	withCString (applicationInfoApplicationName ai) \canm ->
		withCString (applicationInfoEngineName ai) \cenm ->
			f I.ApplicationInfo {
				I.applicationInfoSType = (),
				I.applicationInfoPNext = castPtr pnxt,
				I.applicationInfoPApplicationName = canm,
				I.applicationInfoApplicationVersion =
					applicationInfoApplicationVersion ai,
				I.applicationInfoPEngineName = cenm,
				I.applicationInfoEngineVersion =
					applicationInfoEngineVersion ai,
				I.applicationInfoApiVersion =
					applicationInfoApiVersion ai }
