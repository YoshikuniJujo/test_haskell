{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.ForeignPtr hiding (newForeignPtr, addForeignPtrFinalizer)
import Foreign.Concurrent
import Foreign.C.String
import Foreign.C.Struct
import Control.Exception
import Data.Word

import Vulkan.Exception

import qualified Vulkan.Internal as I

#include <vulkan/vulkan.h>

data ApplicationInfo a = ApplicationInfo {
	applicationInfoNext :: a,
	applicationInfoApplicationName :: String,
	applicationInfoApplicationVersion :: I.ApiVersion,
	applicationInfoEngineName :: String,
	applicationInfoEngineVersion :: I.ApiVersion,
	applicationInfoApiVersion :: I.ApiVersion }
	deriving Show

class Pointable a where
	withPointer :: a -> (Ptr a -> IO b) -> IO b
	fromPointer :: Ptr a -> IO a

instance {-# OVERLAPPABLE #-} Storable a => Pointable a where
	withPointer x f = alloca \p -> poke p x >> f p
	fromPointer = peek

withApplicationInfo :: Pointable a =>
	ApplicationInfo a -> (I.ApplicationInfo -> IO b) -> IO b
withApplicationInfo ai f = withPointer (applicationInfoNext ai) \pnxt ->
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

pokeCString :: Int -> CString -> String -> IO ()
pokeCString n cs str = withCString str \cs_ -> copyBytes cs cs_ n

struct "ExtensionProperties" #{size VkExtensionProperties} [
	("extensionName", ''String,
		[| peekCString . #{ptr VkExtensionProperties, extensionName} |],
		[| \p s -> pokeCString
			#{const VK_MAX_EXTENSION_NAME_SIZE}
			(#{ptr VkExtensionProperties, extensionName} p) s |]),
	("specVersion", ''#{type uint32_t}, [| #{peek VkExtensionProperties, specVersion} |],
		[| #{poke VkExtensionProperties, specVersion} |])
	]
	[''Show, ''Read, ''Eq]

instance Storable ExtensionProperties where
	sizeOf _ = #{size VkExtensionProperties}
	alignment _ = #{alignment VkExtensionProperties}
	peek ps = do
		pd <- malloc
		copyBytes pd ps #{size VkExtensionProperties}
		ExtensionProperties_ <$> newForeignPtr pd (free pd)
	poke pd (ExtensionProperties_ fps) = withForeignPtr fps \ps -> copyBytes pd ps #{size VkExtensionProperties}

enumerateInstanceExtensionProperties :: Maybe String -> IO [ExtensionProperties]
enumerateInstanceExtensionProperties =
	flip withMaybeCString \cs -> alloca \pn -> do
		r <- c_vkEnumerateInstanceExtensionProperties cs pn nullPtr
		case r of Success -> pure (); _ -> throw r
		n <- peek pn
		allocaArray (fromIntegral n) \pProps -> do
			r' <- c_vkEnumerateInstanceExtensionProperties cs pn pProps
			case r' of Success -> pure (); _ -> throw r'
			peekArray (fromIntegral n) pProps

withMaybeCString :: Maybe String -> (CString -> IO a) -> IO a
withMaybeCString mstr f = case mstr of
	Nothing -> f nullPtr
	Just str -> withCString str f

foreign import ccall "vkEnumerateInstanceExtensionProperties"
	c_vkEnumerateInstanceExtensionProperties ::
	CString -> Ptr #{type uint32_t} -> Ptr ExtensionProperties -> IO Result
