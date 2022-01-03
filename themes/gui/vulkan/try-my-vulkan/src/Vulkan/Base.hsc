{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Base where

import Prelude hiding (Bool(..))
import qualified Prelude as P

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Enum
import Foreign.C.Struct
import Data.Word

#include <vulkan/vulkan.h>

type PtrVoid = Ptr ()

type PtrCFloat = Ptr #{type float}

type ListCFloat = [#{type float}]

type PtrCString = Ptr CString

pattern NullPtr :: Ptr a
pattern NullPtr <- ((== nullPtr) -> P.True) where NullPtr = nullPtr

pattern NullFunPtr :: FunPtr a
pattern NullFunPtr <- ((== nullFunPtr) -> P.True) where NullFunPtr = nullFunPtr

withMaybePointer :: Pointable a => Maybe a -> (Ptr a -> IO b) -> IO b
withMaybePointer mx f = case mx of
	Nothing -> f nullPtr
	Just x -> withPointer x f

class Pointable a where
	withPointer :: a -> (Ptr a -> IO b) -> IO b
	fromPointer :: Ptr a -> IO a

instance {-# OVERLAPPABLE #-} Storable a => Pointable a where
	withPointer x f = alloca \p -> poke p x >> f p
	fromPointer = peek

withPointerMaybe :: Pointable a => Maybe a -> (Ptr a -> IO b) -> IO b
withPointerMaybe mx f = maybe (f NullPtr) (`withPointer` f) mx

pokeCStringLen :: Int -> CString -> String -> IO ()
pokeCStringLen n cs str = withCString str \cs_ -> copyBytes cs cs_ n

pokeCString :: CString -> String -> IO ()
pokeCString cs str = withCStringLen str \(cs_, ln) -> do
	copyBytes cs cs_ ln
	poke (cs `plusPtr` ln :: CString) 0

withMaybeCString :: Maybe String -> (CString -> IO a) -> IO a
withMaybeCString mstr f = case mstr of
	Nothing -> f nullPtr
	Just str -> withCString str f

enum "Bool32" ''#{type VkBool32} [''Show, ''Storable] [
	("False", #{const VK_FALSE}), ("True", #{const VK_TRUE}) ]

struct "ExtensionProperties" #{size VkExtensionProperties}
		#{alignment VkExtensionProperties} [
	("extensionName", ''String,
		[| peekCString . #{ptr VkExtensionProperties, extensionName} |],
		[| \p s -> pokeCStringLen
			#{const VK_MAX_EXTENSION_NAME_SIZE}
			(#{ptr VkExtensionProperties, extensionName} p) s |]),
	("specVersion", ''#{type uint32_t}, [| #{peek VkExtensionProperties, specVersion} |],
		[| #{poke VkExtensionProperties, specVersion} |])
	]
	[''Show, ''Read, ''Eq, ''Storable]

struct "Extent2D" #{size VkExtent2D} #{alignment VkExtent2D} [
	("width", ''#{type uint32_t}, [| #{peek VkExtent2D, width} |],
		[| #{poke VkExtent2D, width} |]),
	("height", ''#{type uint32_t}, [| #{peek VkExtent2D, height} |],
		[| #{poke VkExtent2D, height} |]) ]
	[''Show, ''Storable]

struct "Extent3D" #{size VkExtent3D} #{alignment VkExtent3D} [
	("width", ''#{type uint32_t}, [| #{peek VkExtent3D, width} |],
		[| #{poke VkExtent3D, width} |]),
	("height", ''#{type uint32_t}, [| #{peek VkExtent3D, height} |],
		[| #{poke VkExtent3D, height} |]),
	("depth", ''#{type uint32_t}, [| #{peek VkExtent3D, depth} |],
		[| #{poke VkExtent3D, depth} |]) ]
	[''Show, ''Storable]

enum "Format" ''#{type VkFormat} [''Show, ''Eq, ''Storable] [
	("FormatUndefined", #{const VK_FORMAT_UNDEFINED}),
	("FormatR4g4UnormPack8", #{const VK_FORMAT_R4G4_UNORM_PACK8}),
	("FormatR4g4b4a4UnormPack16", #{const VK_FORMAT_R4G4B4A4_UNORM_PACK16}),
	("FormatB4g4r4a4UnormPack16", #{const VK_FORMAT_B4G4R4A4_UNORM_PACK16}),
	-- VK_FORMAT_R5G6B5_UNORM_PACK16
	-- VK_FORMAT_B5G6R5_UNORM_PACK16
	-- VK_FORMAT_R5G5B5A1_UNORM_PACK16
	-- VK_FORMAT_B5G5R5A1_UNORM_PACK16
	-- VK_FORMAT_A1R5G5B5_UNORM_PACK16
	-- ...
	-- VK_FORMAT_R8G8B8A8_SSCALED
	-- VK_FORMAT_R8G8B8A8_UINT
	-- VK_FORMAT_R8G8B8A8_SINT
	-- VK_FORMAT_R8G8B8A8_SRGB
	("FormatB8g8r8a8Unorm", #{const VK_FORMAT_B8G8R8A8_UNORM}),
	-- VK_FORMAT_B8G8R8A8_SNORM
	-- ...
	-- VK_FORMAT_B8G8R8A8_SINT
	("FormatB8g8r8a8Srgb", #{const VK_FORMAT_B8G8R8A8_SRGB}),
	-- VK_FORMAT_A8B8G8R8_UNORM_PACK32
	-- ...
	-- VK_FORMAT_G16_B16R16_2PLANE_422_UNORM_KHR
	("FormatG16B16R163Plane444Unorm",
		#{const VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM})
	]

uint32Max :: Integral n => n
uint32Max = #{const UINT32_MAX}

enum "SharingMode" ''#{type VkSharingMode} [''Show, ''Storable] [
	("SharingModeExclusive", #{const VK_SHARING_MODE_EXCLUSIVE}),
	("SharingModeConcurrent", #{const VK_SHARING_MODE_CONCURRENT}) ]
