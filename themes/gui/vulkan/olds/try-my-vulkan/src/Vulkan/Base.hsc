{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Data.Int

import qualified Data.Bool as B

#include <vulkan/vulkan.h>

type PtrVoid = Ptr ()

type PtrCFloat = Ptr #{type float}

type ListCFloat = [#{type float}]

type PtrCString = Ptr CString

type PtrUint32T = Ptr #{type uint32_t}

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

boolToBool32 :: B.Bool -> Bool32
boolToBool32 = B.bool False True

bool32ToBool :: Bool32 -> B.Bool
bool32ToBool = \case False -> B.False; _ -> B.True

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

struct "Extent3D" #{size VkExtent3D} #{alignment VkExtent3D} [
	("width", ''#{type uint32_t}, [| #{peek VkExtent3D, width} |],
		[| #{poke VkExtent3D, width} |]),
	("height", ''#{type uint32_t}, [| #{peek VkExtent3D, height} |],
		[| #{poke VkExtent3D, height} |]),
	("depth", ''#{type uint32_t}, [| #{peek VkExtent3D, depth} |],
		[| #{poke VkExtent3D, depth} |]) ]
	[''Show, ''Storable]

uint32Max :: Integral n => n
uint32Max = #{const UINT32_MAX}

enum "SharingMode" ''#{type VkSharingMode} [''Show, ''Storable] [
	("SharingModeExclusive", #{const VK_SHARING_MODE_EXCLUSIVE}),
	("SharingModeConcurrent", #{const VK_SHARING_MODE_CONCURRENT}) ]

struct "Offset2d" #{size VkOffset2D} #{alignment VkOffset2D} [
	("x", ''#{type int32_t},
		[| #{peek VkOffset2D, x} |], [| #{poke VkOffset2D, x} |]),
	("y", ''#{type int32_t},
		[| #{peek VkOffset2D, y} |], [| #{poke VkOffset2D, y} |]) ]
	[''Show, ''Storable]

struct "Extent2d" #{size VkExtent2D} #{alignment VkExtent2D} [
	("width", ''#{type uint32_t},
		[| #{peek VkExtent2D, width} |],
		[| #{poke VkExtent2D, width} |]),
	("height", ''#{type uint32_t},
		[| #{peek VkExtent2D, height} |],
		[| #{poke VkExtent2D, height} |]) ]
	[''Show, ''Storable]

struct "Rect2d" #{size VkRect2D} #{alignment VkRect2D} [
	("offset", ''Offset2d,
		[| #{peek VkRect2D, offset} |], [| #{poke VkRect2D, offset} |]),
	("extent", ''Extent2d,
		[| #{peek VkRect2D, extent} |], [| #{poke VkRect2D, extent} |])
	]
	[''Show, ''Storable]

type PtrRect2d = Ptr Rect2d

newtype SampleMask = SampleMask #{type VkSampleMask} deriving (Show, Storable)

type PtrSampleMask = Ptr SampleMask

data PipelineTag
newtype Pipeline vs (ts :: [*]) = Pipeline (Ptr PipelineTag) deriving (Show, Storable)
newtype PipelineC = PipelineC (Ptr PipelineTag) deriving (Show, Storable)

pipelineToC :: Pipeline vs ts -> PipelineC
pipelineToC (Pipeline p) = PipelineC p

pattern NullHandle :: Ptr a
pattern NullHandle <- (ptrToWordPtr -> (WordPtr #{const VK_NULL_HANDLE})) where
	NullHandle = wordPtrToPtr $ WordPtr #{const VK_NULL_HANDLE}

pattern PipelineNullHandle :: Pipeline vs ts
pattern PipelineNullHandle <- Pipeline NullHandle where
	PipelineNullHandle = Pipeline NullHandle

word32ToUint32T :: Word32 -> #{type uint32_t}
word32ToUint32T = fromIntegral

int32ToInt32T :: Int32 -> #{type int32_t}
int32ToInt32T = fromIntegral

floatToFloat :: Float -> #{type float}
floatToFloat = realToFrac

data RenderPassTag
newtype RenderPass = RenderPass (Ptr RenderPassTag) deriving (Show, Storable)

uint64Max :: #{type uint64_t}
uint64Max = #{const UINT64_MAX}

subpassExternal :: #{type uint32_t}
subpassExternal = #{const VK_SUBPASS_EXTERNAL}
