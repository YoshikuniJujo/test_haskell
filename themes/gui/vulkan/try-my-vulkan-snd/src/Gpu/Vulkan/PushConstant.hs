{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PushConstant where

import Foreign.Storable
import Data.Kind
import Data.Word

class IsPrefixOf (part :: [Type]) (whole :: [Type])

instance IsPrefixOf '[] whole

instance IsPrefixOf part whole => IsPrefixOf (t ': part) (t ': whole)

class Size (ts :: [Type]) where sizeSize :: Int -> Int

instance Size '[] where sizeSize sz = sz

instance (Storable t, Size ts) => Size (t ': ts) where
	sizeSize sz = ((sz - 1) `div` al + 1) * al + sizeOf @t undefined
		where al = alignment @t undefined

class OffsetSize (whole :: [Type]) (part :: [Type]) where
	offset :: Word32 -> Word32
	size :: Word32

instance (IsPrefixOf (t ': part) whole, Size (t ': part), Storable t) =>
	OffsetSize whole (t ': part) where
	offset oft = ((oft - 1) `div` al + 1) * al
		where al = fromIntegral $ alignment @t undefined
	size = fromIntegral $ sizeSize @(t ': part) 0

instance (Storable t, OffsetSize whole part) =>
	OffsetSize (t ': whole) part where
	offset oft = ((oft - 1) `div` al + 1) * al +
		fromIntegral (sizeOf @t undefined)
		where al = fromIntegral $ alignment @t undefined
	size = size @whole @part
