{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.Memory.Buffer.TypeLevel where

import Foreign.Storable
import Data.Kind.Object
import Data.HeteroList

import Gpu.Vulkan.Device.Middle
import Gpu.Vulkan.Device.Memory.Buffer.Types

class OffsetSize (obj :: Object) (objss :: [[Object]]) where
	offsetSize :: Size -> HeteroVarList Form objss -> (Size, Size)
	offsetSizeLength :: HeteroVarList Form objss -> ObjectLength obj

instance SizeAlignment obj =>
	OffsetSize obj ((obj ': objs) ': objss) where
	offsetSize n (Form ost _ (ln :...: _) :...: _) =
		(ost', fromIntegral $ objectSize ln)
		where
		ost' = ((ost + n - 1) `div` algn + 1) * algn
		algn = fromIntegral (objectAlignment @obj)
	offsetSizeLength (Form _ _ (ln :...: _) :...: _) = ln

instance {-# OVERLAPPABLE #-} (
	SizeAlignment obj',
	OffsetSize obj (objs ': objss) ) =>
	OffsetSize obj ((obj' ': objs) ': objss) where
	offsetSize n (Form ost sz (ln :...: lns) :...: fs) =
		offsetSize @obj n' (Form ost sz lns :...: fs)
		where
		n' = ((n - 1) `div` algn + 1) * algn + sz'
		sz' = fromIntegral $ objectSize ln
		algn = fromIntegral (objectAlignment @obj')
	offsetSizeLength (Form ost sz (_ :...: lns) :...: fs) =
		offsetSizeLength @obj (Form ost sz lns :...: fs)

instance {-# OVERLAPPABLE #-} (OffsetSize obj objss) =>
	OffsetSize obj ('[] ': objss) where
	offsetSize n (_ :...: fs) = offsetSize @obj 0 fs
	offsetSizeLength (_ :...: fs) = offsetSizeLength @obj fs
