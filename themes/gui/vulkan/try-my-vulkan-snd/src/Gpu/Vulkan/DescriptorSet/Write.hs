{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet.Write (

	-- * WRITE

	Write(..), WriteListToMiddle(..),
	WriteSources(..), WriteSourcesArg(..), WriteSourcesToMiddle,

	) where

import Foreign.Storable.PeekPoke
import Data.TypeLevel.Tuple.MapIndex qualified as TMapIndex
import Data.IORef
import Gpu.Vulkan.Object qualified as VObj
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.Tuple.Uncurry
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:**))

import Gpu.Vulkan.DescriptorSet.Type
import Gpu.Vulkan.DescriptorSet.TypeLevel.Common

import qualified Gpu.Vulkan.Descriptor.Enum as Descriptor
import qualified Gpu.Vulkan.DescriptorSet.Middle as M

import Gpu.Vulkan.DescriptorSet.Write.Sources

data Write mn sds slbts writeSourcesArg = Write {
	writeNext :: TMaybe.M mn,
	writeDstSet :: D sds slbts,
	writeDescriptorType :: Descriptor.Type,
	writeSources :: WriteSources writeSourcesArg }

class M.WriteListToCore (TMapIndex.M0_4 writeArgs) =>
	WriteListToMiddle writeArgs where
	writeListToMiddle ::
		HeteroParList.PL (U4 Write) writeArgs ->
		HeteroParList.PL M.Write (TMapIndex.M0_4 writeArgs)
	writeListUpdateDynamicLength ::
		HeteroParList.PL (U4 Write) writeArgs -> IO ()

instance WriteListToMiddle '[] where
	writeListToMiddle HeteroParList.Nil = HeteroParList.Nil
	writeListUpdateDynamicLength HeteroParList.Nil = pure ()

instance (
	WithPoked (TMaybe.M mn),
	WriteSourcesToMiddle '(sl, bts) wsa,

	WriteListToMiddle writeArgs
	) =>
	WriteListToMiddle ('(mn, s, '(sl, bts), wsa) ': writeArgs) where
	writeListToMiddle (U4 w :** ws) =
		writeToMiddle w :** writeListToMiddle ws
	writeListUpdateDynamicLength (U4 w :** ws) =
		writeUpdateDynamicLength w >> writeListUpdateDynamicLength ws

writeToMiddle :: forall n s slbts wsa . WriteSourcesToMiddle slbts wsa =>
	Write n s slbts wsa -> M.Write n
writeToMiddle Write {
	writeNext = mnxt,
	writeDstSet = D _ ds,
	writeDescriptorType = dt,
	writeSources = srcs
	} = M.Write {
		M.writeNext = mnxt,
		M.writeDstSet = ds,
		M.writeDstBinding = bdg,
		M.writeDstArrayElement = ae,
		M.writeDescriptorType = dt,
		M.writeSources = srcs' }
	where ((bdg, ae), srcs') = writeSourcesToMiddle @slbts srcs

writeUpdateDynamicLength :: forall sbsmobjsobjs n s sl bts . (
	WriteSourcesToLengthList sbsmobjsobjs,
	BindingAndArrayElem bts (WriteSourcesToLengthListObj sbsmobjsobjs) 0
	) =>
	Write n s '(sl, bts) sbsmobjsobjs -> IO ()
writeUpdateDynamicLength Write {
	writeDstSet = D rlns _,
	writeSources = ws } = do
	lns <- readIORef rlns
	maybe	(pure ())
		(writeIORef rlns . updateDynamicLength @bts @(WriteSourcesToLengthListObj sbsmobjsobjs) @0 lns
			. (VObj.onlyDynamicLength @(WriteSourcesToLengthListObj sbsmobjsobjs)))
		(writeSourcesToLengthList @sbsmobjsobjs ws)
