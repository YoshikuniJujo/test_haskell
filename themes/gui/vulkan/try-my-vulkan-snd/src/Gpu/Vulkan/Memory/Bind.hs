{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Memory.Bind (

	-- * OFFSET

	offset, Offset

	) where

import Data.Kind
import Data.TypeLevel.Tuple.Uncurry
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**))

import Gpu.Vulkan.Device.Type qualified as Device
import Gpu.Vulkan.Device.Middle qualified as Device.M
import Gpu.Vulkan.Memory.Types

offset :: forall ib ibargs sd sm . Offset ib ibargs =>
	Device.D sd -> M sm ibargs -> Device.M.Size -> IO Device.M.Size
offset dvc m ost = do
	(ibs, _) <- readM m
	offset' @ib @ibargs dvc ibs ost

class Offset (ib :: ImageBufferArg) (ibargs :: [(Type, ImageBufferArg)]) where
	offset' :: Device.D sd -> HeteroParList.PL (U2 ImageBuffer) ibargs ->
		Device.M.Size -> IO Device.M.Size

instance Offset ib ('(sib, ib) ': ibargs) where
	offset' dvc (U2 ib :** _ibs) ost = fst <$> adjustOffsetSize dvc ib ost

instance {-# OVERLAPPABLE #-} Offset ib ibargs =>
	Offset ib ('(sib', ib') ': ibargs) where
	offset' dvc (U2 ib :** ibs) ost = do
		(ost', sz) <- adjustOffsetSize dvc ib ost
		offset' @ib dvc ibs $ ost' + sz
