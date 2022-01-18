{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.SpecializationInfo where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Cont
import Data.Word

import Vulkan.Base
import qualified Vulkan.SpecializationInfo.Internal as I

#include <vulkan/vulkan.h>

data SpecializationInfo = SpecializationInfo {
	specializationInfoMapEntries :: [I.SpecializationMapEntry],
	specializationInfoDataSize :: #{type size_t},
	specializationInfoPData :: ForeignPtr () }
	deriving Show

specializationInfoToC ::
	SpecializationInfo -> (I.SpecializationInfo -> IO a) -> IO a
specializationInfoToC SpecializationInfo {
	specializationInfoMapEntries = mes,
	specializationInfoDataSize = sz,
	specializationInfoPData = fd } = runContT do
	pmes <- ContT $ allocaArray mec
	lift $ pokeArray pmes mes
	pd <- ContT $ withForeignPtr fd
	pure I.SpecializationInfo {
		I.specializationInfoMapEntryCount = fromIntegral mec,
		I.specializationInfoPMapEntries = pmes,
		I.specializationInfoDataSize = sz,
		I.specializationInfoPData = pd }
	where mec = length mes

buildSpecializationInfo ::
	(SpecializationInfoBuilder -> IO SpecializationInfoBuilder) ->
	IO SpecializationInfo
buildSpecializationInfo f = do
	SpecializationInfoBuilder {
		specializationInfoBuilderMapEntries = mes,
		specializationInfoBuilderDataSize = sz,
		specializationInfoBuilderPData = pd } <-
		f specializationInfoBuilderInit
	fd <- newForeignPtr pd $ free pd
	pure $ SpecializationInfo {
		specializationInfoMapEntries = mes,
		specializationInfoDataSize = fromIntegral sz,
		specializationInfoPData = fd }

data SpecializationInfoBuilder = SpecializationInfoBuilder {
	specializationInfoBuilderMapEntries :: [I.SpecializationMapEntry],
	specializationInfoBuilderDataSize :: Int,
	specializationInfoBuilderPData :: Ptr (),
	specializationInfoBuilderOffset :: Int }

specializationInfoBuilderInit :: SpecializationInfoBuilder
specializationInfoBuilderInit = SpecializationInfoBuilder {
	specializationInfoBuilderMapEntries = [],
	specializationInfoBuilderDataSize = 0,
	specializationInfoBuilderPData = NullPtr,
	specializationInfoBuilderOffset = 0 }

pushSpecConstant :: Storable a =>
	SpecializationInfoBuilder -> #{type uint32_t} -> a ->
	IO SpecializationInfoBuilder
pushSpecConstant SpecializationInfoBuilder {
	specializationInfoBuilderMapEntries = mes,
	specializationInfoBuilderDataSize = sz,
	specializationInfoBuilderPData = pd,
	specializationInfoBuilderOffset = os } i x = do
	let	me = I.SpecializationMapEntry i
			(fromIntegral os) (fromIntegral $ sizeOf x)
		sz' = sz + sizeOf x
		os' = os + sizeOf x
	pd' <- reallocBytes pd sz'
	poke (pd' `plusPtr` fromIntegral os) x
	pure $ SpecializationInfoBuilder {
		specializationInfoBuilderMapEntries = me : mes,
		specializationInfoBuilderDataSize = sz',
		specializationInfoBuilderPData = pd',
		specializationInfoBuilderOffset = os' }
