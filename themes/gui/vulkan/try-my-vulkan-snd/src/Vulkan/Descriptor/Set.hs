{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Descriptor.Set where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.Word

import qualified Vulkan.Descriptor.Pool as Pool
import qualified Vulkan.Descriptor.Set.Layout as Layout
import qualified Vulkan.Descriptor.Set.Core as C

data AllocateInfo n = AllocateInfo {
	allocateInfoNext :: Maybe n,
	allocateInfoDescriptorPool :: Pool.P,
	allocateInfoDescriptorSetCountOrSetLayouts :: Either Word32 [Layout.L] }
	deriving Show

allocateInfoToCore :: Pointable n => AllocateInfo n -> ContT r IO C.AllocateInfo
allocateInfoToCore AllocateInfo {
	allocateInfoNext = mnxt,
	allocateInfoDescriptorPool = Pool.P pl,
	allocateInfoDescriptorSetCountOrSetLayouts = either
		((, Nothing) . (fromIntegral &&& id))
		(((id &&& fromIntegral) `first`) . (length &&& Just)) ->
		((dsci, dscw), msls)
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	psls <- case msls of
		Nothing -> pure NullPtr
		Just sls -> do
			p <- ContT $ allocaArray dsci
			p <$ lift (pokeArray p $ (\(Layout.L l) -> l) <$> sls)
	pure C.AllocateInfo {
		C.allocateInfoSType = (),
		C.allocateInfoPNext = pnxt,
		C.allocateInfoDescriptorPool = pl,
		C.allocateInfoDescriptorSetCount = dscw,
		C.allocateInfoPSetLayouts = psls }
