{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuDepthStencilObject where

import Control.Monad
import Control.Monad.ST
import Data.Word

import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.GpuTextureFormat qualified as JS.GpuTextureFormat
import GHC.JS.Value.GpuStencilComparisonOperationObject qualified as
	JS.GpuStencilComparisonOperationObject

import GHC.JS.Value.GpuDepthStencilCompare qualified as
	JS.GpuDepthStencilCompare

data G = G {
	depthBias :: Maybe Int,
	depthBiasClamp :: Maybe Int,
	depthBiasSlopeScale :: Maybe Int,
	depthCompare :: Maybe JS.GpuDepthStencilCompare.G,
	depthWriteEnabled :: Maybe Bool,
	format :: JS.GpuTextureFormat.G,
	stencilBack :: Maybe JS.GpuStencilComparisonOperationObject.G,
	stencilFront :: Maybe JS.GpuStencilComparisonOperationObject.G,
	stencilReadMask :: Maybe Word32, stencilWriteMask :: Maybe Word32 }
	deriving Show

instance JS.Value.IsJSVal G where
	toJSVal g = JS.Value.toJSVal $ runST $ JS.Object.freeze =<< toObject g

instance JS.Value.V G where toV = JS.Object.toValue; fromV = JS.Object.fromValue

toObject :: (Monad m, JS.Object.M o m) => G -> m o
toObject g = do
	o <- JS.Object.new
	sb <- maybe (pure Nothing)
		((Just <$>) . (JS.Object.freeze <=< JS.GpuStencilComparisonOperationObject.toObject))
		$ stencilBack g
	sf <- maybe (pure Nothing)
		((Just <$>) . (JS.Object.freeze <=< JS.GpuStencilComparisonOperationObject.toObject))
		$ stencilFront g
	maybe (pure ()) (JS.Object.set o "depthBias") $ depthBias g
	maybe (pure ()) (JS.Object.set o "depthBiasClamp") $ depthBiasClamp g
	maybe (pure ()) (JS.Object.set o "depthBiasSlopeScale")
		$ depthBiasSlopeScale g
	maybe (pure ()) (JS.Object.set o "depthCompare") $ depthCompare g
	maybe (pure ()) (JS.Object.set o "depthWriteEnabled")
		$ depthWriteEnabled g
	JS.Object.set o "format" $ format g
	maybe (pure ()) (JS.Object.set o "stencilBack") sb
	maybe (pure ()) (JS.Object.set o "stencilFront") sf
	maybe (pure ()) (JS.Object.set o "stencilReadMask") $ stencilReadMask g
	maybe (pure ()) (JS.Object.set o "stencilWriteMask") $ stencilWriteMask g
	pure o
